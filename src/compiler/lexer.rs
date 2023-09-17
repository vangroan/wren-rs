use super::cursor::Cursor;
use super::token::{Token, TokenKind};
use crate::compiler::span::Span;
use crate::error::{ParseError, ParseResult, WrenResult};

// Errors:
//
// - Unterminated block comment
// - Number literal was too large
// - Unterminated scientific notation
// - Incomplete %s escape sequence (readHexEscape in string)
// - Invalid %s escape sequence (readHexEscape in string)
// - Unterminated raw string
// - Unterminated string
// - Expect '(' after '%%'.
// - Interpolation may only nest 8 levels deep
// - Invalid escape character
// - Invalid character '%c'
// - Invalid byte 0x%x.

/// Lexical analyser (tokeniser) for the Wren language.
pub struct Lexer<'src> {
    /// A cursor tracking the progress of scanning over the source text.
    /// Allows for character look ahead, and keeps track of the UTF-8
    /// character byte positions.
    cursor: Cursor<'src>,
    /// Starting absolute byte position of the current token
    /// in the source.
    start_pos: usize,
}

impl<'src> Lexer<'src> {
    /// Create a new Lexer from the given source code.
    pub fn from_source(source: &'src str) -> Self {
        let mut cursor = Cursor::from_str(source);

        // Initial state of the cursor is a non-existent EOF char,
        // but the initial state of the lexer should be a valid
        // token starting character.
        //
        // Prime the cursor for the first iteration.
        cursor.bump();

        // For what it's worth, the cursor gets to decide what the
        // initial byte position is. It's probably 0.
        let start_pos = cursor.position();

        Lexer { cursor, start_pos }
    }

    /// The original source code that was passed into the lexer.
    #[inline(always)]
    pub fn source(&self) -> &'src str {
        self.cursor.source()
    }

    /// Primes the lexer to consume the next token.
    fn start_token(&mut self) {
        self.start_pos = self.cursor.position();
    }

    /// Indicates whether the lexer is at the end of the source.
    ///
    /// Note that source can contain '\0' (end-of-file) characters,
    /// but not be at the actual end. It's thus important to verify
    /// with this function whenever a [`TokenKind::End`] is encountered.
    // pub fn at_end(&self) -> bool {
    //     self.rest.is_empty()
    // }

    /// Build a token, using the source text from the position
    /// stored by [`start_token`](struct.Lexer.html#fn-start_token) to the
    /// current cursor position.
    fn make_token(&mut self, kind: TokenKind) -> Token {
        let start = self.start_pos;

        // The lexer is expected to have consumed its characters including
        // the final character that must be part of this token.
        //
        // The ending of the span is thus one character after the last.
        let end = self.cursor.position();

        // start and end can be equal, and a token can have 0 size.
        debug_assert!(start <= end);
        let size = end - start;

        // After this token is built, the lexer's internal state
        // is no longer dedicated to this iteration, but to preparing
        // for the next iteration.
        let token = Token {
            span: Span { pos: start, size },
            kind,
        };

        if cfg!(feature = "trace_lexer") {
            log::trace!("{start:>4}:{end:<6}{kind:?} ({size})")
        }

        token
    }

    /// Scan the source characters and construct the next token.
    ///
    /// ## Implementation
    ///
    /// The internal iteration of the lexer follows this convention:
    ///
    /// Each iteration (`next_token` call) starts with the assumption that
    /// the internal cursor is pointing to the start of the remaining source
    /// to be consumed.
    ///
    /// Initially, the lexer must be constructed with a cursor pointing to
    /// the start of the source.
    ///
    /// When an iteration is done building a token, it must leave the cursor
    /// at the start of the next token's text. It may not finish leaving the
    /// cursor pointing into its own token.
    pub fn next_token(&mut self) -> WrenResult<Token> {
        use TokenKind as TK;

        while let Some((idx, ch)) = self.cursor.current() {
            if cfg!(feature = "trace_lexer") {
                log::trace!("char: ({idx}, {ch})");
            }

            // Invariant: The lexer's cursor must be pointing to the
            //            start of the remainder of the source to be consumed.
            self.start_token();

            #[rustfmt::skip]
                let result = match ch {
                // TODO: Ignore BOM
                '(' => self.one_char_token(TK::LeftParen),
                ')' => self.one_char_token(TK::RightParen),
                '[' => self.one_char_token(TK::LeftBracket),
                ']' => self.one_char_token(TK::RightBracket),
                '{' => self.one_char_token(TK::LeftBrace),
                '}' => self.one_char_token(TK::RightBrace),
                ':' => self.one_char_token(TK::Colon),
                ',' => self.one_char_token(TK::Comma),
                '*' => self.one_char_token(TK::Star),
                '%' => self.one_char_token(TK::Star),
                '#' => {
                    // Ignore shebang on the first line.
                    if self.skip_shebang() {
                        continue;
                    }
                    // Otherwise we treat it as a token
                    self.one_char_token(TK::Hash)
                }
                '^' => self.one_char_token(TK::Caret),
                '+' => self.one_char_token(TK::Plus),
                '-' => self.one_char_token(TK::Minus),
                '~' => self.one_char_token(TK::Tilde),
                '?' => self.one_char_token(TK::Question),

                '|' => self.two_char_token('|', TK::PipePipe, TK::Pipe),
                '&' => self.two_char_token('&', TK::AmpAmp, TK::Amp),
                '=' => self.two_char_token('=', TK::EqEq, TK::Eq),
                '!' => self.two_char_token('=', TK::BangEq, TK::Bang),

                '.' => {
                    if self.cursor.peek() == '.' {
                        self.two_char_token('.', TK::Ellipses, TK::DotDot)
                    } else {
                        self.one_char_token(TK::Dot)
                    }
                }

                '/' => {
                    match self.cursor.peek() {
                        '/' => {
                            if self.cursor.peek2() == '/' {
                                self.consume_doc_comment()
                            } else {
                                self.consume_line_comment()
                            }
                        }
                        '*' => self.consume_block_comment(),
                        _ => self.one_char_token(TK::Slash),
                    }
                }

                '\n' => self.one_char_token(TK::Newline),

                // Whitespace must be tested after newline, which
                // is the end-of-statement character.
                ch if ch.is_whitespace() => {
                    self.cursor.bump();
                    continue;
                }

                '"' => {
                    if self.cursor.rest().starts_with(r#"""""#) {
                        self.consume_raw_string()
                    } else {
                        self.consume_string()
                    }
                }

                '_' => {
                    self.consume_name(
                        if self.cursor.peek() == '_' {
                            TK::StaticField
                        } else {
                            TK::Field
                        }
                    )
                }

                '0' => {
                    if self.cursor.peek() == '0' {
                        self.consume_hex_number()
                    } else {
                        self.consume_number()
                    }
                }

                ch if ch.is_ascii_alphabetic() => {
                    self.consume_name(TK::Name)
                }

                ch if ch.is_ascii_digit() => {
                    self.consume_number()
                }

                _ => {
                    if matches!(ch, '\x20'..='\x7E') {
                        Err(ParseError::InvalidCharacter(ch))
                    } else {
                        // NOTE: The reference Wren implementation processes raw bytes, but
                        // we use Rust's UTF-8 encoded string.
                        Err(ParseError::InvalidByte(ch))
                    }
                }
            };

            let token = match result {
                Ok(token) => token,
                Err(parse_error) => {
                    panic!("parse error at {idx}, '{ch}': {parse_error:?}");
                }
            };

            return Ok(token);
        }

        // The source stream has run out, so we signal
        // the caller by emitting an end-of-file token that
        // doesn't exist in the text.
        //
        // The token's span thus points to the element
        // beyond the end of the collection, and has 0 length.
        Ok(Token {
            span: Span::empty(self.cursor.position()),
            kind: TokenKind::End,
        })
    }

    /// A convenience function to get the next token as a tuple.
    ///
    /// # Panic
    ///
    /// Panics on any parsing error.
    pub fn next_token_tuple(&mut self) -> (Span, TokenKind) {
        match self.next_token() {
            Ok(token) => (token.span, token.kind),
            Err(err) => panic!("lexer error: {err}"),
        }
    }
}

impl<'a> Lexer<'a> {
    /// Consume one character to create a token.
    fn one_char_token(&mut self, kind: TokenKind) -> ParseResult<Token> {
        self.cursor.bump();
        Ok(self.make_token(kind))
    }

    /// Helper function for creating a two character token, by peeking
    /// at the next character and comparing it to the given `check`.
    ///
    /// The signature of this function is meant to resemble the similar function
    /// in the original C codebase of Wren.
    fn two_char_token(&mut self, check: char, two: TokenKind, one: TokenKind) -> ParseResult<Token> {
        if self.cursor.peek() == check {
            self.cursor.bump();
            self.cursor.bump();
            Ok(self.make_token(two))
        } else {
            self.cursor.bump();
            Ok(self.make_token(one))
        }
    }

    /// Skips over a shebang line at the start of the input.
    ///
    /// This is used on Unix systems where the first line
    /// of a script uses the shebang as a directive pointing
    /// to the interpreter.
    ///
    /// Returns `true` if the shebang is detected, otherwise `false`.
    fn skip_shebang(&mut self) -> bool {
        // Ignore shebang on the first line.
        if self.cursor.at_start() && self.cursor.rest().starts_with("#!") {
            self.consume_line();
            self.start_token();
            true
        } else {
            false
        }
    }

    /// Consume the rest of the line, including the trailing `\r` and `\n`.
    fn consume_line(&mut self) {
        while !self.cursor.at_end() {
            // This function might be called with an empty line,
            // so we must first check the current character for newline.
            if self.cursor.match_bump('\n') {
                break;
            }
            self.cursor.bump();
        }
    }

    fn consume_block_comment(&mut self) -> ParseResult<Token> {
        debug_assert!(self.cursor.rest().starts_with("/*"));

        self.cursor.bump();
        self.cursor.bump();

        let mut nesting = 1;
        while nesting > 0 {
            match (self.cursor.char(), self.cursor.peek()) {
                ('/', '*') => {
                    self.cursor.bump();
                    self.cursor.bump();
                    nesting += 1;
                }
                ('*', '/') => {
                    self.cursor.bump();
                    self.cursor.bump();
                    nesting -= 1;
                }
                ('\0', _) => {
                    return Err(ParseError::UnterminatedBlockComment);
                }
                _ => {
                    self.cursor.bump();
                }
            }
        }

        Ok(self.make_token(TokenKind::BlockComment))
    }

    /// Consume a line comment starting with '//'.
    fn consume_line_comment(&mut self) -> ParseResult<Token> {
        debug_assert!(self.cursor.rest().starts_with("//"));
        self.consume_line();
        Ok(self.make_token(TokenKind::Comment))
    }

    fn consume_doc_comment(&mut self) -> Result<Token, ParseError> {
        debug_assert!(self.cursor.rest().starts_with("///"));
        self.consume_line();
        Ok(self.make_token(TokenKind::DocComment))
    }

    fn consume_string(&mut self) -> ParseResult<Token> {
        todo!()
    }

    fn consume_raw_string(&mut self) -> ParseResult<Token> {
        todo!()
    }

    fn consume_name(&mut self, kind: TokenKind) -> ParseResult<Token> {
        todo!()
    }

    fn consume_hex_number(&mut self) -> ParseResult<Token> {
        todo!()
    }

    fn consume_number(&mut self) -> ParseResult<Token> {
        todo!()
    }
}

//
// impl<'a> IntoIterator for Lexer<'a> {
//     type Item = WrenResult<Token>;
//     type IntoIter = LexerIter<'a>;
//
//     fn into_iter(self) -> Self::IntoIter {
//         LexerIter {
//             lexer: self,
//             done: false,
//         }
//     }
// }

/// Convenience iterator that wraps the lexer.
// pub struct LexerIter<'a> {
//     // Track end so an EOF token is emitted once.
//     done: bool,
//     lexer: Lexer<'a>,
// }
//
// impl<'a> Iterator for LexerIter<'a> {
//     type Item = WrenResult<Token>;
//
//     fn next(&mut self) -> Option<Self::Item> {
//         if self.lexer.at_end() {
//             if self.done {
//                 None
//             } else {
//                 self.done = true;
//                 self.lexer.next_token()
//             }
//         } else {
//             self.lexer.next_token()
//         }
//     }
// }
#[cfg(test)]
mod test {
    use super::*;
    use crate::compiler::token::TokenKind as TK;

    #[test]
    #[rustfmt::skip]
    fn test_basic_paren() {
        let mut lexer = Lexer::from_source("({[]})");

        assert_eq!(lexer.next_token_tuple(), (Span::new(0, 1), TK::LeftParen));
        assert_eq!(lexer.next_token_tuple(), (Span::new(1, 1), TK::LeftBrace));
        assert_eq!(lexer.next_token_tuple(), (Span::new(2, 1), TK::LeftBracket));
        assert_eq!(lexer.next_token_tuple(), (Span::new(3, 1), TK::RightBracket));
        assert_eq!(lexer.next_token_tuple(), (Span::new(4, 1), TK::RightBrace));
        assert_eq!(lexer.next_token_tuple(), (Span::new(5, 1), TK::RightParen));
        assert_eq!(lexer.next_token_tuple(), (Span::new(6, 0), TK::End));
    }

    #[test]
    fn test_consume_line() {
        let mut lexer = Lexer::from_source("   \n   \n");

        assert_eq!(lexer.next_token_tuple(), (Span::new(3, 1), TK::Newline));
        assert_eq!(lexer.next_token_tuple(), (Span::new(7, 1), TK::Newline));
    }
}
