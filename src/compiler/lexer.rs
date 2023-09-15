use super::cursor::Cursor;
use super::token::{Token, TokenKind};
use crate::compiler::span::Span;
use crate::error::{parse_error, ErrorKind, ParseError, WrenError, WrenResult};
use std::str::CharIndices;
use std::string;

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
    /// Remaining source code text.
    /// The start of the string slice is the next character to be lexed.
    /// It represents the cursor of tokenising loop, with the 0 index
    /// constantly moving back. It shouldn't be used to determine a
    /// token's span, or extract a token text using a span. Spans
    /// are relative to the original source code.
    rest: &'src str,
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

        Lexer {
            rest: source,
            cursor,
            start_pos,
        }
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
    pub fn at_end(&self) -> bool {
        self.rest.is_empty()
    }

    /// Build a token, using the source text from the position
    /// stored by [`start_token`](struct.Lexer.html#fn-start_token) to the
    /// current cursor position.
    fn make_token(&mut self, kind: TokenKind) -> Token {
        let start = self.start_pos;

        // The lexer is expected to have consumed character including
        // the final character that must be part of this token.
        //
        // The ending of the span must thus be one character after.
        //
        // We peek the next position to measure the final character's byte width.
        let end = self.cursor.peek_position();

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

        token
    }

    /// Helper function for creating a two character token, by peeking
    /// at the next character and comparing it to the given `check`.
    ///
    /// The signature of this function is meant to resemble the similar function
    /// in the original C codebase of Wren.
    fn two_char_token(&mut self, check: char, two: TokenKind, one: TokenKind) -> Token {
        if self.cursor.peek() == check {
            self.cursor.bump();
            self.make_token(two)
        } else {
            self.make_token(one)
        }
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
            // Invariant: The lexer's cursor must be pointing to the
            //            start of the remainder of the source to be consumed.
            self.start_token();

            #[rustfmt::skip]
            let result = match ch {
                // TODO: Ignore BOM
                '(' => Ok(self.make_token(TK::LeftParen)),
                ')' => Ok(self.make_token(TK::RightParen)),
                '[' => Ok(self.make_token(TK::LeftBracket)),
                ']' => Ok(self.make_token(TK::RightBracket)),
                '{' => Ok(self.make_token(TK::LeftBrace)),
                '}' => Ok(self.make_token(TK::RightBrace)),
                ':' => Ok(self.make_token(TK::Colon)),
                ',' => Ok(self.make_token(TK::Comma)),
                '*' => Ok(self.make_token(TK::Star)),
                '%' => Ok(self.make_token(TK::Star)),
                '#' => {
                    // Ignore shebang on the first line.
                    if self.skip_shebang() {
                        continue;
                    }
                    // Otherwise we treat it as a token
                    Ok(self.make_token(TK::Hash))
                }
                '^' => Ok(self.make_token(TK::Caret)),
                '+' => Ok(self.make_token(TK::Plus)),
                '-' => Ok(self.make_token(TK::Minus)),
                '~' => Ok(self.make_token(TK::Tilde)),
                '?' => Ok(self.make_token(TK::Question)),

                '|' => Ok(self.two_char_token('|', TK::PipePipe, TK::Pipe)),
                '&' => Ok(self.two_char_token('&', TK::AmpAmp, TK::Amp)),
                '=' => Ok(self.two_char_token('=', TK::EqEq, TK::Eq)),
                '!' => Ok(self.two_char_token('=', TK::BangEq, TK::Bang)),

                '.' => {
                    if self.cursor.peek() == '.' {
                        Ok(self.two_char_token('.', TK::Ellipses, TK::DotDot))
                    } else {
                        Ok(self.make_token(TK::Dot))
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
                        },
                        '*' => self.consume_block_comment(),
                        _ => Ok(self.make_token(TK::Slash)),
                    }
                }

                '\n' => Ok(self.make_token(TK::Newline)),

                ch if ch.is_whitespace() => {
                    self.cursor.bump();
                    continue;
                }

                _ => Err(ParseError::UnexpectedChar),
            };

            let token = match result {
                Ok(token) => token,
                Err(parse_error) => {
                    panic!("parse error at {idx}, '{ch}': {parse_error:?}");
                }
            };

            // Position the cursor at the starting character for the
            // next token, so the lexer's internal state is primed
            // for the next iteration.
            self.cursor.bump();

            return Ok(token);
        }

        // The source stream has run out, so we signal
        // the caller by emitting an end-of-file token that
        // doesn't exist in the text.
        //
        // The token's span thus points to the element
        // beyond the end of the collection, and has 0 length.
        // self.start_pos = self.cursor.peek_offset();
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
    /// Skips over a shebang line at the start of the input.
    ///
    /// This is used on Unix systems where the first line
    /// of a script uses the shebang as a directive pointing
    /// to the interpreter.
    ///
    /// Returns `true` if the shebang is detected, otherwise `false`.
    fn skip_shebang(&mut self) -> bool {
        // Ignore shebang on the first line.
        // if self.at_start() && self.rest.starts_with("#!") {
        //     let pos = self.measure_line();
        //     self.bump_offset(pos);
        //     true
        // } else {
        //     false
        // }
        todo!()
    }

    fn consume_line(&mut self) {
        while !self.cursor.at_end() {
            if self.cursor.char() == '\n' {
                // This function might be called with an empty line,
                // so we must first check the current character for newline.
                break;
            }
            self.cursor.bump();
        }
    }

    fn consume_block_comment(&mut self) -> Result<Token, ParseError> {
        todo!()
    }

    // fn skip_block_comment(&mut self) {
    //     debug_assert_eq!(self.cursor.current(), '/');
    //     debug_assert_eq!(self.cursor.peek(), '*');
    //
    //     self.cursor.bump();
    //     self.cursor.bump();
    //
    //     let mut nesting = 1;
    //     while nesting > 0 {
    //         match [self.cursor.current(), self.cursor.peek()] {
    //             ['\0', _] => {
    //                 self.lex_error("Unterminated block comment.");
    //                 return;
    //             }
    //             ['/', '*'] => {
    //                 self.cursor.bump();
    //                 self.cursor.bump();
    //                 nesting += 1;
    //             }
    //             ['*', '/'] => {
    //                 self.cursor.bump();
    //                 self.cursor.bump();
    //                 nesting += 1;
    //             }
    //             _ => {
    //                 self.cursor.bump();
    //             }
    //         }
    //     }
    // }

    /// Consume a line comment starting with '//'.
    fn consume_line_comment(&mut self) -> Result<Token, ParseError> {
        println!("star_pos: {}; rest: {}", self.start_pos, self.cursor.rest());
        debug_assert!(self.cursor.rest().starts_with("//"));
        self.consume_line();
        Ok(self.make_token(TokenKind::Comment))
    }

    fn consume_doc_comment(&mut self) -> Result<Token, ParseError> {
        debug_assert!(self.cursor.rest().starts_with("///"));
        self.consume_line();
        Ok(self.make_token(TokenKind::DocComment))
    }
}

impl<'a> Lexer<'a> {
    /// Test whether the character is considered whitespace
    /// that should be ignored by the parser later.
    ///
    /// Doesn't include newline characters, because in Wren
    /// newlines are significant, specifying end-of-statement.
    fn is_whitespace(c: char) -> bool {
        matches!(
            c,
            '\u{0020}' // space
            | '\u{0009}' // tab
            | '\u{00A0}' // no-break space
            | '\u{FEFF}' // zero width no-break space
        )
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
}
