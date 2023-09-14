use super::cursor::Cursor;
use super::token::{Token, TokenKind};
use crate::compiler::span::Span;
use crate::error::{WrenError, WrenResult};

/// Lexical analyser (tokeniser) for the Wren language.
pub struct Lexer<'a> {
    cursor: Cursor<'a>,
    /// Keep reference to the source so the parser can
    /// slice fragments from it.
    source: &'a str,
    /// Starting absolute byte position of the current token
    /// in the source.
    start_pos: u32,
}

impl<'a> Lexer<'a> {
    /// Create a new Lexer from the given source code.
    pub fn from_source(source: &'a str) -> Self {
        let mut cursor = Cursor::from_str(source);

        // Initial state of the cursor is a non-existent EOF char,
        // but the initial state of the lexer should be a valid
        // token starting character.
        //
        // Prime the cursor for the first iteration.
        cursor.bump();

        // For what it's worth, the cursor gets to decide what the
        // initial byte position is.
        let start_pos = cursor.offset();

        Lexer {
            source,
            cursor,
            start_pos,
        }
    }

    /// The original source code that was passed into the lexer.
    pub fn source(&self) -> &'a str {
        self.source
    }

    /// Remainder of source to be consumed.
    pub fn rest(&self) -> &'a str {
        let start = self.cursor.offset() as usize;
        let end = self.cursor.original_length() as usize;
        &self.source[start..end]
    }

    /// Primes the lexer to consume the next token.
    fn start_token(&mut self) {
        self.start_pos = self.cursor.offset();
    }

    /// Indicates whether the lexer is at the end of the source.
    ///
    /// Note that source can contain '\0' (end-of-file) characters,
    /// but not be at the actual end. It's thus important to verify
    /// with this function whenever a [`TokenKind::EOF`] is encountered.
    pub fn at_end(&self) -> bool {
        self.cursor.at_end()
    }

    /// Build a token, using the source text from the position
    /// stored by [`start_token`](struct.Lexer.html#fn-start_token) to the
    /// current cursor position.
    ///
    /// Also prepare the cursor for the next iteration.
    fn make_token(&mut self, kind: TokenKind) -> Token {
        let start = self.start_pos;
        let end = self.cursor.peek_offset();

        // start and end can be equal, and a token can have 0 size.
        debug_assert!(end >= start);
        let size = end - start;

        // After this token is built, the lexer's internal state
        // is no longer dedicated to this iteration, but to preparing
        // for the next iteration.
        let token = Token {
            span: Span { pos: start, size },
            kind,
        };

        // Position the cursor to the starting character for the
        // next token, so the lexer's internal state is primed
        // for the next iteration.
        self.cursor.bump();

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

        while !self.cursor.at_end() {
            // Invariant: The lexer's cursor must be pointing to the
            //            start of the remainder of the source to be consumed.
            self.start_token();

            let token = match self.cursor.current() {
                '(' => self.make_token(TK::LeftParen),
                ')' => self.make_token(TK::RightParen),
                '[' => self.make_token(TK::LeftBracket),
                ']' => self.make_token(TK::RightBracket),
                '{' => self.make_token(TK::LeftBrace),
                '}' => self.make_token(TK::RightBrace),
                ':' => self.make_token(TK::Colon),
                ',' => self.make_token(TK::Comma),
                '*' => self.make_token(TK::Star),
                '%' => self.make_token(TK::Star),
                '#' => {
                    // Ignore shebang on the first line.
                    if self.cursor.line() == 1 && self.cursor.peek() == '!' && self.cursor.peek2() == '/' {
                        self.skip_line();
                        continue;
                    }
                    // Otherwise we treat it as a token
                    self.make_token(TK::Hash)
                }
                '^' => self.make_token(TK::Caret),
                '+' => self.make_token(TK::Plus),
                '-' => self.make_token(TK::Minus),
                '~' => self.make_token(TK::Tilde),
                '?' => self.make_token(TK::Question),

                // Two and three character tokens.
                c1 => match [c1, self.cursor.peek(), self.cursor.peek2()] {
                    ['|', '|', _] => self.make_token(TK::PipePipe),
                    ['|', _, _] => self.make_token(TK::Pipe),
                    ['.', '.', '.'] => self.make_token(TK::Ellipses),
                    ['.', '.', _] => self.make_token(TK::DotDot),
                    ['.', _, _] => self.make_token(TK::Dot),
                    ['/', '/', _] => self.consume_line_comment(),
                    ['/', '*', _] => {
                        self.skip_block_comment();
                        continue;
                    }

                    _ => self.make_token(TK::Error),
                },
            };

            return Ok(token);
        }

        // The source stream has run out, so we signal
        // the caller by emitting an end-of-file token that
        // doesn't exist in the text.
        //
        // The token's span thus points to the element
        // beyond the end of the collection, and has 0 length.
        self.start_pos = self.cursor.peek_offset();
        Ok(self.make_token(TK::EOF))
    }
}

impl<'a> Lexer<'a> {
    fn skip_whitespace(&mut self) {
        debug_assert!(Self::is_whitespace(self.cursor.current()));
    }

    // Skips the rest of the current line.
    fn skip_line(&mut self) {
        while self.cursor.peek() != '\n' && !self.cursor.at_end() {
            self.cursor.bump();
        }
    }

    fn skip_block_comment(&mut self) -> WrenResult<()> {
        debug_assert_eq!(self.cursor.current(), '/');
        debug_assert_eq!(self.cursor.peek(), '*');

        self.cursor.bump();
        self.cursor.bump();

        let mut nesting = 1;
        while nesting > 0 {
            match [self.cursor.current(), self.cursor.peek()] {
                ['\0', _] => return Err(WrenError::new_compile("Unterminated block comment")),
                ['/', '*'] => {
                    self.cursor.bump();
                    self.cursor.bump();
                    nesting += 1;
                }
                ['*', '/'] => {
                    self.cursor.bump();
                    self.cursor.bump();
                    nesting += 1;
                }
                _ => {
                    self.cursor.bump();
                }
            }
        }

        Ok(())
    }

    fn consume_line_comment(&mut self) -> Token {
        debug_assert_eq!(self.cursor.current(), '/');
        debug_assert_eq!(self.cursor.peek(), '/');

        while self.cursor.peek() != '\n' && !self.cursor.at_end() {
            self.cursor.bump();
        }

        self.make_token(TokenKind::Comment)
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

impl<'a> IntoIterator for Lexer<'a> {
    type Item = Token;
    type IntoIter = LexerIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        LexerIter {
            lexer: self,
            done: false,
        }
    }
}

/// Convenience iterator that wraps the lexer.
pub struct LexerIter<'a> {
    // Track end so an EOF token is emitted once.
    done: bool,
    lexer: Lexer<'a>,
}

impl<'a> Iterator for LexerIter<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.lexer.at_end() {
            if self.done {
                None
            } else {
                self.done = true;
                self.lexer.next_token()
            }
        } else {
            Some(self.lexer.next_token())
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::compiler::token::TokenKind as TK;

    #[test]
    fn test_basic_paren() {
        let mut lexer = Lexer::from_source("({[]})");

        assert_eq!(lexer.next_token().to_tuple(), (Span::new(0, 1), TK::LeftParen));
        assert_eq!(lexer.next_token().to_tuple(), (Span::new(1, 1), TK::LeftBrace));
        assert_eq!(lexer.next_token().to_tuple(), (Span::new(2, 1), TK::LeftBracket));
        assert_eq!(lexer.next_token().to_tuple(), (Span::new(3, 1), TK::RightBracket));
        assert_eq!(lexer.next_token().to_tuple(), (Span::new(4, 1), TK::RightBrace));
        assert_eq!(lexer.next_token().to_tuple(), (Span::new(5, 1), TK::RightParen));
        assert_eq!(lexer.next_token().to_tuple(), (Span::new(6, 0), TK::EOF));
    }
}
