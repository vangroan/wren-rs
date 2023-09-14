use super::cursor::Cursor;
use super::token::{Token, TokenKind};
use crate::compiler::span::Span;

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
    pub fn next_token(&mut self) -> Token {
        // Invariant: The lexer's cursor must be pointing to the
        //            start of the remainder of the source to be consumed.
        self.start_token();

        match self.cursor.current() {
            _ if self.cursor.at_end() => {
                // The source stream has run out, so we signal
                // the caller by emitting an end-of-file token that
                // doesn't exist in the text.
                //
                // The token's span thus points to the element
                // beyond the end of the collection, and has 0 length.
                self.start_pos = self.cursor.peek_offset();
                self.make_token(TokenKind::EOF)
            }
            '(' => self.make_token(TokenKind::LeftParen),
            ')' => self.make_token(TokenKind::RightParen),
            '[' => self.make_token(TokenKind::LeftBracket),
            ']' => self.make_token(TokenKind::RightBracket),
            '{' => self.make_token(TokenKind::LeftBrace),
            '}' => self.make_token(TokenKind::RightBrace),
            _ => self.make_token(TokenKind::Error),
        }
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
                Some(self.lexer.next_token())
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
