//! Token stream.
use std::slice::SliceIndex;

use itertools::{multipeek, MultiPeek};

use super::lexer::{Lexer, LexerIter};
use super::token::{Token, TokenKind};

/// Buffered stream of tokens that allows arbitrary look ahead.
///
/// Tokens are lazily lexed. Peeking or consuming the next token
/// triggers the internal lexer.
///
/// The peek semantics are determined by the internal `MultiPeek`.
/// Calling `TokenStream::peek` is not idempotent, advancing a peek
/// cursor forward by one token for each `peek()` call. The cursor
/// can be reset explicitly using `TokenStream::reset_peek` or
/// implicitly by calling one of the consuming methods.
pub struct TokenStream<'a> {
    lexer: MultiPeek<LexerIter<'a>>,
    /// Keep reference to the source so the parser can
    /// slice fragments from it.
    source: &'a str,
}

impl<'a> TokenStream<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            source: lexer.source(),
            lexer: multipeek(lexer),
        }
    }

    /// Slice a fragment of source code.
    ///
    /// Returns `None` if the given index is out
    /// of bounds.
    #[inline]
    pub fn fragment<I>(&self, index: I) -> Option<&str>
    where
        I: SliceIndex<str, Output = str>,
    {
        self.source.get(index)
    }

    #[inline]
    pub fn token_fragment(&self, token: &Token) -> &str {
        token.fragment(self.source)
    }

    /// Consumes the current token regardless of type.
    ///
    /// Returns `None` when the cursor is at the end of the token stream.
    pub fn next_token(&mut self) -> Option<Token> {
        self.lexer.next()
    }

    /// Consumes the current token if it matches the given token kind.
    ///
    /// Returns true when matched. Returns false when token kinds
    /// do not match, or the token stream is at the end.
    ///
    /// Does not consume the token if the types do not match.
    pub fn match_token(&mut self, token_kind: TokenKind) -> bool {
        // Ensure clean peek state.
        self.lexer.reset_peek();

        match self.lexer.peek() {
            Some(token) => {
                let is_match = token.kind == token_kind;
                if is_match {
                    self.lexer.next(); // discard
                }
                // peek is reset by next()
                is_match
            }
            None => {
                self.lexer.reset_peek();
                false
            }
        }
    }

    /// Return the current token and advance the cursor.
    ///
    /// The consumed token must match the given token type, otherwise
    /// a parsing error is returned.
    ///
    /// # Errors
    ///
    /// Returns a [`TokenError`] if the token kind doesn't match.
    ///
    /// # Panics
    ///
    /// Panics when at end-of-file.
    pub fn consume(&mut self, token_kind: TokenKind) -> Result<Token, TokenError> {
        // Ensure clean peek state.
        self.lexer.reset_peek();

        // We should not consume the token if the types don't match.
        match self.lexer.peek() {
            Some(token) => {
                if token.kind != token_kind {
                    // TODO: Return parsing error.
                    Err(TokenError {
                        expected: token_kind,
                        encountered: token.kind,
                    })
                } else {
                    self.lexer.next().ok_or_else(|| {
                        // TODO: Change from panic to error
                        panic!("unexpected end-of-file");
                    })
                }
            }
            None => {
                // TODO: Change from panic to error
                panic!("unexpected end-of-file");
            }
        }
    }

    /// Consumes one or more tokens while the token's matches given kind.
    pub fn ignore_many(&mut self, kind: TokenKind) {
        self.lexer.reset_peek();
        if let Some(token) = self.lexer.peek() {
            if token.kind == kind {
                self.lexer.next();
            } else {
                self.lexer.reset_peek();
            }
        }
    }

    /// Consumes one or more tokens while the given predicate tests as `true`.
    pub fn ignore_while(&mut self, predicate: impl Fn(TokenKind) -> bool) {
        self.lexer.reset_peek();
        while let Some(token) = self.lexer.peek() {
            if predicate(token.kind) {
                self.lexer.next();
            } else {
                self.lexer.reset_peek();
                return;
            }
        }
    }

    /// Return the current token without advancing the cursor.
    ///
    /// Subsequent calls to peek will look further ahead.
    /// Call [`reset_peek`] for the lookahead to return to the immediate
    /// next token.
    ///
    /// Returns `None` when lexing is done.
    #[inline]
    pub fn peek(&mut self) -> Option<&Token> {
        self.lexer.peek()
    }

    /// Return the current token kind without advancing the cursor.
    #[inline]
    pub fn peek_kind(&mut self) -> Option<TokenKind> {
        self.lexer.peek().map(|token| token.kind)
    }

    #[inline]
    pub fn reset_peek(&mut self) {
        self.lexer.reset_peek()
    }
}

/// Error returned when an unexpected token type is encountered.
#[derive(Debug)]
pub struct TokenError {
    pub expected: TokenKind,
    pub encountered: TokenKind,
}

impl std::fmt::Display for TokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let TokenError { expected, encountered } = self;
        write!(f, "expected token '{expected:?}', but encountered '{encountered:?}'")
    }
}
