//! Token types outputted by lexing.

use super::span::Span;

#[derive(Debug)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    LeftParen,    // (
    RightParen,   // )
    LeftBracket,  // [
    RightBracket, // ]
    LeftBrace,    // {
    RightBrace,   // }
    Colon,        // :
    Dot,          // .
    DotDot,       // ..
    Ellipses,     // ...
    Comma,        // ,
    Star,         // *
    Slash,        // /
    Percent,      // %
    Hash,         // #
    Plus,         // +
    Minus,        // -
    LtLt,         // <<
    GtGt,         // >>
    Pipe,         // |
    PipePipe,     // ||
    Caret,        // ^
    Amp,          // &
    AmpAmp,       // &&
    Bang,         // !
    Tilde,        // ~
    Question,     // ?
    Eq,           // =
    Lt,           // <
    Gt,           // >
    LtEq,         // <=
    GtEq,         // >=
    EqEq,         // ==
    BangEq,       // !=

    /// Identifier that starts with one underscore.
    Field,
    /// Identifier that starts with two underscores.
    StaticField,
    /// Identifier is called 'name' in the Wren compiler.
    Name,
    /// Number literal.
    Number,
    /// Identifier from the list of known reserved words.
    Keyword(KeywordKind),

    /// A string literal without any interpolation, or the last section of a
    /// string following the last interpolated expression.
    String,
    /// A portion of a string literal preceding an interpolated expression.
    Interpolated,

    /// Either a single line comment, or a whole block comment.
    Comment,

    Newline,
    Error,
    EOF,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum KeywordKind {
    Break,
    Continue,
    Class,
    Construct,
    Else,
    False,
    For,
    Foreign,
    If,
    Import,
    As,
    Is,
    Null,
    Return,
    Static,
    Super,
    This,
    True,
    Var,
    While,
}

impl Token {
    /// Slice a text fragment from the given source code.
    #[inline]
    pub fn fragment<'a>(&self, source: &'a str) -> &'a str {
        let start = self.span.pos as usize;
        let end = start + self.span.size as usize;
        &source[start..end]
    }

    /// Convenience util for converting the token to a tuple struct.
    pub fn to_tuple(self) -> (Span, TokenKind) {
        let Self { span, kind } = self;
        (span, kind)
    }
}
