//! Token types outputted by lexing.

use super::span::Span;
use crate::error::ParseError;

#[derive(Debug)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
    pub value: LiteralValue,
}

#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    None,
    Number(f64),
    String(String),
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
    BlockComment,
    /// This implementation has special support for document comments,
    /// which is not in the reference Wren implementation.
    DocComment,

    Newline,
    Error,
    End,
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
        let start = self.span.pos;
        let end = start + self.span.size;
        &source[start..end]
    }

    /// Convenience util for converting the token to a tuple struct.
    pub fn to_tuple(self) -> (Span, TokenKind) {
        let Self { span, kind, .. } = self;
        (span, kind)
    }
}

pub(crate) trait TokenExt {
    fn kind(&self) -> Option<TokenKind>;
    fn keyword(&self) -> Option<KeywordKind>;
}

impl TokenExt for Option<Token> {
    #[inline(always)]
    fn kind(&self) -> Option<TokenKind> {
        self.as_ref().map(|token| token.kind)
    }

    #[inline(always)]
    fn keyword(&self) -> Option<KeywordKind> {
        match self.kind() {
            Some(TokenKind::Keyword(kw)) => Some(kw),
            Some(_) | None => None,
        }
    }
}

impl LiteralValue {
    pub fn num(&self) -> Option<f64> {
        match self {
            Self::Number(num) => Some(*num),
            _ => None,
        }
    }

    pub fn as_u32(&self) -> Option<u32> {
        self.num().map(|num| num as u32)
    }

    pub fn str(&self) -> Option<&str> {
        match self {
            Self::String(string) => Some(string.as_str()),
            _ => None,
        }
    }
}

impl TryFrom<&str> for KeywordKind {
    type Error = ParseError;

    #[rustfmt::skip]
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "break"     => Ok(Self::Break),
            "continue"  => Ok(Self::Continue),
            "class"     => Ok(Self::Class),
            "construct" => Ok(Self::Construct),
            "else"      => Ok(Self::Else),
            "false"     => Ok(Self::False),
            "for"       => Ok(Self::For),
            "foreign"   => Ok(Self::Foreign),
            "if"        => Ok(Self::If),
            "import"    => Ok(Self::Import),
            "as"        => Ok(Self::As),
            "is"        => Ok(Self::Is),
            "null"      => Ok(Self::Null),
            "return"    => Ok(Self::Return),
            "static"    => Ok(Self::Static),
            "super"     => Ok(Self::Super),
            "this"      => Ok(Self::This),
            "true"      => Ok(Self::True),
            "var"       => Ok(Self::Var),
            "while"     => Ok(Self::While),
            _ => Err(ParseError::InvalidKeyword),
        }
    }
}
