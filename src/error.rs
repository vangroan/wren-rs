//! Error type.
use std::fmt::{self, Display, Formatter};
use std::num::{ParseFloatError, ParseIntError};

use crate::limits::MAX_INTERPOLATION_NESTING;

pub type WrenResult<T> = Result<T, WrenError>;
pub type ParseResult<T> = Result<T, ParseError>;

pub fn parse_error(parse_error: ParseError) -> WrenError {
    WrenError {
        kind: ErrorKind::Parse(parse_error),
    }
}

#[derive(Debug)]
pub struct WrenError {
    pub kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
    Parse(ParseError),
    Compile,
    Runtime,
}

#[derive(Debug)]
pub enum ParseError {
    /// Incomplete escape sequence.
    IncompleteEscape,
    /// Invalid source code byte.
    InvalidByte(char),
    /// Invalid source code character.
    InvalidCharacter(char),
    /// Invalid string escape character.
    InvalidEscapeChar(char),
    /// Invalid Unicode character
    InvalidUnicode(u32),
    /// Invalid keyword identifier.
    InvalidKeyword,
    /// Maximum string interpolation level.
    MaxInterpolationLevel,
    /// Error parsing an integer number
    ParseInt(ParseIntError),
    /// Error parsing a floating point number
    ParseFloat(ParseFloatError),
    /// Unterminated block comment.
    UnterminatedBlockComment,
    /// Unterminated scientific notation.
    UnterminatedScientificNotation,
    /// Unterminated string.
    UnterminatedString,
    /// Unexpected string interpolation character.
    UnexpectedStringInterpolation,
    /// Unexpected character.
    UnexpectedChar,
    /// Unexpected end-of-file.
    UnexpectedEOF,
}

impl Display for WrenError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use ErrorKind as EK;
        use ParseError as PE;

        let prefix = match self.kind {
            EK::Parse(_) => "parse",
            EK::Compile => "compile",
            EK::Runtime => "runtime",
        };

        write!(f, "{prefix} error: ")?;

        match &self.kind {
            EK::Parse(err) => match err {
                PE::IncompleteEscape => write!(f, "incomplete escape sequence"),
                PE::InvalidByte(ch) => write!(f, "invalid byte {}", ch.escape_default()),
                PE::InvalidCharacter(ch) => write!(f, "invalid character '{ch}'"),
                PE::InvalidEscapeChar(ch) => write!(f, "invalid escape character '{}'", ch.escape_default()),
                PE::InvalidUnicode(ord) => write!(f, "invalid Unicode bytes 0x{:04x}", ord),
                PE::InvalidKeyword => write!(f, "invalid keyword"),
                PE::MaxInterpolationLevel => write!(
                    f,
                    "string interpolation may only nest {MAX_INTERPOLATION_NESTING} levels deep"
                ),
                PE::ParseInt(err) => err.fmt(f),
                PE::ParseFloat(err) => err.fmt(f),
                PE::UnterminatedBlockComment => write!(f, "unterminated block comment"),
                PE::UnterminatedScientificNotation => write!(f, "unterminated scientific notation"),
                PE::UnterminatedString => write!(f, "unterminated string"),
                PE::UnexpectedStringInterpolation => write!(f, "expected '(' after '%'"),
                PE::UnexpectedChar => write!(f, "unexpected character"),
                PE::UnexpectedEOF => write!(f, "unexpected end-of-file"),
            },
            EK::Compile => write!(f, "todo"),
            EK::Runtime => write!(f, "todo"),
        }
    }
}
