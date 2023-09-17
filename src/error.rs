//! Error type.
use std::fmt::{self, Display, Formatter};

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
    /// Invalid source code byte.
    InvalidByte(char),
    /// Invalid source code character.
    InvalidCharacter(char),
    /// Invalid keyword identifier.
    InvalidKeyword,
    /// Unterminated block comment.
    UnterminatedBlockComment,
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
                PE::InvalidByte(ch) => write!(f, "invalid byte {}", ch.escape_default()),
                PE::InvalidCharacter(ch) => write!(f, "invalid character '{ch}'"),
                PE::InvalidKeyword => write!(f, "invalid keyword"),
                PE::UnterminatedBlockComment => write!(f, "unterminated block comment"),
                PE::UnexpectedChar => write!(f, "unexpected character"),
                PE::UnexpectedEOF => write!(f, "unexpected end-of-file"),
            },
            EK::Compile => write!(f, "todo"),
            EK::Runtime => write!(f, "todo"),
        }
    }
}
