//! Error type.
use std::fmt::{self, Display, Formatter};

use crate::compiler::Span;

pub type WrenResult<T> = Result<T, WrenError>;

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

        let message = match &self.kind {
            EK::Parse(err) => match err {
                PE::UnterminatedBlockComment => "unterminated block comment",
                PE::UnexpectedChar => "unexpected character",
                PE::UnexpectedEOF => "unexpected end-of-file",
            },
            EK::Compile => "todo",
            EK::Runtime => "todo",
        };

        write!(f, "{prefix} error: {message}")
    }
}
