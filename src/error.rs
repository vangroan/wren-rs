//! Error type.
use std::fmt::{self, Display, Formatter};
use std::num::{ParseFloatError, ParseIntError};

use crate::limits::*;

pub type WrenResult<T> = Result<T, WrenError>;
pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub struct WrenError {
    pub kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
    Parse(ParseError),
    Compile(CompileError),
    Runtime(RuntimeError),
}

#[derive(Debug, Eq, PartialEq)]
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

#[derive(Debug, Eq, PartialEq)]
pub enum CompileError {
    MaxSymbols,
    MaxModuleVariables,
    /// Scope variable already defined.
    ScopeVariableExists(String),
    /// Module variable already defined.
    ModuleVariableExists(String),
    /// Forward variable declaration, meaning it was used before it was declared.
    /// Holds the line number where the usage was recorded.
    ForwardVariable(String, usize),
    SymbolExists,
    SymbolNotFound,
    UnexpectedEndOfTokens,
    InvalidOperator,
}

#[derive(Debug, Eq, PartialEq)]
pub enum RuntimeError {
    StackOverflow,
    StackUnderflow,
    InvalidSlot(u8),
    InvalidType,
    MethodNotFound,
}

impl WrenError {
    pub(crate) fn new_compile(kind: CompileError) -> Self {
        Self {
            kind: ErrorKind::Compile(kind),
        }
    }

    pub(crate) fn new_runtime(kind: RuntimeError) -> Self {
        Self {
            kind: ErrorKind::Runtime(kind),
        }
    }
}

impl Display for WrenError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use CompileError as CE;
        use ErrorKind as EK;
        use ParseError as PE;
        use RuntimeError as RE;

        let prefix = match self.kind {
            EK::Parse(_) => "parse",
            EK::Compile(_) => "compile",
            EK::Runtime(_) => "runtime",
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
            EK::Compile(err) => match err {
                CE::MaxSymbols => write!(f, "symbol table may only contain {MAX_SYMBOLS} entries"),
                CE::MaxModuleVariables => write!(f, "module may only contain {MAX_MODULE_VARS} variables"),
                CE::ScopeVariableExists(name) => write!(f, "scope variable '{name}' already defined"),
                CE::ModuleVariableExists(name) => write!(f, "module variable '{name}' already defined"),
                CE::ForwardVariable(name, line) => write!(
                    f,
                    "variable '{name}' referenced before this definition (first use at line {line})"
                ),
                CE::SymbolExists => write!(f, "symbol already defined"),
                CE::SymbolNotFound => write!(f, "symbol not found"),
                CE::UnexpectedEndOfTokens => write!(f, "unexpected end of tokens"),
                CE::InvalidOperator => write!(f, "value does not support operator"),
            },
            EK::Runtime(err) => match err {
                RE::StackOverflow => write!(f, "stack overflow"),
                RE::StackUnderflow => write!(f, "stack underflow"),
                RE::InvalidSlot(slot_id) => write!(f, "invalid slot {slot_id}"),
                RE::InvalidType => write!(f, "invalid value type"),
                RE::MethodNotFound => write!(f, "method not found"),
            },
        }
    }
}
