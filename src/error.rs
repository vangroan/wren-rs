//! Error type.
use std::fmt::{self, Display, Formatter};

pub type WrenResult<T> = std::result::Result<T, WrenError>;

#[derive(Debug)]
pub struct WrenError {
    kind: ErrorKind,
    message: String,
}

#[derive(Debug)]
pub enum ErrorKind {
    Compile,
    Runtime,
}

impl Display for WrenError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "error: {}", self.message)
    }
}

impl WrenError {
    pub fn new_compile(message: impl ToString) -> Self {
        Self {
            kind: ErrorKind::Compile,
            message: message.to_string(),
        }
    }
}
