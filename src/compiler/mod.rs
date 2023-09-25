#![allow(clippy::module_inception)]

mod compiler;
mod cursor;
mod lexer;
mod span;
mod token;

// TODO: Lexer does not have to be public
pub use self::{
    compiler::WrenCompiler,
    lexer::Lexer,
    span::Span,
    token::{LiteralValue, Token, TokenKind},
};
