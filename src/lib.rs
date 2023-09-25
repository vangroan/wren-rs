#![doc = include_str!("../README.md")]

mod builder;
pub mod compiler;
mod error;
mod limits;
mod string;
mod vm;

pub use {self::builder::WrenBuilder, self::vm::WrenVm};

/// The version of this Wren implementation.
///
/// This will differ from the upstream Wren C implementation. See [`COMPAT_VERSION`]
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// The version of the upstream Wren implementation that this library targets.
pub const COMPAT_VERSION: &str = "0.4.0";
