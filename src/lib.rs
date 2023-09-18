mod builder;
pub mod compiler;
mod error;
mod string;
mod vm;

pub use {self::builder::WrenBuilder, self::vm::WrenVm};

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// The version of the reference Wren implementation that this library targets.
pub const COMPAT_VERSION: &str = "0.4.0";
