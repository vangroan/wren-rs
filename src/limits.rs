/// The maximum depth that interpolation can nest. For example, this string has
/// three levels:
///
/// ```wren
///      "outside %(one + "%(two + "%(three)")")"
/// ```
pub const MAX_INTERPOLATION_NESTING: usize = 8;

/// The maximum number of distinct constants that a function can contain. This
/// value is explicit in the bytecode since [`crate::opcode::Op::Constant`] only takes a single
/// two-byte argument.
pub const MAX_CONSTANTS: usize = 1 << 16;

/// The maximum number of module-level variables that may be defined at one time.
/// This limitation comes from the 16 bits used for the arguments to
/// `CODE_LOAD_MODULE_VAR` and `CODE_STORE_MODULE_VAR`.
pub const MAX_MODULE_VARS: usize = 1 << 16;

pub const MAX_SYMBOLS: usize = 1 << 16;

pub const MAX_VARIABLE_NAME: usize = 64;

pub const MAX_PARAMETERS: usize = 16;

pub const MAX_METHOD_NAME: usize = 64;
