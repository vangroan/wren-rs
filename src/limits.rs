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
