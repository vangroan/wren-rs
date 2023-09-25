// The maximum depth that interpolation can nest. For example, this string has
// three levels:
//
//      "outside %(one + "%(two + "%(three)")")"
pub const MAX_INTERPOLATION_NESTING: usize = 8;
