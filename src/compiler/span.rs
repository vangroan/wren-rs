use std::fmt::{self, Debug, Display};

/// Points to a slice of source code.
#[derive(Clone, PartialEq, Eq)]
pub struct Span {
    pub pos: usize,
    pub size: usize,
}

impl Span {
    pub fn empty(pos: usize) -> Self {
        Self { pos, size: 0 }
    }

    pub fn new(pos: usize, size: usize) -> Self {
        Self { pos, size }
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Formatted as a tuple to reduce the size of prints.
        // Spans are everywhere and pollute the output.
        f.debug_tuple("Span").field(&self.pos).field(&self.size).finish()
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.pos, self.size)
    }
}
