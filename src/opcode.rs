use crate::value::ConstantId;
use crate::SymbolId;

/// The number of arguments for a function call, excluding the receiver's slot.
#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct Arity(u8);

impl Arity {
    pub(crate) fn new(arg_count: u8) -> Self {
        Self(arg_count)
    }

    pub(crate) fn from_usize(index: usize) -> Self {
        if index > u8::MAX as usize {
            // TODO: WrenError
            panic!("arity overflow");
        }

        Self::new(index as u8)
    }

    pub(crate) fn as_u8(self) -> u8 {
        self.0
    }

    pub(crate) fn as_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Op {
    NoOp,

    /// Push the constant value with the given index onto the stack.
    Constant(ConstantId),

    // Invoke the method with symbol.
    Call(Arity, SymbolId),

    /// Exit from the current function and return the value on the top of the stack.
    Return,

    /// This is executed at the end of the module's body. Pushes NULL onto the stack
    /// as the "return value" of the import statement and stores the module as the
    /// most recently imported one.
    EndModule,

    /// This pseudo-instruction indicates the end of the bytecode. It should
    /// always be preceded by a [`Op::Return`], so is never actually executed.
    End,
}

impl Op {
    /// The instruction's "stack effect" -- the amount that the operation
    /// changes the size of the stack.
    ///
    /// A stack effect of 1 means it pushes a value and the stack grows one larger.
    /// -2 means it pops two values, etc.
    pub(crate) fn stack_effect(&self) -> isize {
        use Op::*;

        match self {
            NoOp => 0,
            Call(arg_count, _) => -(arg_count.as_usize() as isize),
            Constant(_) => 1,
            Return => 0,
            EndModule => 1,
            End => -2,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_opcode_size() {
        println!("size_of::<Op>() -> {}", std::mem::size_of::<Op>());
    }
}
