#[derive(Debug, Clone)]
pub(crate) enum Op {
    NoOp,

    /// Push the constant value with the given index onto the stack.
    Constant(u16),

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
            Constant(_) => 1,
            Return => 0,
            EndModule => 1,
            End => -2,
        }
    }
}
