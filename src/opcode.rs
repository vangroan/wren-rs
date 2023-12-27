use crate::value::ConstantId;
use crate::SymbolId;

/// The number of arguments for a function call, excluding the receiver's slot.
#[derive(Debug, Default, Clone, Copy)]
pub struct Arity(u8);

impl Arity {
    pub(crate) const fn new(arg_count: u8) -> Self {
        Self(arg_count)
    }

    pub(crate) fn from_usize(index: usize) -> Self {
        if index > u8::MAX as usize {
            // TODO: WrenError
            panic!("arity overflow");
        }

        Self::new(index as u8)
    }

    pub(crate) const fn as_u8(self) -> u8 {
        self.0
    }

    pub(crate) const fn as_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone)]
pub enum Op {
    NoOp,

    /// Push the constant value with the given index onto the stack.
    Constant(ConstantId),

    /// Push [`crate::value::Value::Null`] onto the stack.
    PushNull,

    /// Pushed the value in the given local slot onto the stack.
    PushLocal(u8),

    /// Store the value at the top of the stack in the module variable identified by the symbol.
    ///
    /// Does not pop the stack.
    StoreModVar(SymbolId),

    /// Pushes the value of the module-level variable identified by the symbol onto the stack.
    LoadModVar(SymbolId),

    // Pop and discard the top value of the stack.
    Pop,

    /// Invoke the method with symbol.
    Call(Arity, SymbolId),

    /// Exit from the current function and return the value on the top of the stack.
    Return,

    /// Creates a closure for the function stored at in the constant table.
    Closure(ConstantId),

    /// Creates a class. Top of stack is the superclass. Below that is a string for
    /// the name of the class. The `u8` argument is the number of fields in the class.
    ///
    /// After evaluation, the top of the stack will contain the class reference object.
    ///
    /// # Examples
    ///
    /// In a simple example where the class does not explicitly inherit a parent,
    /// the parent is implicitly `Object`.
    ///
    /// ```wren
    /// class Foo {}  // 0 fields
    /// ```
    /// # Stack Layout
    ///
    /// ```text
    ///  |  Object |  ObjClass
    ///  |  "Foo"  |  Constant
    ///  |   ...   |
    /// ```
    Class(u8),

    /// Creates a foreign class. Top of stack is the superclass. Below that is a
    /// string for the name of the class.
    ForeignClass,

    /// Completes the process for creating a new class.
    ///
    /// The class attributes instance and the class itself should be on the
    /// top of the fiber's stack.
    ///
    /// This process handles moving the attribute data for a class from
    /// compile time to runtime, since it now has all the attributes associated
    /// with a class, including for methods.
    EndClass,

    /// Define a method for symbol [arg]. The class receiving the method is popped
    /// off the stack, then the function defining the body is popped.
    ///
    /// If a foreign method is being defined, the "function" will be a string
    /// identifying the foreign method. Otherwise, it will be a function or
    /// closure.
    MethodInstance(SymbolId),

    /// Define a method for symbol [arg]. The class whose metaclass will receive
    /// the method is popped off the stack, then the function defining the body is
    /// popped.
    ///
    /// If a foreign method is being defined, the "function" will be a string
    /// identifying the foreign method. Otherwise, it will be a function or
    /// closure.
    MethodStatic(SymbolId),

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
            PushNull => 1,
            PushLocal(_) => 1,
            StoreModVar(_) => 0,
            LoadModVar(_) => 1,
            Pop => -1,
            Return => 0,
            Closure(_) => 1,
            Class(_) => -1,
            ForeignClass => -1,
            EndClass => -2,
            MethodInstance(_) => -2,
            MethodStatic(_) => -2,
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
