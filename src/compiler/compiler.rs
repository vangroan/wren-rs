use super::lexer::Lexer;

pub struct WrenCompiler<'a> {
    lexer: Lexer<'a>,

    /// FIXME: Circular dependency between Compiler and VM
    ///
    /// The parser and compiler need the current VM to:
    ///
    /// - Create new functions, the basic unit of code.
    /// - Allocate new strings.
    /// - Push new sub-compilers onto the stack.
    /// - Use compiler state as GC roots.
    /// vm: &'wren WrenVm;

    /// The compiler for the function enclosing this one,
    /// or `None` if it's the top module level.
    parent: Option<Box<WrenCompiler<'a>>>,

    /// If this is a compiler for a method, then we keep
    /// track of the class enclosing it.
    enclosing_class: Option<ClassInfo>,
}

/// Bookkeeping information for compiling a class definition.
struct ClassInfo {}

impl<'a> WrenCompiler<'a> {
    pub fn new(lexer: Lexer<'a>, parent: Option<Box<WrenCompiler<'a>>>) -> Self {
        Self {
            lexer,
            parent,
            enclosing_class: None,
        }
    }

    pub fn compile(&mut self) {}
}
