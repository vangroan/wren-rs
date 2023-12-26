// TODO: Remove when compiler implementation is done
#![allow(dead_code)]

use crate::compiler::token::{KeywordKind, Token, TokenExt, TokenKind};
use crate::error::{CompileError, ErrorKind, WrenError, WrenResult};
use crate::limits::*;
use crate::opcode::{Arity, Op};
use crate::symbol::SymbolTable;
use crate::value::{new_handle, Handle, ModuleDump, ObjFn, ObjModule, Value};
use crate::SymbolId;

use arrayvec::ArrayString;

use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::Infallible;
use std::fmt::{self, Write as _};
use std::mem;
use std::num::NonZeroUsize;
use std::rc::Rc;

use super::lexer::Lexer;

/// The name of the local variable that holds the receiver object of the method call.
pub const RECEIVER_NAME: &str = "this";

pub struct WrenCompiler<'src> {
    /// Lexical analyser.
    ///
    /// The lexer is shared so functions on the compiler's stack don't consume
    /// the same source code.
    lexer: Lexer<'src>,

    /// Method signature table shared with the virtual machine.
    ///
    /// All modules in the program share the same table, and a given signature
    /// must have the same symbol Id throughout the whole program. Modules can then
    /// call methods on classes from other modules without knowing which function
    /// it will resolve to.
    ///
    /// The method lookups are sort-of "pre-hashed" this way at compiletime. This is
    /// how Wren manages to avoid looking up functions in hashmaps at runtime.
    method_names: Handle<SymbolTable>,

    locals: Vec<Local>,

    /// The current number of slots (locals and temporaries) in use.
    ///
    /// We use this and maxSlots to track the maximum number of additional slots
    /// a function may need while executing. When the function is called, the
    /// fiber will check to ensure its stack has enough room to cover that worst
    /// case and grow the stack if needed.
    ///
    /// This value here doesn't include parameters to the function. Since those
    /// are already pushed onto the stack by the caller and tracked there, we
    /// don't need to double count them here.
    num_slots: usize,

    /// The current level of block scope nesting, where zero is no nesting.
    scope_depth: ScopeDepth,

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
    // parent: Option<&'parent WrenCompiler<'src, 'vm>>,

    /// The Wren module currently being compiled.
    module: Handle<ObjModule>,

    /// If this is a compiler for a method, then we keep
    /// track of the class enclosing it.
    /// TODO: This may have to be a stack because we don't share pointers like C
    enclosing_class: Option<ClassInfo>,

    /// The function being compiled.
    func: FuncState,
    func_stack: Vec<FuncState>,

    /// The last token that was received from the lexer.
    token: Option<Token>,

    /// Collected document comments.
    doc_comments: Vec<Token>,
}

/// Local variable.
#[derive(Debug)]
struct Local {
    /// The name of the local variable as it appears in source code.
    name: String,

    /// The depth in the scope chain that this variable was declared at. Zero is
    /// the outermost scope-parameters for a method, or the first local block in
    /// top level code. One is the scope within that, etc.
    depth: usize,

    /// If this local variable is being used as an up-value.
    is_upvalue: bool,
}

/// Bookkeeping information for compiling a class definition.
struct ClassInfo {
    /// The variable (either module-level or local) containing the class object.
    var: Variable,
    is_foreign: bool,
    name: String,

    /// Table of class fields, built up as they are encountered in method bodies.
    ///
    /// Note that in Wren fields are not explicitly defined in the class body,
    /// like Java or C# would. Instead they are implicitly defined as they are used
    /// in expressions.
    fields: SymbolTable,

    /// Symbols for the methods defined by the class.
    ///
    /// Used for detecting duplicate definitions.
    methods: Vec<SymbolId>,
    static_methods: Vec<SymbolId>,

    /// True if the current method being compiled is static.
    in_static: bool,

    /// Class level attribute annotations.
    attributes: HashMap<String, ()>,
}

/// Bookkeeping information for compiling a function definition.
struct FuncState {
    /// The function object being compiled.
    func: ObjFn,

    /// Local variables, and up-values, declared in the function.
    locals: Vec<Local>,

    /// The current number of slots (locals and temporaries) in use.
    ///
    /// We use this and maxSlots to track the maximum number of additional slots
    /// a function may need while executing. When the function is called, the
    /// fiber will check to ensure its stack has enough room to cover that worst
    /// case and grow the stack if needed.
    ///
    /// This value here doesn't include parameters to the function. Since those
    /// are already pushed onto the stack by the caller and tracked there, we
    /// don't need to double count them here.
    num_slots: usize,

    /// Scope context describes the outer function context of the body currently
    /// being compiled.
    ctx: ScopeContext,

    /// If this function is a method this will contain the enclosing class information.
    enclosing_class: Option<Rc<RefCell<ClassInfo>>>,
}

impl FuncState {
    fn new(module: Handle<ObjModule>) -> Self {
        Self {
            func: ObjFn::new(module),
            locals: Vec::new(),
            num_slots: 0,
            ctx: ScopeContext::Method,
            enclosing_class: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum ScopeDepth {
    Module,
    Block(NonZeroUsize),
}

impl ScopeDepth {
    fn is_module(&self) -> bool {
        matches!(self, ScopeDepth::Module)
    }

    fn block_depth(&self) -> usize {
        match self {
            Self::Module => panic!("scope depth is at module level, not at block level"),
            Self::Block(depth) => depth.get(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ScopeContext {
    /// The implicit top-level function that acts as the module (or script) body.
    Module,
    /// The class body, outside any method bodies.
    ClassBody,
    /// A function directly inside a class body.
    Method,
    /// An anonymous block function locally inside another function body.
    Block,
}

#[derive(Debug, Clone)]
enum Scope {
    Local,
    UpValue,
    Module,
}

#[derive(Debug, Clone)]
struct Variable {
    id: VarId,
    scope: Scope,
}

/// Variable identifier.
#[derive(Debug, Clone)]
enum VarId {
    /// Variable is declared in the module's symbol table, the "global" scope.
    Module(SymbolId),

    /// Variable is declared in a local scope.
    ///
    /// The number is the temporary index in the [`WrenCompiler`] `locals` stack.
    /// It is only meaningful for the lifetime of the scope during _compilation_.
    /// Once the scope is truncated off the compiler's stack, this index should be discarded,
    /// and won't be useful for the interpreter later.
    Local(u8),
}

impl VarId {
    fn symbol(&self) -> Option<SymbolId> {
        match self {
            Self::Module(symbol) => Some(symbol.clone()),
            Self::Local(_) => None,
        }
    }

    fn local_slot(&self) -> Option<u8> {
        match self {
            Self::Local(slot) => Some(*slot),
            Self::Module(_) => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum BlockBodyKind {
    Empty,
    Expr,
    Stmt,
}

impl BlockBodyKind {
    fn is_empty(&self) -> bool {
        matches!(self, Self::Empty)
    }

    fn is_expression(&self) -> bool {
        matches!(self, Self::Expr)
    }

    fn is_statement(&self) -> bool {
        matches!(self, Self::Stmt)
    }
}

impl<'src> WrenCompiler<'src> {
    pub fn new(module: Handle<ObjModule>, source: &'src str, method_names: Handle<SymbolTable>) -> Self {
        Self {
            lexer: Lexer::from_source(source),
            method_names,
            locals: Vec::new(),
            num_slots: 0,
            scope_depth: ScopeDepth::Module,
            module: module.clone(),
            enclosing_class: None,
            func: FuncState::new(module.clone()),
            func_stack: Vec::new(),
            token: None,
            doc_comments: Vec::new(),
        }
    }

    // FIXME: Private ObjFn type in public compile() signature.
    pub(crate) fn compile(mut self, is_expression: bool) -> WrenResult<Handle<ObjFn>> {
        println!("compile");
        self.func = FuncState::new(self.module.clone());

        // Initialise.
        self.next_token()?;
        self.ignore_newlines()?;

        if is_expression {
            todo!("expression parsing");
        } else {
            self.compile_def_stmts()?;
            self.emit_op(Op::EndModule);
        }

        self.emit_op(Op::Return);
        self.end_compiler()
    }

    fn end_compiler(mut self) -> WrenResult<Handle<ObjFn>> {
        println!("end_compiler");

        // Mark the end of the bytecode. Since it may contain multiple early returns,
        // we can't rely on Op::Return to tell us we're at the end.
        self.emit_op(Op::End);

        let module = &*self.module.borrow();
        let dump = ModuleDump::new(module);
        println!("{dump}");

        // Take function state and replace it with a fresh function.
        let func_state = mem::replace(&mut self.func, FuncState::new(self.module.clone()));
        let func_handle = new_handle(func_state.func);
        Ok(func_handle)
    }

    /// End the current function.
    fn end_func(&mut self) {
        println!("end_func");

        // Mark the end of the bytecode. Since it may contain multiple early returns,
        // we can't rely on Op::Return to tell us we're at the end.
        self.emit_op(Op::End);

        // Pop the current function and set the previous function to current.
        let func = self.pop_func_scope();

        // Unpack function state.
        let FuncState { func, .. } = func;

        // Function object will now be managed by the VM.
        let func_handle = new_handle(func);

        // Load the inner function object into the outer function as a closure object.
        let constant_id = self.func.func.intern_constant(func_handle.clone().into());

        // Wrap the function in a closure. We do this even if it has no up-values so
        // that the VM can uniformly assume all called objects are closures. This
        // makes creating a function a little slower, but makes invoking them
        // faster. Given that functions are invoked more often than they are
        // created, this is a win.
        self.emit_op(Op::Closure(constant_id));

        // TODO: Emit arguments for each up-value.
    }

    fn push_scope(&mut self) {
        match &mut self.scope_depth {
            ScopeDepth::Module => self.scope_depth = ScopeDepth::Block(NonZeroUsize::new(1).unwrap()),
            ScopeDepth::Block(depth) => {
                let depth = NonZeroUsize::new(depth.get() + 1).unwrap();
                self.scope_depth = ScopeDepth::Block(depth)
            }
        }
    }

    fn pop_scope(&mut self) {
        match &mut self.scope_depth {
            ScopeDepth::Module => {
                panic!("currently at module scope, so cannot pop scopes any further")
            }
            ScopeDepth::Block(depth) => {
                let depth = depth.get() - 1;
                if depth == 0 {
                    self.scope_depth = ScopeDepth::Module
                } else {
                    self.scope_depth = ScopeDepth::Block(NonZeroUsize::new(depth).unwrap())
                }
            }
        }
    }

    /// Pushes a new enclosing class onto the class stack.
    fn class_scope<R>(&mut self, _func: impl FnOnce(&mut Self) -> WrenResult<R>) {
        todo!()
    }

    /// Pushes a new function scope onto the function stack.
    fn push_func_scope(&mut self, enclosing_class: Option<Rc<RefCell<ClassInfo>>>) {
        // Implicitly the new function is part of the old function's module.
        let module = self.func.func.module.clone();
        let mut new_func = FuncState::new(module);
        new_func.enclosing_class = enclosing_class;
        let prev_func = mem::replace(&mut self.func, new_func);
        self.func_stack.push(prev_func);
    }

    /// Pop the current function scope, without emitting any constants, variables or opcodes.
    fn pop_func_scope(&mut self) -> FuncState {
        let prev_func = self
            .func_stack
            .pop()
            .expect("ending function failed because lexical stack is empty");
        mem::replace(&mut self.func, prev_func)
    }

    /// Pushes a new function scope onto the function stack.
    ///
    /// Further compilation will emit bytecode, and intern constants, to this newly created function.
    fn func_scope<R>(&mut self, parselet: impl FnOnce(&mut Self) -> WrenResult<R>) -> WrenResult<(R, FuncState)> {
        // Implicitly the new function is part of the old function's module.
        let module = self.func.func.module.clone();
        let new_func = FuncState::new(module);
        let prev_func = mem::replace(&mut self.func, new_func);
        self.func_stack.push(prev_func);

        let result = parselet(self);

        // Take the new function for return, and put the previous function back.
        let prev_func = self
            .func_stack
            .pop()
            .expect("function stack must contain the previous function at the top");
        let new_func = mem::replace(&mut self.func, prev_func);

        // Return the newly compiled function so the caller
        // can decide what to do with it.
        result.map(|r| (r, new_func))
    }

    fn parent_func(&self) -> Option<&FuncState> {
        self.func_stack.last()
    }

    fn has_token(&self) -> bool {
        match self.token.kind() {
            Some(TokenKind::End) | None => false,
            Some(_) => true,
        }
    }

    fn next_token(&mut self) -> WrenResult<()> {
        loop {
            match self.lexer.next_token() {
                Ok(token) => {
                    println!(
                        "next_token ➡ {:>6}:{:<4} {:?}",
                        token.span.pos, token.span.size, token.kind
                    );

                    // Collect document comments, which will implicitly belong to
                    // the token following the last comment line.
                    match token.kind {
                        TokenKind::DocComment => {
                            self.doc_comments.push(token);
                            continue;
                        }
                        TokenKind::Comment | TokenKind::BlockComment => {
                            continue;
                        }
                        _ => {
                            self.token = Some(token);
                            return Ok(());
                        }
                    }
                }
                Err(err) => {
                    self.token = None;
                    return Err(err);
                }
            }
        }
    }

    fn try_token(&self) -> WrenResult<&Token> {
        match &self.token {
            Some(token) => Ok(token),
            None => Err(WrenError {
                kind: ErrorKind::Compile(CompileError::UnexpectedEndOfTokens),
            }),
        }
    }

    fn consume_token(&mut self, kind: TokenKind) -> WrenResult<Token> {
        let token = self
            .token
            .as_ref()
            .ok_or_else(|| WrenError::new_compile(CompileError::UnexpectedEndOfTokens))?;
        if kind == token.kind {
            let token = self.token.take().unwrap();
            self.next_token()?;
            Ok(token)
        } else {
            // panic!("expected {:?}, encountered {:?}", kind, token.kind);
            Err(WrenError::new_compile(CompileError::UnexpectedToken(kind, token.kind)))
        }
    }

    fn match_token(&mut self, kind: TokenKind) -> WrenResult<bool> {
        if let Some(token) = self.token.as_ref() {
            if kind == token.kind {
                self.next_token()?;
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            Ok(false)
        }
    }

    fn match_keyword(&mut self, keyword: KeywordKind) -> WrenResult<bool> {
        self.match_token(TokenKind::Keyword(keyword))
    }

    /// Take ownership of the current token, leaving `None` in the [`self.token`] field.
    fn take_token(&mut self) -> WrenResult<Token> {
        self.token
            .take()
            .ok_or_else(|| WrenError::new_compile(CompileError::UnexpectedEndOfTokens))
    }

    /// Check the current token against the given token kind.
    ///
    /// Returns `Ok` if they match, advancing to the next token.
    ///
    /// Returns [`WrenError`] if they mismatch, in which case the compiler
    /// will not advance tot he next token.
    ///
    /// Will return [`CompileError::UnexpectedEndOfTokens`] if the stream is
    /// at the end of the source code.
    fn expect_token(&mut self, kind: TokenKind) -> WrenResult<()> {
        match self.token.kind() {
            Some(current_kind) => {
                if current_kind == kind {
                    self.next_token()?;
                    Ok(())
                } else {
                    Err(WrenError::new_compile(CompileError::UnexpectedToken(
                        kind,
                        current_kind,
                    )))
                }
            }
            None => Err(WrenError::new_compile(CompileError::UnexpectedEndOfTokens)),
        }
    }

    /// Check whether the current token terminates a statement.
    fn expect_end_of_statement(&mut self) -> WrenResult<()> {
        match self.token.kind() {
            Some(TokenKind::Newline) | Some(TokenKind::End) | None => {
                self.next_token()?;
                Ok(())
            }
            // TODO: Error messages that can list expected tokens. Eg. "expected newline or end-of-file, encountered ..."
            Some(kind) => Err(WrenError::new_compile(CompileError::UnexpectedToken(
                TokenKind::Newline,
                kind,
            ))),
        }
    }

    /// Declares a variable in the current scope whose name is the given token.
    fn declare_variable(&mut self, token: &Token) -> WrenResult<Variable> {
        let fragment = token.fragment(self.lexer.source());

        match self.scope_depth {
            ScopeDepth::Module => {
                let symbol = self.module.borrow_mut().define_var(fragment, Value::Null)?;
                Ok(Variable {
                    id: VarId::Module(symbol),
                    scope: Scope::Module,
                })
            }
            ScopeDepth::Block(scope_depth) => {
                // Check if this variable is already declared in this scope.
                //
                // Outer scopes are OK, because they will be shadowed by this one.
                for local in self.locals.iter().rev() {
                    // Scanning variables back to front.
                    // Outer scopes are earlier in the vector, more local scopes are later.
                    if local.depth < scope_depth.get() {
                        break;
                    }

                    if local.name == fragment {
                        return Err(WrenError::new_compile(CompileError::ModuleVariableExists(
                            fragment.to_string(),
                        )));
                    }
                }

                // Index in the scope stack.
                let slot = self.add_local(fragment)?;

                // This function returns both module variable symbol and local stack offset.
                Ok(Variable {
                    id: VarId::Local(slot),
                    scope: Scope::Local,
                })
            }
        }
    }

    fn signature_symbol(&mut self, sig: &Signature) -> WrenResult<SymbolId> {
        let mut sig_str = ArrayString::<MAX_METHOD_SIGNATURE>::new();
        write!(sig_str, "{}", sig).expect("write to signature string");
        self.method_names.borrow_mut().insert(sig_str.as_str())
    }

    /// Declare a method in the enclosing class info.
    ///
    /// This resolves the symbol in the VM environment, then adds
    /// the symbol to the current enclosing [`ClassInfo`] method table.
    fn declare_method(&mut self, class: &mut ClassInfo, sig: &Signature) -> WrenResult<SymbolId> {
        // Map method signature string to symbol
        let sig_str = sig.to_string();
        let symbol = self.method_names.borrow_mut().insert(sig_str.as_str())?;

        let method_symbols = if class.in_static {
            &mut class.static_methods
        } else {
            &mut class.methods
        };

        // Check if the outer class already has this method declared.
        if method_symbols.contains(&symbol) {
            return Err(WrenError::new_compile(CompileError::DuplicateMethod(
                class.name.to_string(),
                sig_str,
            )));
        }

        method_symbols.push(symbol.clone());

        Ok(symbol)
    }

    /// Loads the enclosing class onto the stack and then binds the function already
    /// on the stack as a method on that class.
    fn define_method(&mut self, class_var: &Variable, method_symbol: SymbolId, is_static: bool) -> WrenResult<()> {
        // Load the class. We have to do this for each method because we can't
        // keep the class on top of the stack. If there are static fields, they
        // will be locals above the initial variable slot for the class on the
        // stack. To skip past those, we just load the class each time right before
        // defining a method.
        self.emit_load_var(class_var);

        // Define the method.
        let op = if is_static {
            Op::MethodStatic(method_symbol)
        } else {
            Op::MethodInstance(method_symbol)
        };
        self.emit_op(op);

        Ok(())
    }

    /// Add a local to the compiler's scope stack.
    ///
    /// Returns the index where the new local was inserted.
    fn add_local(&mut self, name: impl ToString) -> WrenResult<u8> {
        let slot = self.locals.len();

        if slot >= MAX_LOCALS {
            return Err(WrenError::new_compile(CompileError::MaxLocals));
        }

        self.locals.push(Local {
            name: name.to_string(),
            depth: self.scope_depth.block_depth(),
            is_upvalue: false,
        });

        Ok(slot as u8)
    }
}

/// Functions that compile tokens.
impl<'src> WrenCompiler<'src> {
    fn ignore_newlines(&mut self) -> WrenResult<()> {
        while let Some(token) = &self.token {
            if token.kind == TokenKind::Newline {
                self.next_token()?;
            } else {
                break;
            }
        }

        Ok(())
    }

    fn compile_def_stmts(&mut self) -> WrenResult<()> {
        while self.has_token() {
            println!("has_token: {:?}", self.try_token());
            self.compile_def_stmt()?;
        }

        Ok(())
    }

    fn compile_def_stmt(&mut self) -> WrenResult<()> {
        use KeywordKind::*;
        use TokenKind::*;

        let token = self.try_token()?;
        println!("compile_def_stmt {:?}", token);

        // TODO: Compile attributes starting with hash (#)

        match token.kind {
            Keyword(Class) => self.compile_class(false),
            Keyword(Foreign) => self.compile_foreign(),
            Keyword(Import) => self.compile_import(),
            Keyword(Var) => self.compile_var_def(),
            _ => self.compile_stmt(),
        }
    }

    /// Compile a simple statement.
    fn compile_stmt(&mut self) -> WrenResult<()> {
        use KeywordKind::*;
        use TokenKind::*;

        let token = self.try_token()?;

        match token.kind {
            Keyword(Return) => self.compile_return(),
            Keyword(kw) => panic!("unexpected keyword: {:?}", kw),
            _ => self.compile_expr(),
        }
    }

    /// Compile a class definition.
    ///
    /// # TODO
    ///
    /// - Create a variable to store the class in.
    /// - Create shared class name value
    /// - Create class name string to track method duplicates
    /// - Make a string constant for the name.
    /// - Load the superclass (if there is one).
    fn compile_class(&mut self, is_foreign: bool) -> WrenResult<()> {
        debug_assert!(matches!(self.token.keyword(), Some(KeywordKind::Class)));

        self.consume_token(TokenKind::Keyword(KeywordKind::Class))?;

        println!("rest: {:?}", self.lexer.rest());
        println!("token1 {:?}", self.token);

        // --------------------------------------------------------------------
        // Create a variable to store the class in.
        //
        // The class itself is a reference object that has to be accessible in the scope
        // as a variable, to use as a receiver for static method calls, or constructing
        // an instance object.
        let class_name = self.consume_token(TokenKind::Name)?;
        let class_var = self.declare_variable(&class_name)?;
        let class_name_str = class_name.fragment(self.lexer.source());
        self.emit_constant(Value::from_str(class_name_str));

        println!("rest: {:?}", self.lexer.rest());
        println!("token2 {:?}", self.token);

        // --------------------------------------------------------------------
        // Super class
        //
        // Load the super class if there is one, else our class
        // inherits from the built-in `Object` class.
        if self.match_keyword(KeywordKind::Is)? {
            // Inheriting from a super class allows for an expression
            // that must evaluate to another class object.
            self.compile_expr_precedence(Precedence::Call)?;
        } else {
            // load builtin `Object` class variable from core module.
            let obj_class = self.module.borrow().resolve_var("Object").expect("core module must be loaded");
            self.emit_op(Op::LoadModVar(obj_class));
        }

        // --------------------------------------------------------------------
        // Number-of-fields argument
        //
        // Store a placeholder for the number of fields argument. We don't know the
        // count until we've compiled all the methods to see which fields are used.
        //
        // In Wren a class' fields are not defined in the definition body, but
        // instead implicitly declared on first usage inside method bodies.
        //
        // Not applicable to foreign classes because they don't have fields in Wren memory.
        let mut class_op_index = None;

        // Note: The ForeignClass and Class instructions are expected to pop the super class'
        //       name off the stack, but leave our current class' name on top.
        if is_foreign {
            self.emit_op(Op::ForeignClass);
        } else {
            class_op_index = Some(self.reserve_op(Op::Class(255)));
        }

        // --------------------------------------------------------------------
        // Define module variable
        //
        // Store a module-level class definition in the module's variable table.
        //
        // A block local class will be on the stack later.
        if let VarId::Module(symbol) = class_var.id {
            self.emit_store_mod_var(symbol);
        }

        // --------------------------------------------------------------------
        // Body
        //
        // Push a new local scope. Static fields used in methods will be hoisted
        // into local variables in this scope. Methods that use them will have
        // up-value referencing them.
        self.push_scope();

        // Shared bookkeeping information for the current class, to be mutated by
        // whichever method is currently being compiled.
        let class_info = ClassInfo {
            var: class_var.clone(),
            is_foreign,
            name: class_name_str.to_string(),

            // Set up a symbol table for the class's fields. We'll initially compile
            // them to slots starting at zero. When the method is bound to the class, the
            // bytecode will be adjusted by [wrenBindMethod] to take inherited fields
            // into account.
            fields: SymbolTable::new(),

            // Set up symbol buffers to track duplicate static and instance methods.
            methods: Vec::new(),
            static_methods: Vec::new(),
            in_static: false,

            // TODO: New attributes feature.
            attributes: HashMap::new(),
        };

        // TODO: Take collected attributes in compiler and move them into class.

        // Share the class info between methods using the heap, to avoid
        // annoying self-borrow constraints with the function stack.
        let enclosing_class = Rc::new(RefCell::new(class_info));

        println!("rest: {:?}", self.lexer.rest());
        println!("token3 {:?}", self.token);
        self.consume_token(TokenKind::LeftBrace)?;
        self.ignore_newlines()?;

        while !self.match_token(TokenKind::RightBrace)? {
            println!("compiling method");
            self.compile_method(enclosing_class.clone(), &class_var)?;

            // Don't require a newline after the last definition.
            if self.match_token(TokenKind::RightBrace)? {
                break;
            }

            // Method definition in class body must end with a newline.
            println!("compile_class: end method definition");
            self.consume_token(TokenKind::Newline)?;
        }

        println!("rest: {:?}", self.lexer.rest());
        println!("token4 {:?}", self.token);

        // Update the class instruction with the final number of fields.
        if !is_foreign {
            if let Some(op_index) = class_op_index {
                let field_count = enclosing_class.borrow().fields.len();
                if field_count >= MAX_FIELDS {
                    return Err(WrenError::new_compile(CompileError::MaxFields));
                }
                self.patch_op(op_index, Op::Class(field_count as u8));
            }
        }

        self.pop_scope();
        self.expect_end_of_statement()?;

        Ok(())
    }

    /// Compile a method definition inside a class body.
    ///
    /// - `enclosing_class` The class definition that owns this method.
    /// - `class_var` A variable (either module level or local) containing the class object.
    fn compile_method(&mut self, enclosing_class: Rc<RefCell<ClassInfo>>, class_var: &Variable) -> WrenResult<()> {
        println!("compile_method {:?}", self.token);

        let mut class = enclosing_class.borrow_mut();

        // TODO: Compile method attributes.

        let is_foreign = self.match_keyword(KeywordKind::Foreign)?;
        let is_static = self.match_keyword(KeywordKind::Static)?;
        class.in_static = is_static;

        // Default the signature to a getter. The simplest method kind?
        let mut sig = self.create_signature(SignatureKind::Getter)?;

        // Start a new function inside the lexical scope of the current function.
        self.push_func_scope(Some(enclosing_class.clone()));

        // Compile the method signature.
        self.compile_signature(&mut sig)?;

        // The method symbol must be declared in the enclosing class, after the
        // the signature's arity has been determined.
        let method_symbol = self.declare_method(&mut *class, &sig)?;

        // Foreign methods may not have bodies.
        if is_foreign {
            // Define a constant for the full signature.
            self.emit_constant(Value::Str(sig.to_string()));

            // The function scope used to compile the signature is
            // no longer needed.
            //
            // Foreign function has no bytecode or closure object.
            self.pop_func_scope();
        } else {
            self.compile_method_body()?;

            self.end_func();
        }

        self.define_method(class_var, method_symbol, is_static)?;

        // When the method is an initializer,
        // then also define a matching constructor method on the metaclass.
        if sig.kind == SignatureKind::Initializer {
            sig.kind = SignatureKind::Method;
            let _symbol = self.signature_symbol(&sig);

            // TODO: Create constructor
        }

        Ok(())
    }

    /// Compile the body of a method, which can be an expression or
    /// a list of statements.
    ///
    /// A block body is considered an expression if code immediately follows
    /// the opening brace without a newline. During execution the expression
    /// will leave a value on the stack which will be returned implicitly.
    ///
    /// ```wren
    /// add(x, y) { x + y }
    /// ```
    ///
    /// If a newline follows the opening brace, the body is parsed as a list
    /// of statements. Explicit return statements are required, but an implicit
    /// null return will be appended to the generated bytecode.
    ///
    /// ```wren
    /// add(x, y) {
    ///   return x + y
    /// }
    /// ```
    fn compile_method_body(&mut self) -> WrenResult<()> {
        // Possibly empty or an expression body.
        let block_body_kind = self.compile_block_body()?;

        // TODO: Constructor

        if block_body_kind.is_statement() {
            // Implicitly return `null` in statement bodies.
            self.emit_op(Op::PushNull);
        }

        self.emit_op(Op::Return);

        Ok(())
    }

    /// Compiles a block body, including the outer braces `{ ... }`.
    fn compile_block_body(&mut self) -> WrenResult<BlockBodyKind> {
        debug_assert_eq!(self.token.kind(), Some(TokenKind::LeftBrace));

        self.next_token()?; // {
        println!("compile_block_body {:?}", self.token);

        // Empty blocks do nothing.
        if self.match_token(TokenKind::RightBrace)? {
            return Ok(BlockBodyKind::Empty);
        }

        // If there's no line after the `{` , it's a single-expression body.
        if !self.match_token(TokenKind::Newline)? {
            self.compile_expr()?;
            self.consume_token(TokenKind::RightBrace)?;
            println!("compile_block_body [done expr] {:?}", self.token);
            return Ok(BlockBodyKind::Expr);
        }

        // Empty blocks only containing newlines do nothing.
        self.ignore_newlines()?;
        if self.match_token(TokenKind::RightBrace)? {
            println!("compile_block_body [done empty] {:?}", self.token);
            return Ok(BlockBodyKind::Empty);
        }

        // Block body is made up of definition statements.
        loop {
            self.compile_def_stmt()?;
            self.expect_end_of_statement()?;

            if self.token.kind() == Some(TokenKind::RightBrace) {
                break;
            }
        }
        self.consume_token(TokenKind::RightBrace)?;

        println!("compile_block_body [done stmt] {:?}", self.token);
        Ok(BlockBodyKind::Stmt)
    }

    /// Compile a foreign class definition.
    ///
    /// A foreign class has no fields in Wren,
    /// because it has a sibling struct in Rust.
    fn compile_foreign(&mut self) -> WrenResult<()> {
        self.consume_token(TokenKind::Keyword(KeywordKind::Foreign))?;
        self.compile_class(true)
    }

    fn compile_import(&mut self) -> WrenResult<()> {
        todo!()
    }

    /// Compile a "var" variable definition statement.
    fn compile_var_def(&mut self) -> WrenResult<()> {
        println!("compile_var_def");
        debug_assert_eq!(self.token.keyword(), Some(KeywordKind::Var));
        self.next_token()?; // var

        // Consume its name token, but don't declare it yet.
        // A local variable should not be in scope in its own initializer.
        let name_token = self.take_token()?;
        self.next_token()?; // name

        // The right-hand-side is optional, defaulting the
        // variable to null if it's omitted.
        if self.token.kind() == Some(TokenKind::Eq) {
            self.next_token()?; // =
            self.ignore_newlines()?;

            // Variable initializer expression.
            self.compile_expr()?;
        } else {
            self.emit_op(Op::PushNull);
        }

        // Terminate statement.
        self.expect_end_of_statement()?;

        // Now put the symbol in scope.
        let var = self.declare_variable(&name_token)?;
        self.emit_store_mod_var(var.id.symbol().unwrap());
        Ok(())
    }

    /// Compile a `return` simple statement.
    fn compile_return(&mut self) -> WrenResult<()> {
        debug_assert_eq!(self.token.keyword(), Some(KeywordKind::Return));

        self.next_token()?; // return

        match self.token.kind() {
            Some(TokenKind::Newline | TokenKind::End) | None => {
                // TODO: If there's no expression after return, initializers should return 'this' and regular methods should return null.
                Ok(())
            }
            _ => {
                // TODO: A constructor cannot return a value.
                self.compile_expr()?;
                self.emit_op(Op::Return);
                Ok(())
            }
        }
    }

    /// Emit instructions to store a module scoped variable.
    ///
    /// The value at the top of the stack is copied into the module's variable table,
    /// then the top of the stack is popped.
    fn emit_store_mod_var(&mut self, symbol: SymbolId) {
        // Module variables are stored in their own table.
        // The top stack value is temporary.
        self.emit_op(Op::StoreModVar(symbol));
        self.emit_op(Op::Pop);
    }
}

/// Kind of method signature.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum SignatureKind {
    /// A normal method signature, with a name and parameters.
    Method,
    /// Method without parameters.
    ///
    /// ```wren
    /// x { _x }
    /// ```
    Getter,
    /// Method with an equals after its name.
    ///
    /// ```wren
    /// x=(a) { _x = a }
    /// ```
    Setter,
    /// Method with square brackets and no name.
    ///
    /// ```wren
    /// [i] { _data[i] }
    /// ```
    Subscript,
    /// Method with square brackets, an equals and parameter list.
    ///
    /// ```wren
    /// [i]=(a) { _data[i] = a }
    /// ```
    SubscriptSetter,
    /// Constructor is like a normal method preceded by the `construct` keyword.
    ///
    /// ```wren
    /// construct new(a, b, c) {
    ///   _a = a
    ///   _b = b
    ///   _c = c
    /// }
    /// ```
    Initializer,
}

/// Method signature.
#[derive(Debug)]
struct Signature {
    name: String,
    kind: SignatureKind,
    arity: Arity,
}

impl Signature {
    fn new(name: impl ToString, kind: SignatureKind) -> WrenResult<Self> {
        let name = name.to_string();
        if name.len() > MAX_METHOD_NAME {
            return Err(WrenError::new_compile(CompileError::MaxMethodName));
        }

        Ok(Self {
            name,
            kind,
            arity: Arity::new(0),
        })
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use SignatureKind::*;

        let Self { name, kind, arity } = &self;

        match kind {
            Method => {
                write!(f, "{name}")?;
                write!(f, "(")?;
                fmt_param_list(f, arity.as_usize())?;
                write!(f, ")")?;
            }
            Getter => {
                write!(f, "{name}")?;
            }
            Setter => {
                write!(f, "{name}")?;
                write!(f, "=(")?;
                fmt_param_list(f, 1)?;
                write!(f, ")")?;
            }
            Subscript => {
                write!(f, "{name}")?;
                write!(f, "[")?;
                fmt_param_list(f, arity.as_usize())?;
                write!(f, "]")?;
            }
            SubscriptSetter => {
                write!(f, "{name}")?;
                write!(f, "[")?;
                fmt_param_list(f, arity.as_usize() - 1)?;
                write!(f, "]")?;
                write!(f, "=(")?;
                fmt_param_list(f, 1)?;
                write!(f, ")")?;
            }
            // Uses keyword `construct` in actual Wren script,
            // but `init` in the internal signature map.
            Initializer => {
                write!(f, "init ")?;
                write!(f, "{name}")?;
                write!(f, "(")?;
                fmt_param_list(f, arity.as_usize())?;
                write!(f, ")")?;
            }
        }

        Ok(())
    }
}

/// Writes a parameter list to the given formatter.
///
/// ```text
/// num_args(_, 3) -> "_,_,_"
/// ```
fn fmt_param_list(f: &mut fmt::Formatter, num_args: usize) -> fmt::Result {
    let num_params = num_args.min(MAX_PARAMETERS);

    for i in 0..num_params {
        if i > 0 {
            write!(f, ",")?;
        }
        write!(f, "_")?;
    }

    Ok(())
}

/// Functions for compiling method signatures.
impl<'src> WrenCompiler<'src> {
    /// Create a method signature using the current token.
    fn create_signature(&self, kind: SignatureKind) -> WrenResult<Signature> {
        let token = self.try_token()?;
        let fragment = token.fragment(self.lexer.source());

        Signature::new(fragment, kind)
    }

    /// Compile a method signature.
    ///
    /// This is the entry point for all method types. Here we will
    /// dispatch to the appropriate operator signatures.
    ///
    /// The current token in the compiler must be the name, or operator,
    /// of the method definition. Keywords like `foreign` and `static`
    /// must already be consumed.
    ///
    /// ```wren
    /// class Game {
    ///
    ///   ┌─ start here
    ///   │
    ///   update() {
    ///     ...
    ///   }
    ///
    ///                  ┌─ start here
    ///                  │
    ///   foreign static +(other)
    /// }
    /// ```
    fn compile_signature(&mut self, sig: &mut Signature) -> WrenResult<()> {
        use TokenKind::*;
        println!("compile_signature {:?}", self.token);

        match self.try_token()?.kind {
            Name => self.compile_signature_named(sig),
            Plus => {
                todo!()
            }
            Minus => {
                todo!()
            }
            Eq => {
                todo!()
            }
            LeftBracket => {
                todo!()
            }
            _ => panic!("expected method definition"),
        }
    }

    /// Getters, setters and named method signatures, with or without parameters.
    fn compile_signature_named(&mut self, sig: &mut Signature) -> WrenResult<()> {
        debug_assert_eq!(self.token.kind(), Some(TokenKind::Name));
        println!("compile_signature_named {:?}", self.token);

        sig.kind = SignatureKind::Getter;
        self.consume_token(TokenKind::Name)?;

        // Setter
        if self.match_token(TokenKind::Eq)? {
            sig.kind = SignatureKind::Setter;
        }

        self.compile_method_parameters(sig)
    }

    /// Updates the `kind` and `arity` of the given `sig` argument.
    fn compile_method_parameters(&mut self, sig: &mut Signature) -> WrenResult<()> {
        // Method parameters are optional.
        if !self.match_token(TokenKind::LeftParen)? {
            return Ok(());
        }

        sig.kind = SignatureKind::Method;

        // Allow newline before an empty parameter list.
        self.ignore_newlines()?;

        // Allow an empty parameter list.
        if self.match_token(TokenKind::RightParen)? {
            sig.arity = Arity::new(0);
            return Ok(());
        }

        self.compile_parameter_list(sig)?;
        self.consume_token(TokenKind::RightParen)?;

        Ok(())
    }

    fn compile_parameter_list(&mut self, sig: &mut Signature) -> WrenResult<()> {
        debug_assert!(
            self.scope_depth != ScopeDepth::Module,
            "parameters must be compiled in a method's scope"
        );

        loop {
            self.ignore_newlines()?;
            sig.arity = Arity::new(sig.arity.as_u8() + 1);

            if sig.arity.as_usize() > MAX_PARAMETERS {
                return Err(WrenError::new_compile(CompileError::MaxParameters));
            }

            let token = self.consume_token(TokenKind::Name)?;
            self.declare_variable(&token)?;

            if !self.match_token(TokenKind::Comma)? {
                break;
            }
        }

        Ok(())
    }
}

// -------------------------------------------------------------------------------------------------
// Expression parser.

/// Token precedence.
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
#[rustfmt::skip]
enum Precedence {
    /// Tokens that terminate an expression
    /// should have a precedence of `None`.
    None = 0,
    Lowest = 1,
    Assignment = 2,
    // =
    Conditional = 3,
    // ?:
    LogicalOr = 4,
    // ||
    LogicalAnd = 5,
    // &&
    Equality = 6,
    // == !=
    Is = 7,
    // is
    Comparison = 8,
    // < > <= >=
    BitwiseOr = 9,
    // |
    BitwiseXor = 10,
    // ^
    BitwiseAnd = 11,
    // &
    BitwiseShift = 12,
    // << >>
    Range = 13,
    // .. ...
    Term = 14,
    // + -
    Factor = 15,
    // * / %
    Unary = 16,
    // - ! ~
    Call = 17,
    // . () []
    Primary = 18,
}

impl Precedence {
    #[inline(always)]
    fn as_i32(&self) -> i32 {
        *self as i32
    }

    /// Get the precedence of the given token type in the context
    /// of the expression parser.
    #[rustfmt::skip]
    fn of(kind: TokenKind) -> Precedence {
        use TokenKind::*;

        match kind {
            Number | Name => Precedence::Lowest,
            Plus | Minus => Precedence::Term,
            Star | Slash => Precedence::Factor,
            Eq => Precedence::Assignment,
            EqEq | BangEq => Precedence::Equality,

            Dot | LeftParen | LeftBracket => Precedence::Call,

            // Terminators
            RightParen | RightBracket => Precedence::None,
            Comma => Precedence::None,
            _ => Precedence::None,
        }
    }
}

impl TryFrom<i32> for Precedence {
    type Error = Infallible;

    #[rustfmt::skip]
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        use Precedence as P;
        match value {
            0 => Ok(P::None),
            1 => Ok(P::Lowest),
            2 => Ok(P::Assignment),
            3 => Ok(P::Conditional),
            4 => Ok(P::LogicalOr),
            5 => Ok(P::LogicalAnd),
            6 => Ok(P::Equality),
            7 => Ok(P::Is),
            8 => Ok(P::Comparison),
            9 => Ok(P::BitwiseOr),
            10 => Ok(P::BitwiseXor),
            11 => Ok(P::BitwiseAnd),
            12 => Ok(P::BitwiseShift),
            13 => Ok(P::Range),
            14 => Ok(P::Term),
            15 => Ok(P::Factor),
            16 => Ok(P::Unary),
            17 => Ok(P::Call),
            18 => Ok(P::Primary),
            _ => Ok(P::None),
        }
    }
}

impl fmt::Display for Precedence {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        std::fmt::Display::fmt(&self.as_i32(), f)
    }
}

impl std::ops::Add<i32> for Precedence {
    type Output = Precedence;

    fn add(self, rhs: i32) -> Self::Output {
        Precedence::try_from(self.as_i32() + rhs).unwrap()
    }
}

/// Associativity is the precedence tie-breaker.
#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Associativity {
    Left,
    Right,
}

impl Associativity {
    /// Determine the associativity of the given token kind.
    fn of(token_ty: TokenKind) -> Associativity {
        if token_ty == TokenKind::Eq {
            Associativity::Right
        } else {
            Associativity::Left
        }
    }

    fn is_left(&self) -> bool {
        *self == Associativity::Left
    }
}

/// Functions that form the Pratt-parser.
impl<'src> WrenCompiler<'src> {
    fn compile_expr(&mut self) -> WrenResult<()> {
        println!("compile_expr");
        self.compile_expr_precedence(Precedence::Lowest)
    }

    fn compile_expr_precedence(&mut self, precedence: Precedence) -> WrenResult<()> {
        println!("compile_expr_precedence {:?} {precedence:?}", self.try_token());
        let kind = self.try_token()?.kind;

        // TODO: Handle assignment expression.
        self.compile_prefix()?;

        println!(
            "infix token: {:?}; precedence {:?} <= {:?}",
            self.try_token()?,
            precedence,
            Precedence::of(self.try_token()?.kind)
        );
        while precedence <= Precedence::of(self.try_token()?.kind) {
            println!("infix loop for {kind:?} {precedence:?}");
            self.compile_infix()?;
        }

        println!("compile_expr_precedence return");
        Ok(())
    }

    fn compile_prefix(&mut self) -> WrenResult<()> {
        use TokenKind::*;
        let token = self.try_token()?;
        println!("prefix token: {:?}", token.kind);

        match token.kind {
            Number => self.compile_number_literal(),
            _ => todo!("compile_prefix"),
        }
    }

    fn compile_number_literal(&mut self) -> WrenResult<()> {
        debug_assert_eq!(self.token.kind(), Some(TokenKind::Number));
        let token = self.try_token()?;
        println!("compile_number_literal {:?}", token);
        let value = token.value.num().unwrap().into();
        let constant_id = self.func.func.intern_constant(value);
        self.emit_op(Op::Constant(constant_id));
        self.next_token()?;
        Ok(())
    }

    fn compile_infix(&mut self) -> WrenResult<()> {
        let kind = self.try_token()?.kind;
        let precedence = Precedence::of(kind);
        println!("compile_infix token {kind:?}");

        // An infix operator cannot end an expression.
        self.ignore_newlines()?;

        // Compile the right-hand side.
        self.next_token()?;
        self.compile_expr_precedence(precedence + 1)?;

        // Call the operator on the left-hand side's class.
        let method_name =
            token_method_name(kind).ok_or_else(|| WrenError::new_compile(CompileError::InvalidOperator))?;
        let signature = Signature {
            name: method_name.to_string(),
            kind: SignatureKind::Method,
            arity: Arity::new(1),
        };
        self.compile_call_signature(signature)
    }

    fn compile_call_signature(&mut self, signature: Signature) -> WrenResult<()> {
        println!("compile_call_signature {:?}", signature);
        let symbol = self
            .method_names
            .borrow()
            .resolve(signature.name.as_str())
            .ok_or_else(|| WrenError::new_compile(CompileError::SymbolNotFound))?;
        self.emit_op(Op::Call(signature.arity, symbol));
        Ok(())
    }
}

/// Determine the method signature for the given token.
///
/// This is for mapping unary and binary operators to their method calls.
fn token_method_name(kind: TokenKind) -> Option<&'static str> {
    use TokenKind::*;
    println!("token_method_name({kind:?})");
    match kind {
        Plus => Some("+(_)"),
        Minus => Some("-(_)"),
        Slash => Some("/(_)"),
        Star => Some("*(_)"),
        _ => None,
    }
}

// -------------------------------------------------------------------------------------------------
/// Functions that emit bytecode instructions.
impl<'src> WrenCompiler<'src> {
    /// Emit the given instruction to the current function.
    fn emit_op(&mut self, op: Op) {
        self.func.func.push_op(op, 0);
    }

    /// Emit an instruction with the intent of replacing it later.
    ///
    /// Returns the index in the bytecode where the instruction is located,
    /// which can be used later in a call to [`Self::patch_op`].
    fn reserve_op(&mut self, op: Op) -> usize {
        let index = self.func.func.code.len();
        self.func.func.push_op(op, 0);
        index
    }

    /// Replace the instruction located at `index` with the given instruction.
    fn patch_op(&mut self, index: usize, op: Op) {
        self.func.func.code[index] = op;
    }

    /// Emit an instruction to load the given constant onto the fiber's stack.
    fn emit_constant(&mut self, constant: Value) {
        let constant_id = self.func.func.intern_constant(constant);
        // TODO: Line number from lexer
        self.func.func.push_op(Op::Constant(constant_id), 0);
    }

    /// Emit the code to load a variable onto the stack.
    fn emit_load_var(&mut self, var: &Variable) {
        match var.scope {
            Scope::Local => self.emit_op(Op::PushLocal(var.id.local_slot().unwrap())),
            Scope::UpValue => {
                todo!()
            }
            Scope::Module => {
                self.emit_op(Op::LoadModVar(var.id.symbol().unwrap()));
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::value::new_handle;

    #[test]
    fn test_expr() {
        let module = new_handle(ObjModule::new("main"));
        let method_names = new_handle(SymbolTable::new());
        let compiler = WrenCompiler::new(module, "return 7", method_names);

        let _obj_fn = compiler.compile(false).unwrap();
    }

    #[test]
    fn test_var_def() {
        let module = new_handle(ObjModule::new("main"));
        let method_names = new_handle(SymbolTable::new());
        let compiler = WrenCompiler::new(module, "var x = 7", method_names);

        let obj_fn = compiler.compile(false).unwrap();
        println!("{:?}", obj_fn.borrow().code);
    }
}
