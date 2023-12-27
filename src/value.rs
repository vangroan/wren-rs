//! Dynamically typed value.
use std::cell::RefCell;
use std::fmt::{self, Formatter};
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
use std::rc::Rc;

use crate::error::{CompileError, RuntimeError, WrenError, WrenResult};
use crate::limits::*;
use crate::opcode::Op;
use crate::primitive::PrimitiveFn;
use crate::symbol::SymbolTable;
use crate::SymbolId;

/// A reference counted handle with runtime interior mutability.
///
/// Bad for performance, but good enough to get the VM working
/// until we can implement proper garbage collection.
pub type Handle<T> = Rc<RefCell<T>>;

pub fn new_handle<T: 'static>(obj: T) -> Handle<T> {
    Rc::new(RefCell::new(obj))
}

pub struct Pointer<T: ?Sized>(NonNull<T>);

impl<T> Pointer<T> {
    /// Create a new pointer.
    ///
    /// Returns `None` if the given pointer is null.
    pub(crate) fn from_ptr(ptr: *mut T) -> Option<Self> {
        NonNull::new(ptr).map(Pointer)
    }

    pub(crate) fn from_non_null(ptr: NonNull<T>) -> Self {
        Pointer(ptr)
    }

    pub(crate) fn from_box(boxed: &mut Box<T>) -> Self {
        let box_ptr: *mut T = &mut **boxed;
        Pointer::from_ptr(box_ptr).unwrap()
    }

    pub(crate) fn as_ptr(&mut self) -> *mut T {
        self.0.as_ptr()
    }
}

impl<T> Deref for Pointer<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // SAFETY:
        //   Pointer must contain a valid pointer.
        unsafe { self.0.as_ref() }
    }
}

impl<T> DerefMut for Pointer<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY:
        //   Pointer must contain a valid pointer.
        unsafe { self.0.as_mut() }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ConstantId(pub(crate) u16);

impl ConstantId {
    pub(crate) fn as_usize(self) -> usize {
        self.0 as usize
    }
}

impl From<u16> for ConstantId {
    fn from(value: u16) -> Self {
        ConstantId(value)
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Null,
    True,
    False,
    Undefined,
    Num(f64),
    Str(String),
    Func(Handle<ObjFn>),
    Closure(Handle<ObjClosure>),
    Object,
    Class(Handle<ObjClass>),
}

impl Value {
    pub const fn from_num(num: f64) -> Self {
        Value::Num(num)
    }

    pub const fn is_num(&self) -> bool {
        matches!(self, Self::Num(_))
    }

    /// Attempt to cast the value to a number.
    pub fn try_num(&self) -> Result<f64, RuntimeError> {
        match self {
            Self::Num(num) => Ok(*num),
            _ => Err(RuntimeError::InvalidType),
        }
    }

    pub fn from_str(string: impl ToString) -> Self {
        Value::Str(string.to_string())
    }

    /// Attempt to cast the value to a string.
    pub fn try_str(&self) -> Result<&str, RuntimeError> {
        match self {
            Self::Str(string) => Ok(string.as_str()),
            _ => Err(RuntimeError::InvalidType),
        }
    }

    pub fn is_class(&self) -> bool {
        matches!(self, Self::Class(_))
    }

    pub fn try_class(&self) -> Result<&Handle<ObjClass>, RuntimeError> {
        match self {
            Self::Class(class) => Ok(class),
            _ => Err(RuntimeError::InvalidType),
        }
    }
}

impl From<Handle<ObjFn>> for Value {
    fn from(func: Handle<ObjFn>) -> Self {
        Value::Func(func)
    }
}

impl From<Handle<ObjClosure>> for Value {
    fn from(closure: Handle<ObjClosure>) -> Self {
        Value::Closure(closure)
    }
}

impl From<Handle<ObjClass>> for Value {
    fn from(class: Handle<ObjClass>) -> Self {
        Value::Class(class)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        Value::Num(value)
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Null
    }
}

impl PartialEq<Self> for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;

        match (self, other) {
            (Null, Null) => true,
            (True, True) => true,
            (False, False) => true,
            (Undefined, Undefined) => true,
            (Num(a), Num(b)) => *a == *b,
            (Closure(a), Closure(b)) => Handle::ptr_eq(a, b),
            (Class(a), Class(b)) => Handle::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Eq for Value {}

/// Identifies which specific type a heap-allocated object is.
#[derive(Debug)]
pub(crate) enum ObjType {
    Class,
    Closure,
    Fiber,
    Fn,
    Foreign,
    Instance,
    List,
    Map,
    Module,
    Range,
    String,
    Upvalue,
}

/// Common object header.
#[derive(Debug)]
pub(crate) struct Obj {
    kind: ObjType,
    // FIXME: Does every object have a metaclass? Can we git rid of this Option?
    pub(crate) class: Option<Handle<ObjClass>>,
}

#[derive(Debug)]
pub struct ObjModule {
    name: String,
    variables: Vec<Value>,
    var_names: SymbolTable,
}

impl ObjModule {
    pub fn new(name: impl ToString) -> Self {
        Self {
            name: name.to_string(),
            variables: Vec::new(),
            var_names: SymbolTable::new(),
        }
    }

    pub(crate) fn name(&self) -> &str {
        self.name.as_str()
    }

    pub(crate) fn var_count(&self) -> usize {
        debug_assert_eq!(self.variables.len(), self.var_names.len());
        self.variables.len()
    }

    pub(crate) fn variables(&self) -> &[Value] {
        self.variables.as_slice()
    }

    pub(crate) fn var_pairs(&self) -> impl Iterator<Item = (SymbolId, &str, &Value)> {
        self.var_names
            .pairs()
            .zip(self.variables.iter())
            .map(|((symbol, name), value)| (symbol, name, value))
    }

    /// Directly insert a variable into the module's symbol table, without scope checking
    /// or ensuring the name doesn't exist.
    pub(crate) fn insert_var(&mut self, name: impl ToString, value: Value) -> WrenResult<SymbolId> {
        let symbol_id = self.var_names.insert(name)?;
        debug_assert_eq!(
            symbol_id.as_usize(),
            self.variables.len(),
            "variable symbol table must correlate with the variable value buffer"
        );
        self.variables.push(value);
        Ok(symbol_id)
    }

    #[inline(always)]
    pub(crate) fn store_var(&mut self, symbol: SymbolId, value: Value) {
        self.variables[symbol.as_usize()] = value;
    }

    pub(crate) fn get_var(&mut self, symbol: SymbolId) -> Option<&Value> {
        self.variables.get(symbol.as_usize())
    }

    pub(crate) fn define_var(&mut self, name: impl ToString, value: Value) -> WrenResult<SymbolId> {
        if self.var_names.len() >= MAX_MODULE_VARS {
            return Err(WrenError::new_compile(CompileError::MaxModuleVariables));
        }

        let var_name = name.to_string();

        // Check if the variable is already implicitly declared.
        match self.var_names.resolve(var_name.as_str()) {
            None => {
                // Brand new variable.
                self.insert_var(name, value)
            }
            Some(symbol) => {
                // The variable already exists.
                let existing = &self.variables[symbol.as_usize()];

                // It could be a forward declared variable, in which
                // case the value holds the line number where the variable
                // was used before it was declared.
                if let Value::Num(line) = existing {
                    return Err(WrenError::new_compile(CompileError::ForwardVariable(
                        var_name,
                        *line as usize,
                    )));
                }

                return Err(WrenError::new_compile(CompileError::ModuleVariableExists(var_name)));
            }
        }
    }

    pub(crate) fn find_var(&self, var_name: &str) -> Option<&Value> {
        self.var_names
            .resolve(var_name)
            .and_then(|index| self.variables.get(index.as_usize()))
    }

    pub(crate) fn resolve_var(&self, var_name: &str) -> Option<SymbolId> {
        self.var_names.resolve(var_name)
    }

    /// Copy all of this module's variables to a vector.
    pub(crate) fn dump_vars(&self) -> Vec<(&str, Value)> {
        self.var_names.iter().zip(self.variables.iter().cloned()).collect()
    }
}

/// `ModuleDump` is a debug formatter that will display
/// the internals of a module object.
pub(crate) struct ModuleDump<'a> {
    module: &'a ObjModule,
}

impl<'a> ModuleDump<'a> {
    pub(crate) fn new(module: &'a ObjModule) -> Self {
        Self { module }
    }
}

impl<'a> fmt::Display for ModuleDump<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let Self { module } = *self;

        writeln!(f, "{}", module.name)?;
        for _ in 0..module.name.len() {
            write!(f, "=")?;
        }
        writeln!(f, "")?;

        writeln!(f, "Variables")?;
        writeln!(f, "---------")?;

        for (symbol, name, value) in module.var_pairs() {
            let index = symbol.as_u16();
            writeln!(f, "  {index:>6} {name:<12?}")?;
        }

        writeln!(f, "Classes")?;
        writeln!(f, "-------")?;

        let classes = module.var_pairs().filter(|(_, _, value)| value.is_class());
        for (symbol, name, value) in classes {
            // " {symbol:?} {name:?}"
            writeln!(f, " {symbol:?} {name:?}")?;

            let class_handle = value.try_class().cloned().unwrap();
            let class_obj = class_handle.borrow();
            write!(f, "class {}", class_obj.name)?;

            match &class_obj.super_class {
                Some(super_class) => {
                    writeln!(f, " is {}", super_class.borrow().name)?;
                }
                None => writeln!(f, "")?,
            }

            writeln!(f, "fields")?;
            // TODO: fields

            writeln!(f, "methods")?;
            assert!(class_obj.methods.len() < u16::MAX as usize);
            for (index, maybe_method) in class_obj.methods.iter().enumerate() {
                if let Some(method) = maybe_method {
                    writeln!(f, "  {index:>6} {method:?}")?;
                }
            }

            writeln!(f, "")?;
        }

        writeln!(f, "")?;

        Ok(())
    }
}

/// Debug information for a function object.
#[derive(Debug, Default)]
pub(crate) struct FnDebug {
    pub(crate) lines: Vec<usize>,
}

// TODO: Make ObjFn immutable, and give Compiler its own mutable function representation.
pub struct ObjFn {
    /// TODO: These can become `Box<[Op]>` after compile is complete
    pub(crate) code: Vec<Op>,
    pub(crate) constants: Vec<Value>,

    pub(crate) module: Handle<ObjModule>,

    // TODO: In upstream Wren this was turned into a pointer, for an unknown performance benefit. (smaller struct?)
    debug: FnDebug,
}

impl ObjFn {
    pub fn new(module: Handle<ObjModule>) -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            module,
            debug: FnDebug::default(),
        }
    }

    pub fn push_op(&mut self, op: Op, lineno: usize) {
        self.code.push(op);
        self.debug.lines.push(lineno);
    }

    /// Inserts the constant if it doesn't exist.
    ///
    /// Returns index of constant.
    pub(crate) fn intern_constant(&mut self, value: Value) -> ConstantId {
        if self.constants.len() >= MAX_CONSTANTS {
            panic!("maximum function constants reached");
        }

        // Scan the constant table.
        let found_index = self.constants.iter().position(|el| &value == el);
        let index = found_index.unwrap_or_else(|| {
            let index = self.constants.len();
            self.constants.push(value);
            index
        });

        ConstantId(index as u16)
    }

    #[inline(always)]
    pub fn get_op(&self, index: usize) -> Op {
        self.code.get(index).cloned().unwrap_or(Op::NoOp)
    }

    pub fn dump_opcodes(&self) -> OpcodeDump {
        OpcodeDump { func: self }
    }

    pub fn dump_constants(&self) -> ConstantDump {
        ConstantDump { func: self }
    }
}

impl fmt::Debug for ObjFn {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        // Circular reference between modules and functions.
        f.debug_struct("ObjFn")
            .field("code", &self.code)
            .field("constants", &self.constants)
            .field("module", &self.module.borrow().name)
            .finish_non_exhaustive()
    }
}

pub struct OpcodeDump<'a> {
    func: &'a ObjFn,
}

impl<'a> fmt::Display for OpcodeDump<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for (offset, op) in self.func.code.iter().enumerate() {
            writeln!(f, "{offset:>6} : {op:?}")?;
        }
        Ok(())
    }
}

pub struct ConstantDump<'a> {
    func: &'a ObjFn,
}

impl<'a> fmt::Display for ConstantDump<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for (id, op) in self.func.constants.iter().enumerate() {
            writeln!(f, "{id:>6} : {op:?}")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct ObjClosure {
    pub(crate) func: Handle<ObjFn>,
    // TODO: Upvalues
}

impl ObjClosure {
    pub(crate) fn new(func: Handle<ObjFn>) -> Self {
        Self { func }
    }
}

pub(crate) struct CallFrame {
    /// The program counter pointing to the next-to-be-executed
    /// instruction in the function's bytecode.
    ///
    /// Upstream Wren calls this `ip` (instruction pointer).
    pub(crate) pc: usize,

    /// The closure being executed.
    pub(crate) closure: Handle<ObjClosure>,

    /// Offset to the first stack slot used by this call frame. This will contain
    /// the receiver, followed by the function's parameters, then local variables
    /// and temporaries.
    pub(crate) start: usize,

    /// The previous frame on the stack.
    pub(crate) parent: Option<Box<CallFrame>>,
}

impl CallFrame {
    pub(crate) fn new(closure: Handle<ObjClosure>) -> Self {
        Self {
            pc: 0,
            closure,
            start: 0,
            parent: None,
        }
    }

    pub(crate) fn with_parent(closure: Handle<ObjClosure>, parent: Box<CallFrame>) -> Self {
        Self {
            parent: Some(parent),
            ..Self::new(closure)
        }
    }

    /// Increments the program counter and returns the new value.
    pub(crate) fn incr_pc(&mut self) -> usize {
        self.pc += 1;
        self.pc
    }
}

#[derive(Debug)]
pub(crate) enum StackError {
    Overflow,
    Underflow,
}

impl From<StackError> for RuntimeError {
    fn from(value: StackError) -> Self {
        use StackError::*;

        match value {
            Overflow => RuntimeError::StackOverflow,
            Underflow => RuntimeError::StackUnderflow,
        }
    }
}

pub(crate) struct CallStack<const MAX_FRAMES: usize> {
    // TODO: A lower level implementation using raw alloc
    data: Vec<CallFrame>,
}

impl<const MAX_FRAMES: usize> CallStack<MAX_FRAMES> {
    const MIN_CAPACITY: usize = 8;

    pub(crate) fn new() -> Self {
        Self { data: vec![] }
    }

    #[inline(always)]
    pub(crate) fn capacity(&self) -> usize {
        self.data.len()
    }

    pub(crate) fn push(&mut self, frame: CallFrame) {
        self.try_push(frame).expect("stack overflow")
    }

    pub(crate) fn pop(&mut self) -> Option<CallFrame> {
        Some(self.try_pop().expect("stack underflow"))
    }

    pub(crate) fn try_push(&mut self, frame: CallFrame) -> Result<(), StackError> {
        if self.data.len() >= MAX_FRAMES {
            return Err(StackError::Overflow);
        }

        self.data.push(frame);
        Ok(())
    }

    pub(crate) fn try_pop(&mut self) -> Result<CallFrame, StackError> {
        self.data.pop().ok_or(StackError::Underflow)
    }

    #[inline(always)]
    pub(crate) fn len(&self) -> usize {
        self.data.len()
    }

    #[inline(always)]
    pub(crate) fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

pub(crate) enum ObjUpvalue {
    Open(usize),
    Closed(Value),
}

// Tracks how a fiber has been invoked, aside from the ways that can be
// detected from the state of other fields in the fiber.
#[derive(Debug)]
pub(crate) enum FiberState {
    /// The fiber is being run from another fiber using a call to `try()`.
    Try,

    /// The fiber was directly invoked by [`WrenVm::interpret`]. This means it's the
    /// initial fiber used by a call to `wrenCall()` or `wrenInterpret()`.
    Root,

    /// The fiber is invoked some other way.
    Other,
}

pub(crate) struct ObjFiber {
    /// The operand stack slots.
    ///
    /// This is used for holding local variables and temporaries while the fiber is executing.
    pub(crate) stack: Vec<Value>,

    /// The stack of call frames.
    ///
    /// TODO: Does C Wren have a hardcoded maximum stack size?
    pub(crate) frames: Vec<CallFrame>,

    /// List of open upvalues that are still on the stack.
    upvalues: Vec<ObjUpvalue>,

    /// The fiber that ran this one. If this fiber is yielded, control will resume to our caller.
    pub(crate) caller: Option<Handle<ObjFiber>>,

    /// If the fiber failed because of a runtime error, this will contain the error object.
    /// Otherwise, it will be [`Value::Null`].
    error: Value,

    /// Tracks how this fiber has been invoked.
    pub(crate) state: FiberState,
}

impl ObjFiber {
    pub(crate) fn new(closure: Option<Handle<ObjClosure>>) -> Result<Self, RuntimeError> {
        let mut fiber = Self {
            stack: Vec::new(),
            frames: Vec::new(),
            upvalues: Vec::new(),
            caller: None,
            error: Value::Null,
            state: FiberState::Other,
        };

        if let Some(closure) = closure {
            fiber.push_call_frame(closure.clone(), 0)?;

            // The first slot always holds the closure.
            fiber.stack.push(closure.into());
        }

        Ok(fiber)
    }

    pub(crate) fn push_call_frame(&mut self, closure: Handle<ObjClosure>, start: usize) -> Result<(), StackError> {
        let mut frame = CallFrame::new(closure);
        frame.start = start;
        self.frames.push(frame);
        Ok(())
    }

    pub(crate) unsafe fn top_frame_ptr(&mut self) -> Option<Pointer<CallFrame>> {
        unimplemented!()
    }

    /// Remove and return the top frame of the call stack.
    ///
    /// Returns `None` when the stack is empty.
    pub fn pop_frame(&mut self) -> Option<CallFrame> {
        self.frames.pop()
    }

    pub(crate) fn top_frame(&self) -> Option<&CallFrame> {
        self.frames.last()
    }

    pub(crate) fn top_frame_mut(&mut self) -> Option<&mut CallFrame> {
        self.frames.last_mut()
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Method {
    /// A primitive native method implemented in Rust in the VM, which returns a [`Value`].
    ///
    /// Unlike foreign methods, this can directly manipulate the fiber's stack.
    PrimitiveValue(PrimitiveFn),

    /// A primitive that handles .call on Fn.
    FunctionCall,

    /// A externally-defined C method.
    Foreign,

    /// A normal user-defined method.
    Block,
}

impl Method {
    pub(crate) fn as_primitive_fn(&self) -> Option<PrimitiveFn> {
        match *self {
            Self::PrimitiveValue(primitive_fn) => Some(primitive_fn),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct ObjClass {
    pub(crate) obj: Obj,
    pub(crate) super_class: Option<Handle<ObjClass>>,

    // TODO: Method table can be immutable after compile.
    methods: Vec<Option<Method>>,

    pub(crate) is_foreign: bool,

    // The number of fields needed for an instance of this class,
    // including all of its superclass fields.
    pub(crate) num_fields: u8,

    // The name of the class.
    name: String,
}

impl ObjClass {
    pub(crate) fn new(name: impl ToString, num_fields: u8) -> Self {
        Self {
            obj: Obj {
                kind: ObjType::Class,
                class: None,
            },
            super_class: None,
            methods: Vec::new(),
            is_foreign: false,
            num_fields,
            name: name.to_string(),
        }
    }

    /// Grow the method table so it can include the given index.
    pub(crate) fn grow_method_table(&mut self, index: usize) {
        if index >= self.methods.len() {
            self.methods.extend((self.methods.len()..index + 1).map(|_| None));
        }
    }

    #[inline]
    pub(crate) fn bind_method(&mut self, symbol_id: SymbolId, method: Option<Method>) {
        let index = symbol_id.as_usize();
        self.grow_method_table(index);
        self.methods[index] = method;
    }

    pub(crate) fn get_method(&self, symbol_id: SymbolId) -> Option<&Method> {
        self.methods.get(symbol_id.as_usize()).map(|opt| opt.as_ref()).flatten()
    }

    pub(crate) fn iter_methods(&self) -> impl Iterator<Item = (SymbolId, Option<&Method>)> {
        assert!(
            self.methods.len() < MAX_SYMBOLS,
            "method symbols exceed limit {MAX_SYMBOLS}"
        );

        self.methods
            .iter()
            .enumerate()
            .map(|(index, method)| (SymbolId::new(index as u16), method.as_ref()))
    }

    pub(crate) fn method_len(&self) -> usize {
        self.methods.len()
    }

    pub(crate) fn bind_super_class(&mut self, super_class: Handle<ObjClass>) {
        if self.is_foreign {
            // FIXME: Can this happen in because of a script? Does it need to be RuntimeError?
            panic!("foreign class cannot inherit from a class with fields")
        }

        // Include the superclass in the total number of fields.
        self.num_fields += super_class.borrow().num_fields;

        // Inherit methods from its superclass.
        {
            let super_class_ref = super_class.borrow();
            let super_methods = super_class_ref.methods.as_slice();
            self.grow_method_table(if super_methods.is_empty() {
                0
            } else {
                super_methods.len() - 1
            });
            for (index, method) in super_methods.iter().enumerate() {
                // Bind method without guards, for performance.
                self.methods[index] = method.clone();
            }
        }

        self.super_class = Some(super_class);
    }

    pub(crate) fn unbind_super_class(&mut self) {
        self.super_class = None;
    }
}

#[derive(Debug)]
pub struct ObjInstance {}
