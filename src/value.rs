//! Dynamically typed value.
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
use std::rc::Rc;

use crate::error::RuntimeError;
use crate::limits::MAX_CONSTANTS;
use crate::opcode::Op;

/// A reference counted handle with runtime interior mutability.
///
/// Bad for performance, but good enough to get the VM working
/// until we can implement proper garbage collection.
pub type Handle<T: 'static> = Rc<RefCell<T>>;

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
pub(crate) struct ConstantId(pub(crate) u16);

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
    Closure(Handle<ObjClosure>),
    Object,
    Class,
}

impl From<Handle<ObjClosure>> for Value {
    fn from(closure: Handle<ObjClosure>) -> Self {
        Value::Closure(closure)
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
            (Closure(a), Closure(b)) => Rc::ptr_eq(a, b),
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

pub(crate) struct ObjModule {
    name: String,
}

impl ObjModule {
    pub fn new(name: impl ToString) -> Self {
        Self { name: name.to_string() }
    }
}

/// Debug information for a function object.
#[derive(Debug, Default)]
pub(crate) struct FnDebug {
    pub(crate) lines: Vec<usize>,
}

// TODO: Make ObjFn immutable, and give Compiler its own mutable function representation.
#[derive(Debug)]
pub(crate) struct ObjFn {
    /// TODO: These can become `Box<[Op]>` after compile is complete
    pub(crate) code: Vec<Op>,
    pub(crate) constants: Vec<Value>,

    // TODO: In upstream Wren this was turned into a pointer, for an unknown performance benefit. (smaller struct?)
    debug: FnDebug,
}

impl ObjFn {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
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
        let index = match found_index {
            Some(index) => index,
            None => {
                let index = self.constants.len();
                self.constants.push(value);
                index
            }
        };

        ConstantId(index as u16)
    }

    #[inline(always)]
    pub fn get_op(&self, index: usize) -> Op {
        self.code.get(index).cloned().unwrap_or(Op::NoOp)
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
