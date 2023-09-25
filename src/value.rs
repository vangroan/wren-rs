//! Dynamically typed value.
use crate::error::RuntimeError;
use std::cell::RefCell;
use std::rc::Rc;

use crate::opcode::Op;

/// A reference counted handle with runtime interior mutability.
///
/// Bad for performance, but good enough to get the VM working
/// until we can implement proper garbage collection.
pub type Handle<T: 'static> = Rc<RefCell<T>>;

pub fn new_handle<T: 'static>(obj: T) -> Handle<T> {
    Rc::new(RefCell::new(obj))
}

#[derive(Debug)]
pub enum Value {
    Null,
    Closure(Handle<ObjClosure>),
}

impl From<Handle<ObjClosure>> for Value {
    fn from(closure: Handle<ObjClosure>) -> Self {
        Value::Closure(closure)
    }
}

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

#[derive(Debug)]
pub(crate) struct ObjFn {
    /// TODO: These can become `Box<[Op]>` after compile is complete
    code: Vec<Op>,
    constants: Vec<Value>,

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
}

#[derive(Debug)]
pub struct ObjClosure {
    func: Handle<ObjFn>,
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
    pc: usize,

    /// The closure being executed.
    closure: Handle<ObjClosure>,

    /// Offset to the first stack slot used by this call frame. This will contain
    /// the receiver, followed by the function's parameters, then local variables
    /// and temporaries.
    start: usize,
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
    stack: Vec<Value>,

    /// The stack of call frames. This is a dynamic array that grows as needed but never shrinks.
    ///
    /// TODO: Does C Wren have a hardcoded maximum stack size?
    frames: CallStack<65535>,

    /// List of open upvalues that are still on the stack.
    upvalues: Vec<ObjUpvalue>,

    /// The fiber that ran this one. If this fiber is yielded, control will resume to our caller.
    caller: Option<Rc<ObjFiber>>,

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
            frames: CallStack::new(),
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
        let frame = CallFrame { pc: 0, closure, start };

        self.frames.try_push(frame)
    }

    pub(crate) fn top_frame(&self) -> Option<&CallFrame> {
        self.frames.data.last()
    }

    pub(crate) fn load_frame(&self) -> (Option<&CallFrame>, Option<Handle<ObjFn>>) {
        let frame = self.top_frame().unwrap();
        let func = frame.closure.borrow().func.clone();
        (Some(frame), Some(func))
    }
}
