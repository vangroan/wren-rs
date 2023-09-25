//! Dynamically typed value.
use std::rc::Rc;

use crate::opcode::Op;

#[derive(Debug)]
pub enum Value {
    Null,
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
#[derive(Default)]
pub(crate) struct FnDebug {
    pub(crate) lines: Vec<usize>,
}

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

pub(crate) struct ObjClosure {
    func: Rc<ObjFn>,
}

impl ObjClosure {
    pub(crate) fn new(func: Rc<ObjFn>) -> Self {
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
    closure: ObjClosure,

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
    state: FiberState,
}
