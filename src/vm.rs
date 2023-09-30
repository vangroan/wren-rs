use std::collections::HashMap;
use std::marker::PhantomData;

use crate::compiler::WrenCompiler;
use crate::error::{RuntimeError, WrenError, WrenResult};
use crate::opcode::Op;
use crate::value::{new_handle, CallFrame, FiberState, Handle, ObjClosure, ObjFiber, ObjFn, ObjModule, Pointer, Value};

pub type WrenWriteFn = fn(&mut WrenVm, message: &str);
pub type WrenErrorFn = fn(&mut WrenVm, message: &str, module_name: &str);

struct LoopState<'a> {
    stack: &'a mut Vec<Value>,
    constants: &'a [Value],
    frame: Pointer<CallFrame>,
    ops: &'a [Op],
    pc: usize,
    _borrowed: PhantomData<&'a mut CallFrame>,
}

impl<'a> LoopState<'a> {
    unsafe fn new(fiber: &'a mut ObjFiber) -> Self {
        // let ObjFiber { stack, frames, .. } = fiber;
        // let frame = frames.as_mut().map(Pointer::from_box).expect("fiber call stack is empty");
        //
        // // Keep a copy of the program counter to avoid a pointer jump
        // // to get to the call frame.
        // let pc = frame.pc;
        //
        // let func = frame.closure.borrow().func.borrow();
        // let constants = func.constants.as_slice();
        // let ops: &[Op] = &*(func.code.as_slice() as *const _);
        //
        // Self { stack, constants, frame, ops, pc, _borrowed: PhantomData }
        todo!()
    }

    #[inline(always)]
    fn get_op(&self) -> Op {
        // FIXME: Unwrap causes a jump
        self.ops.get(self.pc).cloned().unwrap_or(Op::End)
    }

    #[inline(always)]
    fn incr_pc(&mut self) {
        self.pc += 1;
    }

    #[inline(always)]
    fn save(&mut self) {
        self.frame.pc = self.pc;
    }
}

#[derive(Debug)]
enum FiberAction {
    Call,
    Try,
    Yield,
    Return(Value),
    Err(RuntimeError),
}

enum FuncAction {
    Call,
    CallForeign,
    Return(Value),
    FiberAction(FiberAction),
}

pub struct WrenVm {
    /// Map of module names to module objects.
    modules: HashMap<String, Handle<ObjModule>>,

    /// The currently running fiber.
    fiber: Option<Handle<ObjFiber>>,

    config: WrenConfig,
}

pub struct WrenConfig {
    pub write_fn: Option<WrenWriteFn>,
    pub error_fn: Option<WrenErrorFn>,
}

impl WrenVm {
    pub fn new(config: WrenConfig) -> Self {
        Self {
            modules: HashMap::new(),
            fiber: None,
            config,
        }
    }

    pub fn interpret(&mut self, module_name: &str, source: &str) -> WrenResult<Value> {
        let closure = new_handle(self.compile_in_module(module_name, source, false)?);
        let fiber = new_handle(ObjFiber::new(Some(closure))?);
        run_interpreter(self, fiber)
    }

    fn compile_in_module(&mut self, module_name: &str, source: &str, is_expression: bool) -> WrenResult<ObjClosure> {
        // let module: Option<ObjModule> = get_module(module_name)
        let module = match self.get_module(module_name) {
            Some(module) => module,
            None => {
                // TODO: Implicitly import core module
                // FIXME: Do we need to copy module name to a `Value`?
                new_handle(ObjModule::new(module_name))
            }
        };

        let func = self.compile(module, source, is_expression)?;

        // Functions are always wrapped in closures, to simplify VM implementation.
        let closure = ObjClosure::new(func);

        Ok(closure)
    }

    fn compile(&mut self, module: Handle<ObjModule>, source: &str, is_expression: bool) -> WrenResult<Handle<ObjFn>> {
        let mut compiler = WrenCompiler::new(source);
        compiler.compile(module, is_expression)
    }

    fn get_module(&self, module_name: &str) -> Option<Handle<ObjModule>> {
        self.modules.get(module_name).cloned()
    }
}

/// Execute top level interpreter loop.
fn run_interpreter(vm: &mut WrenVm, fiber: Handle<ObjFiber>) -> WrenResult<Value> {
    // Remember fiber
    fiber.borrow_mut().state = FiberState::Root;
    vm.fiber = Some(fiber.clone());

    loop {
        // Keep a pointer to the fiber to avoid the borrow bookkeeping.
        let fiber = &mut *fiber.borrow_mut();

        match run_fiber(fiber) {
            FiberAction::Call => todo!(),
            FiberAction::Try => todo!(),
            FiberAction::Yield => todo!(),
            FiberAction::Return(value) => {
                match fiber.caller.take() {
                    Some(caller) => {
                        // Pass return value to parent fiber.
                        caller.borrow_mut().stack.push(value);

                        // Return control back to parent fiber.
                        vm.fiber = Some(caller);

                        continue;
                    }
                    None => {
                        // Upstream Wren leave the fiber in the VM after return.
                        return Ok(value);
                    }
                }
            }
            FiberAction::Err(err) => {
                return Err(WrenError::new_runtime(err));
            }
        }
    }
}

/// Execute the given fiber.
fn run_fiber(fiber: &mut ObjFiber) -> FiberAction {
    loop {
        let ObjFiber { stack, frames, .. } = fiber;
        let frame = frames.last_mut().unwrap();

        match run_op_loop(stack, frame) {
            Ok(FuncAction::Call) => {
                // Setup call stack for next iteration.
                todo!()
            }
            Ok(FuncAction::CallForeign) => {
                // Call into Rust code
                // TODO: Is this where Fibers can call each other, or yield?
                todo!()
            }
            Ok(FuncAction::Return(value)) => {
                frames.pop();

                if frames.is_empty() {
                    // TODO: Close upvalues that are still in scope.
                    return FiberAction::Return(value);
                } else {
                    // Leave return value on the operand stack for the parent frame.
                    fiber.stack.push(value);
                    continue;
                }
            }
            Ok(FuncAction::FiberAction(action)) => {
                return action;
            }
            Err(err) => {
                return FiberAction::Err(err);
            }
        }
    }
}

/// Execute the instruction loop.
fn run_op_loop(stack: &mut Vec<Value>, frame: &mut CallFrame) -> Result<FuncAction, RuntimeError> {
    let mut pc = frame.pc;
    let func_rc = frame.closure.borrow().func.clone();
    let func = func_rc.borrow();
    let constants = func.constants.as_slice();
    let ops: &[Op] = func.code.as_slice();

    // Save the the loop state kept on the stack to the frame.
    macro_rules! save {
        ($frame:expr, $pc:expr) => {
            $frame.pc = $pc;
        };
    }

    loop {
        let op = ops.get(pc).cloned().unwrap();
        pc += 1;

        match op {
            Op::NoOp => {
                todo!()
            }
            Op::Constant(idx) => {
                stack.push(constants[idx as usize].clone());
            }
            Op::Return => {
                let value = stack.pop().unwrap_or_default();
                save!(frame, pc);
                return Ok(FuncAction::Return(value));
            }
            Op::EndModule => {
                todo!()
            }
            Op::End => {
                todo!()
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::value::*;

    fn write(_vm: &mut WrenVm, message: &str) {
        println!("{message}");
    }

    fn error(_vm: &mut WrenVm, message: &str, module_name: &str) {
        eprintln!("error [{module_name}] {message}")
    }

    fn create_test_config() -> WrenConfig {
        WrenConfig {
            write_fn: Some(write),
            error_fn: Some(error),
        }
    }

    #[test]
    fn test_basic() {
        let mut vm = WrenVm::new(create_test_config());

        // vm.interpret("main", r#"System.print("Hello, world!")"#).expect("interpret");
        // vm.interpret("main", r#"12345"#).expect("interpret");

        // Construct a closure as the compiler would
        let mut obj_fn = ObjFn::new();
        obj_fn.intern_constant(7.0.into());
        obj_fn.push_op(Op::Constant(0), 0);
        obj_fn.push_op(Op::Return, 0);
        obj_fn.push_op(Op::End, 0);

        let closure = ObjClosure::new(new_handle(obj_fn));

        let fiber = ObjFiber::new(Some(new_handle(closure))).unwrap();

        let value = run_interpreter(&mut vm, new_handle(fiber)).unwrap();
        println!("Return value: {value:?}");
    }
}
