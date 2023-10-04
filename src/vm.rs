use std::collections::HashMap;
use std::marker::PhantomData;

use crate::compiler::WrenCompiler;
use crate::core::{initialize_core, load_core_module, teardown_core, BuiltIns};
use crate::error::{RuntimeError, WrenError, WrenResult};
use crate::opcode::Op;
use crate::symbol::SymbolTable;
use crate::value::{
    new_handle, CallFrame, FiberState, Handle, Method, ObjClass, ObjClosure, ObjFiber, ObjFn, ObjModule, Pointer, Value,
};
use crate::SymbolId;

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
    pub(crate) modules: HashMap<String, Handle<ObjModule>>,

    pub(crate) method_names: SymbolTable,

    pub(crate) builtins: Option<BuiltIns>,

    /// The currently running fiber.
    fiber: Option<Handle<ObjFiber>>,

    last_module: Option<Handle<ObjModule>>,

    config: WrenConfig,
}

pub struct WrenConfig {
    pub write_fn: Option<WrenWriteFn>,
    pub error_fn: Option<WrenErrorFn>,
}

impl WrenVm {
    pub fn new(config: WrenConfig) -> Self {
        let mut vm = Self {
            modules: HashMap::new(),
            method_names: SymbolTable::new(),
            // TODO: Figure out how to avoid paying for the Option unwrap in the interpreter loop.
            builtins: None,
            fiber: None,
            last_module: None,
            config,
        };

        // Important: Builtins must be initiated by the core module.
        initialize_core(&mut vm).expect("initializing core module");

        vm
    }

    pub fn config(&self) -> &WrenConfig {
        &self.config
    }

    pub fn interpret(&mut self, module_name: &str, source: &str) -> WrenResult<Value> {
        let closure = new_handle(self.compile_in_module(module_name, source, false)?);
        println!("ops: {:#?}", closure.borrow().func.borrow().code.as_slice());

        let fiber = new_handle(ObjFiber::new(Some(closure)).map_err(WrenError::new_runtime)?);
        run_interpreter(self, fiber)
    }

    fn compile_in_module(&mut self, module_name: &str, source: &str, is_expression: bool) -> WrenResult<ObjClosure> {
        // let module: Option<ObjModule> = get_module(module_name)
        let module = match self.get_module(module_name) {
            Some(module) => module,
            None => {
                println!("Creating new module");

                // FIXME: Do we need to copy module name to a `Value`?
                let new_module = new_handle(ObjModule::new(module_name));

                // Implicitly import core module into top-level scope.
                load_core_module(self, &mut *new_module.borrow_mut())?;

                self.modules.insert(module_name.to_string(), new_module.clone());
                new_module
            }
        };

        let func = self.compile(module, source, is_expression)?;

        // Functions are always wrapped in closures, to simplify VM implementation.
        let closure = ObjClosure::new(func);

        Ok(closure)
    }

    fn compile(&mut self, module: Handle<ObjModule>, source: &str, is_expression: bool) -> WrenResult<Handle<ObjFn>> {
        let mut compiler = WrenCompiler::new(module, source, &mut self.method_names);
        compiler.compile(is_expression)
    }

    fn get_module(&self, module_name: &str) -> Option<Handle<ObjModule>> {
        self.modules.get(module_name).cloned()
    }

    #[inline(always)]
    fn class_by_value(&self, value: &Value) -> Handle<ObjClass> {
        // FIXME: Get rid of Option
        let core = self.builtins.as_ref().unwrap();

        match value {
            Value::Num(_) => core.num_class.clone(),
            _ => todo!("class_by_value"),
        }
    }
}

impl Drop for WrenVm {
    fn drop(&mut self) {
        // Ensure the circular `Rc` web of the object model is unlinked
        // so the memory doesn't leak.
        teardown_core(self);
    }
}

// ============================================================================
// Interpreter

/// Execute top level interpreter loop.
fn run_interpreter(vm: &mut WrenVm, fiber: Handle<ObjFiber>) -> WrenResult<Value> {
    // Remember fiber
    fiber.borrow_mut().state = FiberState::Root;
    vm.fiber = Some(fiber.clone());

    loop {
        // Keep a pointer to the fiber to avoid the borrow bookkeeping.
        let fiber = &mut *fiber.borrow_mut();

        match run_fiber(vm, fiber) {
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
fn run_fiber(vm: &mut WrenVm, fiber: &mut ObjFiber) -> FiberAction {
    loop {
        // let ObjFiber { stack, frames, .. } = fiber;
        // Remove the current frame so we can mutably borrow it without conflicting with fiber.
        let mut frame = fiber.frames.pop().unwrap();

        match run_op_loop(vm, fiber, &mut frame) {
            Ok(FuncAction::Call) => {
                // Create the new frame.
                let mut temp_frame: CallFrame = todo!();

                // Put the current frame back onto the stack.
                std::mem::swap(&mut frame, &mut temp_frame);

                // Setup call stack for next iteration.
                fiber.frames.push(temp_frame);

                todo!()
            }
            Ok(FuncAction::CallForeign) => {
                // Call into Rust code
                // TODO: Is this where Fibers can call each other, or yield?
                todo!()
            }
            Ok(FuncAction::Return(value)) => {
                // Note: We leave the frame off the call stack, so no explicit pop is needed.

                if fiber.frames.is_empty() {
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
fn run_op_loop(vm: &mut WrenVm, fiber: &mut ObjFiber, frame: &mut CallFrame) -> Result<FuncAction, RuntimeError> {
    let mut pc = frame.pc;
    let func_rc = frame.closure.borrow().func.clone();
    let func = func_rc.borrow();
    let module_rc = func.module.clone();
    let mut module = module_rc.borrow_mut();
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

        if cfg!(feature = "trace_opcodes") {
            let prev_pc = pc - 1;
            println!("{prev_pc:06X} : {op:?}");
        }

        match op {
            Op::NoOp => {
                todo!()
            }
            Op::Call(arity, symbol) => {
                // Add one for the implicit receiver argument.
                let num_args = arity.as_usize() + 1;
                let recv_offset = fiber.stack.len() - num_args;
                let receiver = &fiber.stack[recv_offset];
                let obj_class_handle = vm.class_by_value(receiver);
                let obj_class = &*obj_class_handle.borrow();
                match call_method(vm, fiber, obj_class, symbol, num_args)? {
                    FuncAction::Return(value) => {
                        fiber.stack.truncate(recv_offset + 1);
                        fiber.stack[recv_offset] = value;
                    }
                    action => {
                        return Ok(action);
                    }
                }
            }
            Op::Constant(id) => {
                fiber.stack.push(constants[id.as_usize()].clone());
            }
            Op::PushNull => fiber.stack.push(Value::Null),
            Op::StoreModVar(symbol) => {
                if let Some(value) = fiber.stack.last() {
                    module.store_var(symbol, value.clone());
                }

                // TODO: Performance - Commonly followed by Pop.
            }
            Op::LoadModVar(symbol) => {
                let value = module.get_var(symbol).cloned().unwrap_or(Value::Null);
                fiber.stack.push(value);
            }
            Op::Pop => {
                fiber.stack.pop();
            }
            Op::Return => {
                let value = fiber.stack.pop().unwrap_or_default();
                save!(frame, pc);
                return Ok(FuncAction::Return(value));
            }
            Op::Class(_field_num) => {
                todo!()
            }
            Op::ForeignClass => {
                todo!()
            }
            Op::EndModule => {
                fiber.stack.push(Value::Null);
                vm.last_module = Some(func.module.clone());
            }
            Op::End => {
                todo!()
            }
        }
    }
}

fn call_method(
    vm: &mut WrenVm,
    fiber: &ObjFiber,
    obj_class: &ObjClass,
    symbol: SymbolId,
    num_args: usize,
) -> Result<FuncAction, RuntimeError> {
    let method = obj_class.get_method(symbol).ok_or_else(|| RuntimeError::MethodNotFound)?;

    match method {
        // Call to native Rust function through a safe pointer.
        Method::PrimitiveValue(fn_ptr) => {
            let offset = fiber.stack.len() - num_args;
            let args = &fiber.stack[offset..];

            fn_ptr(vm, args).map(FuncAction::Return)
        }
        Method::FunctionCall => todo!("function call"),
        Method::Foreign => todo!("foreign call"),
        Method::Block => todo!("block call"),
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
        let module = new_handle(ObjModule::new("main"));

        // Construct a closure as the compiler would
        let mut obj_fn = ObjFn::new(module);
        obj_fn.intern_constant(7.0.into());
        obj_fn.push_op(Op::Constant(0.into()), 0);
        obj_fn.push_op(Op::Return, 0);
        obj_fn.push_op(Op::End, 0);

        let closure = ObjClosure::new(new_handle(obj_fn));

        let fiber = ObjFiber::new(Some(new_handle(closure))).unwrap();

        let value = run_interpreter(&mut vm, new_handle(fiber)).unwrap();
        println!("Return value: {value:?}");
    }

    #[test]
    fn test_with_compile() {
        let mut vm = WrenVm::new(create_test_config());

        let value = vm.interpret("main", r"return 745635").expect("interpret");
        println!("Return value: {value:?}");
    }

    #[test]
    fn test_num_add() {
        let mut vm = WrenVm::new(create_test_config());

        let value = vm.interpret("main", r"return 7 + 11 + 42").expect("interpret");
        println!("Return value: {value:?}");

        assert_eq!(value, Value::from_num(60.0))
    }

    #[test]
    fn test_num_arithmetic() {
        let mut vm = WrenVm::new(create_test_config());

        let value = vm.interpret("main", r"return 1 + 2 - 3 / 4 * 5").expect("interpret");
        println!("Return value: {value:?}");

        assert_eq!(value, Value::from_num(-0.75))
    }

    #[test]
    fn test_var_def() {
        let mut vm = WrenVm::new(create_test_config());

        let value = vm
            .interpret(
                "main",
                r#"
        var x = 7
        var y = 1 + 2 - 3 / 4 * 5
        return
        "#,
            )
            .expect("interpret");
        println!("Return value: {value:?}");

        let module = vm.modules.get("main").unwrap();
        println!("'{}' variables:", module.borrow().name());
        for (name, value) in module.borrow().dump_vars() {
            println!("{name:>12}: {value:?}");
        }
    }

    #[test]
    fn test_class_def() {
        let mut vm = WrenVm::new(create_test_config());
        let value = vm.interpret("main", "class Foo {}");
        println!("Return value: {value:?}");
        assert!(value.is_ok());
    }
}
