use std::collections::HashMap;
use std::rc::Rc;

use crate::compiler::WrenCompiler;
use crate::error::WrenResult;
use crate::value::{new_handle, CallFrame, FiberState, Handle, ObjClosure, ObjFiber, ObjFn, ObjModule, Value};

pub type WrenWriteFn = fn(&mut WrenVm, message: &str);
pub type WrenErrorFn = fn(&mut WrenVm, message: &str, module_name: &str);

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
        let fiber = ObjFiber::new(Some(closure));

        todo!()
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

    fn run_interpreter(&mut self, fiber: Handle<ObjFiber>) -> WrenResult<()> {
        // Remember fiber
        fiber.borrow_mut().state = FiberState::Root;
        self.fiber = Some(fiber.clone());

        let mut frame: Option<&CallFrame> = None;
        let mut func: Option<Handle<ObjFn>> = None;

        (frame, func) = fiber.borrow().load_frame();

        todo!()
    }
}

#[cfg(test)]
mod test {
    use super::*;

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
        vm.interpret("main", r#"12345"#).expect("interpret");
    }
}
