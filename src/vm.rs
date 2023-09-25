use crate::compiler::WrenCompiler;
use crate::error::WrenResult;
use crate::value::{ObjClosure, ObjFn, ObjModule, Value};
use std::rc::Rc;

pub struct WrenVm {}

impl WrenVm {
    pub fn interpret(&mut self, module_name: &str, source: &str) -> WrenResult<Value> {
        let _closure = self.compile_in_module(module_name, source, false)?;

        todo!()
    }

    fn compile_in_module(&mut self, module_name: &str, source: &str, is_expression: bool) -> WrenResult<ObjClosure> {
        // let module: Option<ObjModule> = get_module(module_name)
        // if module.is_none() { create module }
        //      implicitly import core module
        // FIXME: Do we need to copy module name to a `Value`?
        let module = ObjModule::new(module_name);

        let func = self.compile(module, source, is_expression)?;

        // Functions are always wrapped in closures, to simplify VM implementation.
        Ok(ObjClosure::new(func))
    }

    fn compile(&mut self, module: ObjModule, source: &str, is_expression: bool) -> WrenResult<Rc<ObjFn>> {
        let mut compiler = WrenCompiler::new(source);
        compiler.compile(module, is_expression)
    }
}
