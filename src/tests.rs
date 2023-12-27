//! Integration tests that require internal access.
use crate::builder::WrenBuilder;
use crate::compiler::WrenCompiler;
use crate::core::load_core_module;
use crate::value::{new_handle, ObjModule, Value};

#[test]
fn test_class_definition() {
    let mut vm = WrenBuilder::new().build();

    const SOURCE: &str = r#"class Foo {
    bar(x) { 1 + 1 }
}"#;
    let module = new_handle(ObjModule::new("main"));
    load_core_module(&mut vm, &mut *module.borrow_mut()).unwrap();
    let compiler = WrenCompiler::new(module, SOURCE, vm.method_names.clone());

    let obj_fn = compiler.compile(false).unwrap();
    println!("{}", vm.vm_dump());
    println!("Constants\n---------");
    println!("{}", obj_fn.borrow().dump_constants());
    println!("Opcodes\n-------");
    println!("{}", obj_fn.borrow().dump_opcodes());

    println!("Functions\n---------");
    for (index, value) in obj_fn.borrow().constants.iter().enumerate() {
        if let Value::Func(func) = value {
            println!("Constant {index}");
            println!("Constants\n---------");
            println!("{}", func.borrow().dump_constants());
            println!("Opcodes\n-------");
            println!("{}", func.borrow().dump_opcodes());
        }
    }
}
