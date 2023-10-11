use wren::WrenBuilder;

#[ignore = "TODO: VM not implemented"]
#[test]
fn test_hello_world() {
    let mut vm = WrenBuilder::new().build();

    vm.interpret("main", include_str!("test_basic.wren")).expect("interpret failed");
}

#[test]
fn test_class_def() {
    let mut vm = WrenBuilder::new().build();

    const SOURCE: &str = r#"
    class Foo {
      bar() {
        return 7
      }
    }
    "#;

    vm.interpret("main", SOURCE).expect("interpret failed");
}
