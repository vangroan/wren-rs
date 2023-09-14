use wren::WrenBuilder;

#[test]
fn test_hello_world() {
    let vm = WrenBuilder::new().build();

    vm.interpret(include_str!("test_basic.wren"));
}
