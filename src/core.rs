//! Wren's Core module of builtins.
use crate::error::{RuntimeError, WrenResult};
use crate::limits::MAX_MODULE_VARS;
use crate::primitive::{PrimitiveFn, PrimitiveResult};
use crate::value::{new_handle, Handle, Method, ObjClass, ObjModule, Value};
use crate::vm::WrenVm;
use crate::SymbolId;

const CORE_MODULE_NAME: &str = "<core>";

/// Load the core module's classes and functions into the given module.
pub(crate) fn load_core_module(module: &mut ObjModule) -> WrenResult<()> {
    todo!()
}

pub(crate) struct BuiltIns {
    pub(crate) obj_class: Handle<ObjClass>,
    pub(crate) num_class: Handle<ObjClass>,
}

pub(crate) fn initialize_core(vm: &mut WrenVm) -> WrenResult<()> {
    let core_module_handle = new_handle(ObjModule::new(CORE_MODULE_NAME));
    if let Some(existing_module) = vm.modules.insert(CORE_MODULE_NAME.to_string(), core_module_handle.clone()) {
        log::warn!(
            "an existing module was occupying the core module slot: {}",
            existing_module.borrow().name()
        );
    }

    let core_module = &mut *core_module_handle.borrow_mut();

    // ----------------------------------------------------------------------------------
    // Object
    // Define the root Object class.
    // This has to be done a little specially because it has no superclass.
    let obj_class_handle = define_class(core_module, "Object")?;
    {
        let obj_class = &mut *obj_class_handle.borrow_mut();
        bind_primitive(vm, obj_class, "!", object_not)?;
    }

    // ----------------------------------------------------------------------------------
    // Number
    let num_class_handle = define_class(core_module, "Num")?;
    {
        let num_class = &mut *num_class_handle.borrow_mut();
        bind_primitive(vm, num_class, "+", num_plus)?;
    }

    vm.builtins = Some(BuiltIns {
        obj_class: obj_class_handle,
        num_class: num_class_handle,
    });

    Ok(())
}

pub(crate) fn define_class(module: &mut ObjModule, class_name: impl ToString) -> WrenResult<Handle<ObjClass>> {
    let name_string = class_name.to_string();
    let class = new_handle(ObjClass::new(name_string.clone(), 0));

    // The object holding the class is simply a variable in the module.
    define_variable(module, name_string, Value::from(class.clone()))?;

    Ok(class)
}

pub(crate) fn define_variable(module: &mut ObjModule, var_name: impl ToString, value: Value) -> WrenResult<SymbolId> {
    if module.var_count() >= MAX_MODULE_VARS {
        panic!("maximum module variables reached");
    }

    module.insert_var(var_name, value)
}

pub(crate) fn bind_primitive(
    vm: &mut WrenVm,
    class_obj: &mut ObjClass,
    method_name: &str,
    func: PrimitiveFn,
) -> WrenResult<()> {
    let symbol_id = vm.method_names.insert(method_name)?;
    class_obj.bind_method(symbol_id, Method::PrimitiveValue(func));
    Ok(())
}

// ============================================================================
// Built-in Functions

macro_rules! get_slot {
    ($args:tt, $slot_id:tt) => {
        $args.get($slot_id).ok_or_else(|| RuntimeError::InvalidSlot($slot_id))
    };
}

// ----------------------------------------------------------------------------
// Object

fn object_not(_vm: &mut WrenVm, _args: &[Value]) -> PrimitiveResult {
    Ok(Value::new_false())
}

// ----------------------------------------------------------------------------
// Number

fn num_plus(_vm: &mut WrenVm, args: &[Value]) -> PrimitiveResult {
    let arg0 = get_slot!(args, 0)?;
    let arg1 = get_slot!(args, 1)?;
    match (arg0, arg1) {
        (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a + b)),
        _ => Err(RuntimeError::InvalidType),
    }
}
