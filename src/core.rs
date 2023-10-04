//! Wren's Core module of builtins.
use crate::error::{RuntimeError, WrenError, WrenResult};
use crate::limits::MAX_MODULE_VARS;
use crate::primitive::{PrimitiveFn, PrimitiveResult};
use crate::value::{new_handle, Handle, Method, ObjClass, ObjModule, Value};
use crate::vm::WrenVm;
use crate::SymbolId;

const CORE_MODULE_NAME: &str = "<core>";

// TODO: Use upstream Wren's Core module when we can compile and interpret the whole language.
// const CORE_MODULE_SCRIPT: &str = include_str!("../wren-c/src/vm/wren_core.wren");
const CORE_MODULE_SCRIPT: &str = include_str!("wren_core.wren");

/// Load the core module's classes and functions into the given module.
pub(crate) fn load_core_module(vm: &WrenVm, module: &mut ObjModule) -> WrenResult<()> {
    println!("loading core module into {}", module.name());

    let core_handle = vm.modules.get(CORE_MODULE_NAME).cloned().unwrap();
    let core = core_handle.borrow();

    for (_, name, value) in core.var_pairs() {
        module.insert_var(name, value.clone())?;
    }

    Ok(())
}

pub(crate) struct BuiltIns {
    pub(crate) obj_class: Handle<ObjClass>,
    pub(crate) cls_class: Handle<ObjClass>,
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
    // Class
    let cls_class_handle = define_class(core_module, "Class")?;
    {
        let cls_class = &mut *cls_class_handle.borrow_mut();
        cls_class.bind_super_class(obj_class_handle.clone());
    }

    // ----------------------------------------------------------------------------------
    // Object Metaclass
    let obj_meta_class = define_class(core_module, "Object metaclass")?;

    // Wire up the metaclass relationships now that all three classes are built.
    // Note: This creates circular references that will leak heap memory.
    obj_class_handle.borrow_mut().obj.class = Some(obj_meta_class.clone());
    obj_meta_class.borrow_mut().obj.class = Some(cls_class_handle.clone());
    cls_class_handle.borrow_mut().obj.class = Some(cls_class_handle.clone());

    obj_class_handle.borrow_mut().bind_super_class(cls_class_handle.clone());

    // The core class diagram ends up looking like this, where single lines point
    // to a class's superclass, and double lines point to its metaclass:
    //
    //        .------------------------------------. .====.
    //        |                  .---------------. | #    #
    //        v                  |               v | v    #
    //   .---------.   .-------------------.   .-------.  #
    //   | Object  |==>| Object metaclass  |==>| Class |=="
    //   '---------'   '-------------------'   '-------'
    //        ^                                 ^ ^ ^ ^
    //        |                  .--------------' # | #
    //        |                  |                # | #
    //   .---------.   .-------------------.      # | # -.
    //   |  Base   |==>|  Base metaclass   |======" | #  |
    //   '---------'   '-------------------'        | #  |
    //        ^                                     | #  |
    //        |                  .------------------' #  | Example classes
    //        |                  |                    #  |
    //   .---------.   .-------------------.          #  |
    //   | Derived |==>| Derived metaclass |=========="  |
    //   '---------'   '-------------------'            -'

    // The rest of the classes can now be defined normally.
    // vm.interpret(CORE_MODULE_NAME, CORE_MODULE_SCRIPT)?;

    // ----------------------------------------------------------------------------------
    // Number
    let num_class_handle = define_class(core_module, "Num")?;
    {
        let num_class = &mut *num_class_handle.borrow_mut();
        bind_primitive(vm, num_class, "+(_)", num_plus)?;
        bind_primitive(vm, num_class, "-(_)", num_minus)?;
        bind_primitive(vm, num_class, "*(_)", num_multiply)?;
        bind_primitive(vm, num_class, "/(_)", num_div)?;
    }

    // ----------------------------------------------------------------------------------
    // System
    let sys_class_handle = define_class(core_module, "System")?;
    {
        let sys_class = &mut *sys_class_handle.borrow_mut();
        bind_primitive(vm, sys_class, "writeString_(_)", sys_write_string)?;
    }

    vm.builtins = Some(BuiltIns {
        obj_class: obj_class_handle,
        cls_class: cls_class_handle,
        num_class: num_class_handle,
    });

    Ok(())
}

pub(crate) fn teardown_core(vm: &mut WrenVm) {
    // Unlink circular references in object model
    if let Some(builtins) = vm.builtins.take() {
        if let Some(obj_meta_class) = builtins.obj_class.borrow().obj.class.clone() {
            let mut obj_meta_class_ref = obj_meta_class.borrow_mut();
            obj_meta_class_ref.obj.class = None;
            obj_meta_class_ref.unbind_super_class();
        }

        builtins.obj_class.borrow_mut().obj.class = None;
        builtins.cls_class.borrow_mut().obj.class = None;
        builtins.num_class.borrow_mut().obj.class = None;

        builtins.obj_class.borrow_mut().unbind_super_class();
        builtins.cls_class.borrow_mut().unbind_super_class();
        builtins.cls_class.borrow_mut().unbind_super_class();
    }
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

pub(crate) fn find_class_var(module: &mut ObjModule, var_name: impl AsRef<str>) -> WrenResult<Handle<ObjClass>> {
    module
        .find_var(var_name.as_ref())
        .unwrap()
        .try_class()
        .cloned()
        .map_err(|err| WrenError::new_runtime(err))
}

/// Bind a native Rust function to a class as a method.
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
    Ok(Value::False)
}

// ----------------------------------------------------------------------------
// Number

macro_rules! num_bin_op {
    ($args:path, $arg0:ident $op:tt $arg1:ident) => {
        {
            let $arg0 = get_slot!($args, 0)?;
            let $arg1 = get_slot!($args, 1)?;
            match ($arg0, $arg1) {
                (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a $op b)),
                _ => Err(RuntimeError::InvalidType),
            }
        }
    }
}

fn num_plus(_vm: &mut WrenVm, args: &[Value]) -> PrimitiveResult {
    num_bin_op!(args, arg0 + arg1)
}

fn num_minus(_vm: &mut WrenVm, args: &[Value]) -> PrimitiveResult {
    num_bin_op!(args, arg0 - arg1)
}

fn num_multiply(_vm: &mut WrenVm, args: &[Value]) -> PrimitiveResult {
    num_bin_op!(args, arg0 * arg1)
}

fn num_div(_vm: &mut WrenVm, args: &[Value]) -> PrimitiveResult {
    num_bin_op!(args, arg0 / arg1)
}

// ----------------------------------------------------------------------------
// System

fn sys_write_string(vm: &mut WrenVm, args: &[Value]) -> PrimitiveResult {
    let arg1 = get_slot!(args, 1)?;

    if let Some(write_fn) = vm.config().write_fn {
        let string = arg1.try_str()?;
        write_fn(vm, string);
    }

    Ok(arg1.clone())
}
