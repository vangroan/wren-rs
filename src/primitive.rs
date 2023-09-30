use crate::error::RuntimeError;
use crate::value::Value;
use crate::WrenVm;

pub(crate) type PrimitiveFn = fn(vm: &mut WrenVm, args: &[Value]) -> PrimitiveResult;
pub(crate) type PrimitiveResult = Result<Value, RuntimeError>;
