//! Wren VM builder.
use crate::vm::WrenVm;

pub struct WrenBuilder {}

impl WrenBuilder {
    pub fn new() -> Self {
        Self {}
    }

    pub fn build(self) -> WrenVm {
        WrenVm {}
    }
}
