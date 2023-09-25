//! Wren VM builder.
use crate::vm::WrenVm;

pub struct WrenBuilder {}

impl Default for WrenBuilder {
    fn default() -> Self {
        WrenBuilder::new()
    }
}

impl WrenBuilder {
    pub fn new() -> Self {
        Self {}
    }

    pub fn build(self) -> WrenVm {
        WrenVm {}
    }
}
