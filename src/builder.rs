//! Wren VM builder.
use crate::vm::{WrenConfig, WrenErrorFn, WrenVm, WrenWriteFn};

pub struct WrenBuilder {
    write_fn: Option<WrenWriteFn>,
    error_fn: Option<WrenErrorFn>,
}

impl Default for WrenBuilder {
    fn default() -> Self {
        WrenBuilder::new()
    }
}

impl WrenBuilder {
    pub fn new() -> Self {
        Self {
            write_fn: None,
            error_fn: None,
        }
    }

    pub fn build(self) -> WrenVm {
        let Self { write_fn, error_fn } = self;

        let config = WrenConfig { write_fn, error_fn };

        WrenVm::new(config)
    }
}
