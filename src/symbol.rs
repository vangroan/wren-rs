use crate::error::{CompileError, WrenError, WrenResult};
use crate::limits::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SymbolId(u16);

impl SymbolId {
    pub(crate) fn new(value: u16) -> Self {
        Self(value)
    }

    pub(crate) fn from_usize(value: usize) -> Self {
        if value > u16::MAX as usize {
            panic!("value is too large for a 16-bit symbol identifier")
        }

        Self(value as u16)
    }

    pub fn as_u16(self) -> u16 {
        self.0
    }

    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

impl From<u16> for SymbolId {
    fn from(value: u16) -> Self {
        SymbolId::new(value)
    }
}

impl From<usize> for SymbolId {
    fn from(value: usize) -> Self {
        SymbolId::from_usize(value)
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    symbols: Vec<String>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { symbols: Vec::new() }
    }

    pub fn resolve(&self, name: impl AsRef<str>) -> Option<SymbolId> {
        let query = name.as_ref();
        self.symbols.iter().position(|el| el == query).map(SymbolId::from_usize)
    }

    pub fn insert(&mut self, name: impl ToString) -> WrenResult<SymbolId> {
        if self.symbols.len() >= MAX_SYMBOLS {
            return Err(WrenError::new_compile(CompileError::MaxSymbols));
        }

        let name_string = name.to_string();

        match self.resolve(name_string.as_str()) {
            // Symbol already exists.
            Some(symbol_id) => Ok(symbol_id),
            None => {
                let index = self.symbols.len();
                self.symbols.push(name_string);
                Ok(SymbolId::from(index))
            }
        }
    }

    pub fn len(&self) -> usize {
        self.symbols.len()
    }

    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &str> {
        self.symbols.iter().map(|string| string.as_str())
    }

    pub fn pairs(&self) -> impl Iterator<Item = (SymbolId, &str)> {
        self.symbols
            .iter()
            .enumerate()
            .map(|(idx, name)| (SymbolId::from_usize(idx), name.as_str()))
    }
}
