use crate::context::{diagnostics::DiagManager, id_pool::IdPool};

pub mod id_pool;
pub mod symtable;
pub mod diagnostics;

pub struct Context<'t> {
    pub strpool: IdPool,
    pub diags: DiagManager<'t>,
}

impl<'t> Context<'t> {
    pub fn new(strpool: IdPool, diags: DiagManager<'t>) -> Self {
        Self {
            strpool,
            diags,
        }
    }
}
