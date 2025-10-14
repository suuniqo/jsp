use super::diag::DiagManager;
use super::sym_table::SymTable;


pub struct Context<'t> {
    pub strpool: SymTable,
    pub diags: DiagManager<'t>,
}

impl<'t> Context<'t> {
    pub fn new(strpool: SymTable, diags: DiagManager<'t>) -> Self {
        Self {
            strpool,
            diags,
        }
    }
}
