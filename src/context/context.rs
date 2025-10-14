use super::diag::DiagManager;
use super::symtable::SymTable;


pub struct Context<'t> {
    pub symtable: SymTable,
    pub diags: DiagManager<'t>,
}

impl<'t> Context<'t> {
    pub fn new(symtable: SymTable, diags: DiagManager<'t>) -> Self {
        Self {
            symtable,
            diags,
        }
    }
}
