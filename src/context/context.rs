use super::diag::DiagManager;
use super::symtable::SymTable;


pub struct Context<'t, T: SymTable> {
    pub symtable: T,
    pub diags: DiagManager<'t>,
}

impl<'t, T: SymTable> Context<'t, T> {
    pub fn new(symtable: T, diags: DiagManager<'t>) -> Self {
        Self {
            symtable,
            diags,
        }
    }
}
