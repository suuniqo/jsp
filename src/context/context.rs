use super::reporter::Reporter;
use super::symtable::SymTable;


pub struct Context<'t, T: SymTable> {
    pub symtable: T,
    pub reporter: Reporter<'t>,
}

impl<'t, T: SymTable> Context<'t, T> {
    pub fn new(symtable: T, reporter: Reporter<'t>) -> Self {
        Self {
            symtable,
            reporter,
        }
    }
}
