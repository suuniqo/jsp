use super::symbol::Symbol;


pub trait SymTable {
    fn intern(&mut self, bytes: &[u8]) -> usize;
    fn get(&self, pos: usize) -> Option<&Symbol>;
}
