use super::symbol::Symbol;


pub trait SymTable {
    fn intern(&mut self, lexeme: &str) -> usize;
    fn get(&self, pos: usize) -> Option<&Symbol>;
}
