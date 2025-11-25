use std::rc::Rc;

use super::symbol::Symbol;


pub trait SymTable {
    fn intern(&mut self, lexeme: &str) -> (usize, Rc<str>);
    fn get(&self, pos: usize) -> Option<&Symbol>;
}
