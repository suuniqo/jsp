use crate::langtype::{TypeFunc, TypeVar};

use super::scope::Sym;


pub trait SymTable {
    fn pop_scope(&mut self);
    fn push_func(&mut self, pool_id: usize, ftype: TypeFunc) -> Result<(bool, Sym), ()>;
    fn push_var(&mut self, pool_id: usize, vtype: TypeVar) -> (bool, Sym);
}
