use crate::{langtype::{TypeFunc, TypeVar}, writer::WriterErr};

use super::scope::Sym;


pub trait SymTable {
    fn pop_scope(&mut self);
    fn push_func(&mut self, pool_id: usize, ftype: TypeFunc) -> Option<(bool, Sym)>;
    fn push_var(&mut self, pool_id: usize, vtype: TypeVar) -> (bool, Sym);
    fn search(&self, pool_id: usize) -> Option<&Sym>;
    fn before_drop(&mut self) -> Option<Result<(), WriterErr>> { None }
}
