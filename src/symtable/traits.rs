use std::rc::Rc;

use crate::{langtype::{TypeFunc, TypeVar}, span::Span, writer::WriterErr};

use super::scope::Sym;


pub trait SymTable {
    fn pop_scope(&mut self);
    fn push_func(&mut self, pool_id: usize, ftype: TypeFunc, span: Option<Span>) -> (bool, Sym);
    fn push_var(&mut self, pool_id: usize, vtype: TypeVar, span: Option<Span>) -> (bool, Sym);

    fn is_current_func(&self, pool_id: usize) -> bool;
    fn add_func_type(&mut self, pool_id: usize, func_type: TypeFunc);

    fn scopes(&self) -> usize;
    fn lexeme(&self, pool_id: usize) -> Option<Rc<str>>;
    fn search(&self, pool_id: usize) -> Option<&Sym>;

    fn before_drop(&mut self) -> Option<Result<(), WriterErr>> { None }
}
