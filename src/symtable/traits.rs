use std::rc::Rc;

use crate::{langtype::TypeVar, span::Span, symtable::SymTableCore, writer::Tracer};

use super::scope::Sym;


pub trait SymTable: Tracer<SymTableCore> {
    fn pop_scope(&mut self);

    fn push_func(&mut self, pool_id: usize, span: Option<Span>) -> (bool, Sym);
    fn push_local(&mut self, pool_id: usize, vtype: TypeVar, span: Option<Span>) -> (bool, Sym);
    fn push_global(&mut self, pool_id: usize, vtype: TypeVar, span: Option<Span>) -> (bool, Sym);

    fn add_params(&mut self, params: &[TypeVar]);
    fn add_ret_type(&mut self, ret_type: TypeVar);

    fn scopes(&self) -> usize;
    fn lexeme(&self, pool_id: usize) -> Option<Rc<str>>;
    fn search(&self, pool_id: usize) -> Option<&Sym>;
}
