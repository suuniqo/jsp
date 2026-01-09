use std::rc::Rc;

use crate::{ltype::TypeVar, span::Span, symtable::SymTableCore, write::{Tracer, HasTracer}};

use super::bind::Sym;


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

impl dyn SymTable {
    pub fn make(trace: &Option<Option<String>>, inner: SymTableCore) -> Box<dyn SymTable> {
        if let Some(file) = trace {
            inner.tracer(file.as_deref())
        } else {
            Box::new(inner)
        }
    }
}
