use std::{error, rc::Rc};

use crate::{ltype::TypeVar, pool::PoolLookup, span::Span, symtable::SymTableCore, tracer::HasTracer};

use super::scope::Sym;


pub trait SymTable {
    fn pop_scope(&mut self);

    fn push_func(&mut self, pool_id: usize, span: Option<Span>) -> (bool, Sym);
    fn push_local(&mut self, pool_id: usize, vtype: TypeVar, span: Option<Span>) -> (bool, Sym);
    fn push_global(&mut self, pool_id: usize, vtype: TypeVar, span: Option<Span>) -> (bool, Sym);

    fn add_params(&mut self, params: &[TypeVar]);
    fn add_ret_type(&mut self, ret_type: TypeVar);

    fn scopes(&self) -> usize;
    fn lexeme(&self, pool_id: usize) -> Option<Rc<str>>;
    fn search(&self, pool_id: usize) -> Option<&Sym>;

    fn finish(self: Box<Self>, _failure: bool) -> Result<(), Box<dyn error::Error>> {
        Ok(())
    }
}

impl dyn SymTable {
    pub fn make<Pool: PoolLookup>(trace: &Option<Option<String>>, inner: SymTableCore<Pool>) -> Box<dyn SymTable> {
        if let Some(file) = trace {
            inner.tracer(file.as_deref())
        } else {
            Box::new(inner)
        }
    }
}
