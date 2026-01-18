use std::{rc::Rc, cell::RefCell, cell::Ref};

use crate::{types::{TypeId, TypeFunc, TypeVar}, span::Span, pool::PoolLookup};

use super::{scope::{Scope, Sym}, SymTable};


pub struct SymTableCore<Pool: PoolLookup> {
    pool: Rc<RefCell<Pool>>,
    scopes: Vec<Scope>,
    curr_idx: usize,
}

impl<Pool: PoolLookup> SymTableCore<Pool> {
    const MAX_NESTED_FUNCS: usize = 2;

    pub fn new(pool: Rc<RefCell<Pool>>) -> Self {
        Self {
            pool,
            scopes: vec![Scope::new(0, None)],
            curr_idx: 0,
        }
    }

    fn global_scope_mut(&mut self) -> &mut Scope {
        &mut self.scopes[0]
    }

    fn curr_scope_mut(&mut self) -> &mut Scope {
        let len = self.scopes.len() - 1;

        &mut self.scopes[len]
    }

    fn push_scope(&mut self, id: usize) {
        self.curr_idx += 1;
        self.scopes.push(Scope::new(self.curr_idx, Some(id)));
    }

    fn push_var<F>(&mut self, scope: F, vtype: TypeVar, pool_id: usize, span: Option<Span>, implicit: bool) -> (bool, Sym)
    where
        F: FnOnce(&mut Self) -> &mut Scope
    {
        if self.pool().lookup(pool_id).is_none() {
            unreachable!("tried to push variable with invalid pool id");
        }

        let ltype = TypeId::Var(vtype);

        scope(self).intern(pool_id, ltype, span, implicit)
    }

    pub(super) fn pop_scope_inner(&mut self) -> Option<Scope> {
        self.scopes.pop()
    }

    pub(super) fn pool(&self) -> Ref<'_, Pool> {
        self.pool.borrow()
    }
}

impl<Pool: PoolLookup> SymTable for SymTableCore<Pool> {
    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn push_func(&mut self, pool_id: usize, params: &[TypeVar], ret_type: TypeVar, span: Option<Span>) -> (bool, Sym) {
        if self.pool().lookup(pool_id).is_none() {
            unreachable!("tried to push function with invalid pool id");
        }

        let func_type = TypeFunc::new(ret_type, params);

        let (inserted, sym) = self.curr_scope_mut().intern(pool_id, TypeId::Func(func_type), span.clone(), false);

        self.push_scope(pool_id);

        if self.scopes.len() > Self::MAX_NESTED_FUNCS {
            unreachable!("surpassed maximum nesting limit");
        }

        (inserted, sym)
    }

    fn push_local(&mut self, pool_id: usize, vtype: TypeVar, span: Option<Span>) -> (bool, Sym) {
        self.push_var(Self::curr_scope_mut, vtype, pool_id, span, false)
    }

    fn push_global(&mut self, pool_id: usize, vtype: TypeVar, span: Option<Span>) -> (bool, Sym) {
        self.push_var(Self::global_scope_mut, vtype, pool_id, span, true)
    }

    fn scopes(&self) -> usize {
        self.scopes.len()
    }

    fn lexeme(&self, pool_id: usize) -> Option<Rc<str>> {
        self.pool().lookup(pool_id).cloned()
    }

    fn search(&self, pool_id: usize) -> Option<&Sym> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.find(pool_id))
    }
}
