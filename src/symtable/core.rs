use std::{rc::Rc, cell::RefCell, cell::Ref};

use crate::{langtype::{LangType, TypeFunc, TypeVar}, span::Span};

use super::{scope::{Scope, Sym}, StrPool, SymTable};


pub struct SymTableCore {
    pool: Rc<RefCell<StrPool>>,
    scopes: Vec<Scope>,
    curr_idx: usize,
}

impl SymTableCore {
    const MAX_NESTED_FUNCS: usize = 2;

    pub fn new(pool: Rc<RefCell<StrPool>>) -> Self {
        Self {
            pool,
            scopes: vec![Scope::new(0)],
            curr_idx: 0,
        }
    }

    fn curr_scope(&mut self) -> &mut Scope {
        let len = self.scopes.len() - 1;

        &mut self.scopes[len]
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new(self.curr_idx));
        self.curr_idx += 1;
    }

    pub(super) fn pop_scope_inner(&mut self) -> Option<Scope> {
        self.scopes.pop()
    }

    pub(super) fn pool(&self) -> Ref<'_, StrPool> {
        self.pool.borrow()
    }
}

impl SymTable for SymTableCore {
    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn push_func(&mut self, pool_id: usize, ftype: TypeFunc, cause: Span) -> Option<(bool, Sym)> {
        if self.pool().get(pool_id).is_none() {
            unreachable!("tried to push function with invalid pool id");
        }
        if self.scopes.len() >= Self::MAX_NESTED_FUNCS {
            return None;
        }

        let ltype = LangType::Func(ftype);
        let (inserted, sym) = self.curr_scope().intern(pool_id, ltype, cause);

        if inserted {
            self.push_scope();
        }

        Some((inserted, sym))
    }

    fn push_var(&mut self, pool_id: usize, vtype: TypeVar, cause: Span) -> (bool, Sym) {
        if self.pool().get(pool_id).is_none() {
            unreachable!("tried to push function with invalid pool id");
        }

        let scope = self.curr_scope();

        let ltype = LangType::Var(vtype);
        let result = scope.intern(pool_id, ltype, cause);

        result 
    }

    fn search(&self, pool_id: usize) -> Option<&Sym> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(pool_id))
    }
}
