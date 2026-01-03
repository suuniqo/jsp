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
            scopes: vec![Scope::new(0, None)],
            curr_idx: 0,
        }
    }

    fn global_scope(&mut self) -> &mut Scope {
        &mut self.scopes[0]
    }

    fn curr_scope(&self) -> &Scope {
        let len = self.scopes.len() - 1;

        &self.scopes[len]
    }

    fn curr_scope_mut(&mut self) -> &mut Scope {
        let len = self.scopes.len() - 1;

        &mut self.scopes[len]
    }

    fn push_scope(&mut self, id: usize) {
        self.curr_idx += 1;
        self.scopes.push(Scope::new(self.curr_idx, Some(id)));
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

    fn push_func(&mut self, pool_id: usize, func_type: TypeFunc, span: Option<Span>) -> (bool, Sym) {
        if self.pool().get(pool_id).is_none() {
            unreachable!("tried to push function with invalid pool id");
        }

        let ltype = LangType::Func(func_type);
        let (inserted, sym) = self.curr_scope_mut().intern(pool_id, ltype, span);

        if inserted {
            self.push_scope(pool_id);
        }

        if self.scopes.len() > Self::MAX_NESTED_FUNCS {
            unreachable!("surpassed maximum nesting limit");
        }

        (inserted, sym)
    }

    fn push_var(&mut self, pool_id: usize, vtype: TypeVar, span: Option<Span>) -> (bool, Sym) {
        if self.pool().get(pool_id).is_none() {
            unreachable!("tried to push function with invalid pool id");
        }

        if let Some(res) = self.search(pool_id) {
            return (false, res.clone());
        }

        let scope = self.curr_scope_mut();

        let ltype = LangType::Var(vtype);
        let result = scope.intern(pool_id, ltype, span);

        result 
    }

    fn scopes(&self) -> usize {
        self.scopes.len()
    }

    fn is_current_func(&self, pool_id: usize) -> bool {
        self.curr_scope().id().is_some_and(|id| id == pool_id)
    }

    fn add_func_type(&mut self, pool_id: usize, func_type: TypeFunc) {
        self.global_scope().change_type(pool_id, LangType::Func(func_type));
    }

    fn lexeme(&self, pool_id: usize) -> Option<Rc<str>> {
        self.pool().get(pool_id).cloned()
    }

    fn search(&self, pool_id: usize) -> Option<&Sym> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.find(pool_id))
    }
}
