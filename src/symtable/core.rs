use std::{rc::Rc, cell::RefCell, cell::Ref};

use crate::{langtype::{LangType, TypeFunc, TypeVar}, span::Span};

use super::{scope::{Scope, Sym}, StrPool, SymTable};


pub struct SymTableCore {
    pool: Rc<RefCell<StrPool>>,
    scopes: Vec<Scope>,
    curr_idx: usize,
    scope_func: Option<(Sym, bool)>,
}

impl SymTableCore {
    const MAX_NESTED_FUNCS: usize = 2;

    pub fn new(pool: Rc<RefCell<StrPool>>) -> Self {
        Self {
            pool,
            scopes: vec![Scope::new(0, None)],
            curr_idx: 0,
            scope_func: None,
        }
    }

    fn global_scope_mut(&mut self) -> &mut Scope {
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

    fn push_var<F>(&mut self, scope: F, vtype: TypeVar, pool_id: usize, span: Option<Span>, implicit: bool) -> (bool, Sym)
    where
        F: FnOnce(&mut Self) -> &mut Scope
    {
        if self.pool().get(pool_id).is_none() {
            unreachable!("tried to push variable with invalid pool id");
        }

        let ltype = LangType::Var(vtype);
        let result = scope(self).intern(pool_id, ltype, span, implicit);

        result 
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

    fn push_func(&mut self, pool_id: usize, span: Option<Span>) -> (bool, Sym) {
        if self.pool().get(pool_id).is_none() {
            unreachable!("tried to push function with invalid pool id");
        }

        let Some(LangType::Func(func_type)) = self.scope_func.as_ref().map(|sym| sym.0.lang_type.clone()) else {
            unreachable!("scope func has type typevar");
        };

        let (inserted, sym) = self.curr_scope_mut().intern(pool_id, LangType::Func(func_type), span.clone(), false);

        if inserted {
            self.scope_func = Some((sym.clone(), false));
        } else if let Some(scope_func) = &mut self.scope_func {
            scope_func.0.pool_id = pool_id;
            scope_func.0.span = span;
            scope_func.1 = true;
        } else {
            unreachable!("missing func_type during scope insertion");
        }

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

    fn add_params(&mut self, params: &[TypeVar]) {
        let pool_id = self.curr_scope().id()
            .expect("tried changing function type of global scope");

        let Some(scope_func) = &mut self.scope_func else {
            unreachable!("tried to add parameters to empty func type");
        };

        let LangType::Func(func_type) = scope_func.0.lang_type.clone() else {
            unreachable!("scope func has type typevar");
        };

        let func_type = TypeFunc::new(func_type.ret_type.clone(), &params);

        scope_func.0.lang_type = LangType::Func(func_type.clone());

        if !scope_func.1 {
            self.global_scope_mut().change_type(pool_id, LangType::Func(func_type));
        }
    }

    fn add_ret_type(&mut self, ret_type: TypeVar) {
        self.scope_func = Some((
            Sym::explicit(
                LangType::Func(TypeFunc::new(ret_type, &[])),
                0,
                0,
                None,
            ),
            false,
        ))
    }

    fn scopes(&self) -> usize {
        self.scopes.len()
    }

    fn lexeme(&self, pool_id: usize) -> Option<Rc<str>> {
        self.pool().get(pool_id).cloned()
    }

    fn search(&self, pool_id: usize) -> Option<&Sym> {
        if let Some(id) = self.curr_scope().id()
            && id == pool_id
            && let Some(sym) = &self.scope_func {

            return Some(&sym.0);
        }

        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.find(pool_id))
    }
}
