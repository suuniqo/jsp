use std::{cell::RefCell, rc::Rc};

use crate::{
    lexer::{Lexer, LexerCore},
    parser::{Parser, ParserCore},
    pool::{PoolInterner, PoolLookup, StrPool},
    reporter::Reporter,
    symtable::{SymTable, SymTableCore},
    target::Target,
    tracer::HasTracer
};


pub struct Factory;

impl Factory {
    pub fn pool() -> Rc<RefCell<StrPool>> {
        Rc::new(RefCell::new(StrPool::new()))
    }

    pub fn reporter<'t, Pool>(
        target: &'t Target,
        pool: Rc<RefCell<Pool>>,
        quiet: bool
    ) -> Rc<RefCell<Reporter<'t, Pool>>>
    where
        Pool: PoolLookup
    {
        Rc::new(RefCell::new(Reporter::new(target, pool, quiet)))
    }

    pub fn lexer<'t, Pool>(
        trace: &Option<Option<String>>,
        reporter: Rc<RefCell<Reporter<'t, Pool>>>,
        pool: Rc<RefCell<Pool>>,
        target: &'t Target
    ) -> Box<dyn Lexer + 't>
    where
        Pool: PoolLookup + PoolInterner
    {
        let inner = LexerCore::new(reporter, pool, target);

        if let Some(file) = trace {
            inner.tracer(file.as_deref())
        } else {
            Box::new(inner)
        }
    }

    pub fn symtable<Pool>(
        trace: &Option<Option<String>>,
        pool: Rc<RefCell<Pool>>
    ) -> Box<dyn SymTable>
    where 
        Pool: PoolLookup
    {
        let inner = SymTableCore::new(pool);

        if let Some(file) = trace {
            inner.tracer(file.as_deref())
        } else {
            Box::new(inner)
        }
    }

    pub fn parser<'t, 'l, 's, Pool>(
        trace: &Option<Option<String>>,
        reporter: Rc<RefCell<Reporter<'t, Pool>>>,
        lexer: &'l mut dyn Lexer,
        symtable: &'s mut dyn SymTable,
    ) -> Box<dyn Parser + 's>
    where
        't: 'l,
        'l: 's,
        Pool: PoolLookup
    {
        let inner = ParserCore::new(reporter, lexer, symtable);

        if let Some(file) = trace {
            inner.tracer(file.as_deref())
        } else {
            Box::new(inner)
        }
    }
}
