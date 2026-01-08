use std::{cell::Ref, fmt, rc::Rc};

use crate::{langtype::TypeVar, span::Span, writer::{HasTracer, Tracer, Writer, WriterErr}, strpool::StrPool};

use super::{scope::{Scope, Sym}, SymTableCore, SymTable};


pub struct SymTableTracer {
    writer: Writer,
    trace: Vec<Scope>,
    inner: SymTableCore,
}

impl SymTableTracer {
    fn new(inner: SymTableCore, dump_path: Option<&str>) -> Box<Self> {
        let writer = Writer::new(dump_path);

        Box::new(Self {
            writer,
            trace: Vec::new(),
            inner,
        })
    }
}

impl SymTable for SymTableTracer {
    fn pop_scope(&mut self) {
        if let Some(scope) = self.inner.pop_scope_inner() {
            self.trace.push(scope);
        }
    }

    fn push_func(&mut self, pool_id: usize, span: Option<Span>) -> (bool, Sym) {
        self.inner.push_func(pool_id, span)
    }

    fn push_local(&mut self, pool_id: usize, vtype: TypeVar, span: Option<Span>) -> (bool, Sym) {
        self.inner.push_local(pool_id, vtype, span)
    }

    fn push_global(&mut self, pool_id: usize, vtype: TypeVar, span: Option<Span>) -> (bool, Sym) {
        self.inner.push_global(pool_id, vtype, span)
    }

    fn add_params(&mut self, params: &[TypeVar]) {
        self.inner.add_params(params);
    }

    fn add_ret_type(&mut self, ret_type: TypeVar) {
        self.inner.add_ret_type(ret_type);
    }

    fn scopes(&self) -> usize {
        self.inner.scopes()
    }

    fn lexeme(&self, pool_id: usize) -> Option<Rc<str>> {
        self.inner.lexeme(pool_id)
    }

    fn search(&self, pool_id: usize) -> Option<&Sym> {
        self.inner.search(pool_id)
    }
}

struct SymTableTracerDisplay<'a> {
    trace: &'a [Scope],
    pool: Ref<'a, StrPool>,
}

impl<'a> fmt::Display for SymTableTracerDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.trace.iter().enumerate().try_for_each(|(i, scope)| {
            scope.fmt(f, &self.pool)?;

            if i != self.trace.len() - 1 {
                writeln!(f)?;
            }

            Ok(())
        })
    }
}

impl Tracer<SymTableCore> for SymTableTracer {
    fn dump(&mut self) -> Result<(), WriterErr> {
        let display = SymTableTracerDisplay {
            trace: &self.trace,
            pool: self.inner.pool(),
        };

        self.writer.write(format_args!("{}", display))
    }

    fn before_drop(&mut self) -> Option<Result<(), WriterErr>> {
        Some(self.dump())
    }
}

impl HasTracer for SymTableCore {
    type Tracer = SymTableTracer;

    fn tracer(self, dump_path: Option<&str>) -> Box<Self::Tracer> {
        SymTableTracer::new(self, dump_path)
    }
}

impl Tracer<SymTableCore> for SymTableCore {}
