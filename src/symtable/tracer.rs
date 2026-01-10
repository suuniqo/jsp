use std::{cell::Ref, fmt, error, rc::Rc};

use crate::{ltype::TypeVar, pool::PoolLookup, span::Span, tracer::{HasTracer, Tracer, Writer, WriterErr}};

use super::{scope::{Scope, Sym}, SymTableCore, SymTable};


pub struct SymTableTracer<S: SymTable> {
    writer: Writer,
    trace: Vec<Scope>,
    inner: S,
}

impl<S: SymTable> SymTableTracer<S> {
    fn new(inner: S, dump_path: Option<&str>) -> Box<Self> {
        let writer = Writer::new(dump_path);

        Box::new(Self {
            writer,
            trace: Vec::new(),
            inner,
        })
    }
}

impl<P: PoolLookup> SymTable for SymTableTracer<SymTableCore<P>> {
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

    fn finish(self: Box<Self>, failure: bool) -> Result<(), Box<dyn error::Error>> {
        self.trace(failure)?;

        Ok(())
    }
}

struct SymTableTracerDisplay<'a, P: PoolLookup> {
    trace: &'a [Scope],
    pool: Ref<'a, P>,
}

impl<'a, P: PoolLookup> fmt::Display for SymTableTracerDisplay<'a, P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.trace.iter().enumerate().try_for_each(|(i, scope)| {
            scope.fmt(f, &*self.pool)?;

            if i != self.trace.len() - 1 {
                writeln!(f)?;
            }

            Ok(())
        })
    }
}

impl<P: PoolLookup> Tracer<SymTableCore<P>> for SymTableTracer<SymTableCore<P>> {
    fn dump(&mut self) -> Result<(), WriterErr> {
        let display = SymTableTracerDisplay {
            trace: &self.trace,
            pool: self.inner.pool(),
        };

        self.writer.write(format_args!("{}", display))
    }
}

impl<P: PoolLookup> HasTracer for SymTableCore<P> {
    type Tracer = SymTableTracer<SymTableCore<P>>;

    fn tracer(self, dump_path: Option<&str>) -> Box<Self::Tracer> {
        SymTableTracer::new(self, dump_path)
    }
}
