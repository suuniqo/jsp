use std::{fmt, cell::Ref};

use crate::{langtype::{TypeFunc, TypeVar}, writer::{Tracer, HasTracer, Writer, WriterErr}};

use super::{scope::{Scope, Sym}, SymTableCore, SymTable, StrPool};


pub struct SymTableTracer {
    writer: Writer,
    trace: Vec<Scope>,
    inner: SymTableCore,
}

impl SymTable for SymTableTracer {
    fn pop_scope(&mut self) {
        if let Some(scope) = self.inner.pop_scope_inner() {
            self.trace.push(scope);
        }
    }

    fn push_func(&mut self, pool_id: usize, ftype: TypeFunc) -> Result<(bool, Sym), ()> {
        self.inner.push_func(pool_id, ftype)
    }

    fn push_var(&mut self, pool_id: usize, vtype: TypeVar) -> (bool, Sym) {
        self.inner.push_var(pool_id, vtype)
    }

    fn before_drop(&mut self) -> Option<Result<(), WriterErr>> {
        Some(self.dump())
    }
}

struct SymTableTracerDisplay<'a> {
    trace: &'a [Scope],
    pool: Ref<'a, StrPool>,
}

impl<'a> fmt::Display for SymTableTracerDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.trace.iter().try_for_each(|scope| scope.fmt(f, &self.pool))
    }
}

impl Tracer<SymTableCore> for SymTableTracer {
    fn new(inner: SymTableCore, dump_path: Option<&str>) -> Result<Box<Self>, WriterErr> {
        let writer = Writer::new(dump_path)?;

        Ok(Box::new(Self {
            writer,
            trace: Vec::new(),
            inner,
        }))
    }

    fn dump(&mut self) -> Result<(), WriterErr> {
        let display = SymTableTracerDisplay {
            trace: &self.trace,
            pool: self.inner.pool(),
        };

        self.writer.write(format_args!("{}", display))
    }
}

impl HasTracer for SymTableCore {
    type Tracer = SymTableTracer;

    fn tracer(self, dump_path: Option<&str>) -> Result<Box<Self::Tracer>, WriterErr> {
        SymTableTracer::new(self, dump_path)
    }
}
