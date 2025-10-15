use crate::tracer::{Tracer, TracerErr};

use super::{SymTable, SymTableCore};


pub struct SymTableTracer {
    tracer: Tracer,
    inner: SymTableCore,
}

impl SymTableTracer {
    pub fn new(inner: SymTableCore, dump_path: Option<&str>) -> Result<SymTableTracer, TracerErr> {
        let tracer = Tracer::new(dump_path)?;

        Ok(Self {
            tracer,
            inner,
        })
    }
}

impl SymTable for SymTableTracer {
    fn intern(&mut self, bytes: &[u8]) -> usize {
        let ((sym, pos), new) = self.inner.intern_ref(bytes);

        if new {
            if let Err(err) = self.tracer.trace(&sym) {
                eprintln!("error tracing symbol {}: {}", sym, err);
            }
        }

        pos
    }

    fn as_keyword(&self, pos: usize) -> Option<crate::token::TokenKind> {
        self.inner.as_keyword(pos)
    }
}
