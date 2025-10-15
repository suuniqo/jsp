use crate::writer::{Writer, WriterErr};

use super::{SymTable, SymTableCore, symbol::Symbol};


pub struct SymTableTracer {
    writer: Writer,
    trace: Vec<Symbol>,
    inner: SymTableCore,
}

impl SymTableTracer {
    pub fn new(inner: SymTableCore, dump_path: Option<&str>) -> Result<SymTableTracer, WriterErr> {
        let writer = Writer::new(dump_path)?;
        let trace = Vec::new();

        Ok(Self {
            writer,
            trace,
            inner,
        })
    }
}

impl SymTable for SymTableTracer {
    fn intern(&mut self, bytes: &[u8]) -> usize {
        let ((sym, pos), new) = self.inner.intern_ref(bytes);

        if new {
            self.trace.push(sym);
        }

        pos
    }

    fn as_keyword(&self, pos: usize) -> Option<crate::token::TokenKind> {
        self.inner.as_keyword(pos)
    }
}

impl Drop for SymTableTracer {
    fn drop(&mut self) {
        self.writer
            .write(&String::from("table #0:"))
            .expect("error writing symtable output");

        for sym in self.trace.iter() {
            self.writer
                .write(sym)
                .expect("error writing symtable output")
        }
    }
}
