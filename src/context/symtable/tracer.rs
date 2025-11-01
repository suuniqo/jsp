use crate::writer::{Writer, WriterErr};

use super::{SymTable, SymTableCore, symbol::Symbol};


pub struct SymTableTracer {
    writer: Writer,
    trace: Vec<Symbol>,
    inner: SymTableCore,
    pos: usize,
}

impl SymTableTracer {
    pub fn new(inner: SymTableCore, dump_path: Option<&str>) -> Result<SymTableTracer, WriterErr> {
        let writer = Writer::new(dump_path)?;
        let trace = Vec::new();
        let pos = 0;

        Ok(Self {
            writer,
            trace,
            inner,
            pos,
        })
    }
}

impl SymTable for SymTableTracer {
    fn intern(&mut self, lexeme: &str) -> usize {
        let pos = self.inner.intern(lexeme);

        if pos == self.pos {
            self.trace.push(self.inner.get(pos).expect("couldn't newly inserted id").clone());
            self.pos += 1;
        }

        pos
    }

    fn get(&self, pos: usize) -> Option<&Symbol> {
        self.inner.get(pos)
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
