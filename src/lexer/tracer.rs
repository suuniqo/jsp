use crate::{context::symtable::SymTable, token::{Token, TokenKind}, writer::{Writer, WriterErr}};

use super::{LexerCore, Lexer};


pub struct LexerTracer<'t, 'c, T: SymTable> {
    writer: Writer,
    trace: Vec<TokenKind>,
    inner: LexerCore<'t, 'c, T>,
}

impl<'t, 'c, T: SymTable> LexerTracer<'t, 'c, T> {
    pub fn new(inner: LexerCore<'t, 'c, T>, dump_path: Option<&str>) -> Result<Self, WriterErr> {
        let writer = Writer::new(dump_path)?;
        let trace = Vec::new();

        Ok(Self {
            writer,
            trace,
            inner,
        })
    }
}

impl<'t, 'c, T: SymTable> Iterator for LexerTracer<'t, 'c, T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(token) = self.inner.next() else {
            return None;
        };

        self.trace.push(token.kind.clone());

        Some(token)
    }
}

impl<T: SymTable> Lexer for LexerTracer<'_, '_, T> {}

impl<T: SymTable> Drop for LexerTracer<'_, '_, T> {
    fn drop(&mut self) {
        for kind in self.trace.iter() {
            self.writer
                .write(kind)
                .expect("error writing lexer output");
        }
    }
}
