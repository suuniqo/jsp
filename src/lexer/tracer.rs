use crate::{symtable::SymTable, token::{Token, TokenKind}, writer::{Writer, WriterErr}};

use super::{LexerCore, Lexer};


pub struct LexerTracer<'t, T: SymTable> {
    writer: Writer,
    trace: Vec<TokenKind>,
    inner: LexerCore<'t, T>,
}

impl<'t, 'r, 's, T: SymTable> LexerTracer<'t, T> {
    pub fn new(inner: LexerCore<'t, T>, dump_path: Option<&str>) -> Result<Self, WriterErr> {
        let writer = Writer::new(dump_path)?;
        let trace = Vec::new();

        Ok(Self {
            writer,
            trace,
            inner,
        })
    }
}

impl<'t, 'r, 's, T: SymTable> Iterator for LexerTracer<'t, T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(token) = self.inner.next() else {
            return None;
        };

        self.trace.push(token.kind.clone());

        Some(token)
    }
}

impl<T: SymTable> Lexer for LexerTracer<'_, T> {}

impl<T: SymTable> Drop for LexerTracer<'_, T> {
    fn drop(&mut self) {
        let lexer_trace = self.trace
            .iter()
            .map(|kind| kind.to_string())
            .collect::<Vec<_>>()
            .join("\n");

        self.writer
            .write(&lexer_trace)
            .expect("error writing lexer output");
    }
}
