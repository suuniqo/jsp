use crate::{context::symtable::SymTable, token::Token, tracer::{Tracer, TracerErr}};

use super::{LexerCore, Lexer};


pub struct LexerTracer<'t, 'c, T: SymTable> {
    tracer: Tracer,
    inner: LexerCore<'t, 'c, T>,
}

impl<'t, 'c, T: SymTable> LexerTracer<'t, 'c, T> {
    pub fn new(inner: LexerCore<'t, 'c, T>, dump_path: Option<&str>) -> Result<Self, TracerErr> {
        let tracer = Tracer::new(dump_path)?;

        Ok(Self {
            tracer,
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

        if let Err(err) = self.tracer.trace(&token.kind) {
            eprintln!("error tracing token {}: {}", token, err);
        }

        Some(token)
    }
}

impl<T: SymTable> Lexer for LexerTracer<'_, '_, T> {}
