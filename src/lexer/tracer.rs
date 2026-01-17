use std::error;

use crate::{diag::DiagLevel, pool::{PoolInterner, PoolLookup}, token::{Token, TokenKind}, tracer::{HasTracer, Tracer, TracerErr, Writer}};

use super::{LexerCore, Lexer};


pub struct LexerTracer<L: Lexer> {
    writer: Writer,
    trace: Vec<TokenKind>,
    inner: L,
}

impl<L: Lexer> LexerTracer<L> {
    fn new(inner: L, dump_path: Option<&str>) -> Box<LexerTracer<L>> {
        let writer = Writer::new(dump_path);
        let trace = Vec::new();

        Box::new(Self {
            writer,
            trace,
            inner,
        })
    }
}

impl<L: Lexer> Iterator for LexerTracer<L> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.inner.next()?;

        self.trace.push(token.kind.clone());

        Some(token)
    }
}

impl<L: Lexer> Lexer for LexerTracer<L> {
    fn finish(self: Box<Self>, failure: Option<DiagLevel>) -> Result<(), Box<dyn error::Error>> {
        self.trace(failure.is_some_and(|failure| failure.is_lexical()))?;

        Ok(())
    }
}

impl<L: Lexer> Tracer<L> for LexerTracer<L> {
    fn dump(&mut self) -> Result<(), TracerErr> {
        self.trace
            .iter()
            .try_for_each(|kind| self.writer.write(format_args!("{}\n", kind)))
    }
}

impl<'t, Pool> HasTracer for LexerCore<'t, Pool>
where
    Pool: PoolLookup + PoolInterner
{
    type Tracer = LexerTracer<LexerCore<'t, Pool>>;

    fn tracer(self, dump_path: Option<&str>) -> Box<LexerTracer<LexerCore<'t, Pool>>> {
        LexerTracer::new(self, dump_path)
    }
}
