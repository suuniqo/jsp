use std::error;

use crate::{pool::PoolInterner, token::{Token, TokenKind}, tracer::{HasTracer, Tracer, Writer, WriterErr}};

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
    fn finish(self: Box<Self>, failure: bool) -> Result<(), Box<dyn error::Error>> {
        self.trace(failure)?;

        Ok(())
    }
}

impl<L: Lexer> Tracer<L> for LexerTracer<L> {
    fn dump(&mut self) -> Result<(), WriterErr> {
        self.trace
            .iter()
            .try_for_each(|kind| self.writer.write(format_args!("{}\n", kind)))
    }
}

impl<'t, P: PoolInterner> HasTracer for LexerCore<'t, P> {
    type Tracer = LexerTracer<LexerCore<'t, P>>;

    fn tracer(self, dump_path: Option<&str>) -> Box<LexerTracer<LexerCore<'t, P>>> {
        LexerTracer::new(self, dump_path)
    }
}
