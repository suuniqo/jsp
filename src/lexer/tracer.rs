use crate::{tok::{Token, TokenKind}, write::{HasTracer, Tracer, Writer, WriterErr}};

use super::{LexerCore, Lexer};


pub struct LexerTracer<'t> {
    writer: Writer,
    trace: Vec<TokenKind>,
    inner: LexerCore<'t>,
}

impl<'t> LexerTracer<'t> {
    fn new(inner: LexerCore<'t>, dump_path: Option<&str>) -> Box<LexerTracer<'t>> {
        let writer = Writer::new(dump_path);
        let trace = Vec::new();

        Box::new(Self {
            writer,
            trace,
            inner,
        })
    }
}

impl Iterator for LexerTracer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.inner.next()?;

        self.trace.push(token.kind.clone());

        Some(token)
    }
}

impl<'t> Lexer<'t> for LexerTracer<'t> {}

impl<'t> Tracer<LexerCore<'t>> for LexerTracer<'t> {
    fn dump(&mut self) -> Result<(), WriterErr> {
        self.trace
            .iter()
            .try_for_each(|kind| self.writer.write(format_args!("{}\n", kind)))
    }

    fn before_drop(&mut self) -> Option<Result<(), WriterErr>> {
        Some(self.dump())
    }
}

impl<'t> HasTracer for LexerCore<'t> {
    type Tracer = LexerTracer<'t>;

    fn tracer(self, dump_path: Option<&str>) -> Box<LexerTracer<'t>> {
        LexerTracer::new(self, dump_path)
    }
}

impl<'t> Tracer<LexerCore<'t>> for LexerCore<'t> {}
