use crate::{token::{Token, TokenKind}, writer::{Writer, WriterErr, Tracer}};

use super::{LexerCore, Lexer};


pub struct LexerTracer<'t> {
    writer: Writer,
    trace: Vec<TokenKind>,
    inner: LexerCore<'t>,
}

impl<'t> LexerTracer<'t> {
    pub fn new(inner: LexerCore<'t>, dump_path: Option<&str>) -> Result<Self, WriterErr> {
        let writer = Writer::new(dump_path)?;
        let trace = Vec::new();

        Ok(Self {
            writer,
            trace,
            inner,
        })
    }
}

impl Iterator for LexerTracer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(token) = self.inner.next() else {
            return None;
        };

        self.trace.push(token.kind.clone());

        Some(token)
    }
}

impl Lexer for LexerTracer<'_> {}

impl Tracer for LexerTracer<'_> {
    fn dump(mut self) -> Result<(), WriterErr> {
        self.trace
            .iter()
            .try_for_each(|kind| self.writer.write(format_args!("{}\n", kind)))
    }
}
