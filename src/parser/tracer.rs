use crate::{lexer::Lexer, writer::{Writer, WriterErr}};

use super::{ParserCore, Parser};


pub struct ParserTracer<'t, 'l, L: Lexer> {
    writer: Writer,
    trace: Option<Vec<usize>>,
    inner: ParserCore<'t, 'l, L>,
}

impl<'t, 'l, L: Lexer> ParserTracer<'t, 'l, L> {
    pub fn new(inner: ParserCore<'t, 'l, L>, dump_path: Option<&str>) -> Result<Self, WriterErr> {
        let writer = Writer::new(dump_path)?;
        let trace = None;

        Ok(Self {
            writer,
            trace,
            inner,
        })
    }
}

impl<'t, 'l, L: Lexer> Parser for ParserTracer<'t, 'l, L> {
    fn parse(&mut self) -> Option<Vec<usize>> {
        self.trace = self.inner.parse();

        self.trace.clone()
    }
}

impl<'t, 'l, L: Lexer> Drop for ParserTracer<'t, 'l, L> {
    fn drop(&mut self) {
        let Some(trace) = &self.trace else {
            self.writer
                .write(&"ascending")
                .expect("error writing parser output");

            return;
        };

        let parse_trace = trace
            .iter()
            .map(|rule_idx| rule_idx.to_string())
            .collect::<Vec<_>>()
            .join(" ");

        self.writer
            .write(&["ascending", &parse_trace].join(" "))
            .expect("error writing parser output");
    }
}

