use crate::{lexer::Lexer, writer::{Tracer, Writer, WriterErr}};

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

impl<'t, 'l, L: Lexer> Tracer for ParserTracer<'t, 'l, L> {
    fn dump(mut self) -> Result<(), WriterErr> {
        let Some(trace) = &self.trace else {
            self.writer.write(format_args!("ascending\n"))?;
            return  Ok(());
        };

        let len = trace.len();

        self.writer.write(format_args!("ascending"))?;

        trace.iter()
             .enumerate()
             .try_for_each(|(i, rule)| if i + 1 != len {
                 self.writer.write(format_args!(" {}", rule))
             } else {
                 self.writer.write(format_args!(" {}\n", rule))
             })
    }
}

