use std::error;

use crate::{diag::DiagLevel, pool::PoolLookup, tracer::{HasTracer, Tracer, TracerErr, Writer}};

use super::{ParserCore, Parser};


pub struct ParserTracer<P: Parser> {
    writer: Writer,
    trace: Option<Vec<usize>>,
    inner: P,
}

impl<P: Parser> ParserTracer<P> {
    fn new(inner: P, dump_path: Option<&str>) -> Box<Self> {
        let writer = Writer::new(dump_path);
        let trace = None;

        Box::new(Self {
            writer,
            trace,
            inner,
        })
    }
}

impl<P: Parser> Parser for ParserTracer<P> {
    fn parse(&mut self) -> Option<Vec<usize>> {
        self.trace = self.inner.parse();

        self.trace.clone()
    }

    fn finish(self: Box<Self>, failure: Option<DiagLevel>) -> Result<(), Box<dyn error::Error>> {
        self.trace(failure.is_some_and(|failure| failure.is_syntactic()))?;

        Ok(())
    }
}

impl<P: Parser> Tracer<P> for ParserTracer<P> {
    fn dump(&mut self) -> Result<(), TracerErr> {
        let Some(trace) = &self.trace else {
            self.writer.write(format_args!("Ascending\n"))?;
            return  Ok(());
        };

        let len = trace.len();

        self.writer.write(format_args!("Ascending"))?;

        trace.iter()
             .enumerate()
             .try_for_each(|(i, rule)| if i + 1 != len {
                 self.writer.write(format_args!(" {}", rule))
             } else {
                 self.writer.write(format_args!(" {}\n", rule))
             })
    }
}

impl<'t: 'l, 'l: 's, 's, Pool: PoolLookup> HasTracer for ParserCore<'t, 'l, 's, Pool> {
    type Tracer = ParserTracer<ParserCore<'t, 'l, 's, Pool>>;

    fn tracer(self, dump_path: Option<&str>) -> Box<ParserTracer<ParserCore<'t, 'l, 's, Pool>>> {
        ParserTracer::new(self, dump_path)
    }
}
