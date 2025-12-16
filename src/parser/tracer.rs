use crate::writer::{Tracer, HasTracer, Writer, WriterErr};

use super::{ParserCore, Parser};


pub struct ParserTracer<'t, 'l, 's> {
    writer: Writer,
    trace: Option<Vec<usize>>,
    inner: ParserCore<'t, 'l, 's>,
}

impl<'t, 'l, 's> Parser for ParserTracer<'t, 'l, 's> {
    fn parse(&mut self) -> Option<Vec<usize>> {
        self.trace = self.inner.parse();

        self.trace.clone()
    }

    fn before_drop(&mut self) -> Option<Result<(), WriterErr>> {
        Some(self.dump())
    }
}

impl<'t, 'l, 's> Tracer<ParserCore<'t, 'l, 's>> for ParserTracer<'t, 'l, 's> {
    fn new(inner: ParserCore<'t, 'l, 's>, dump_path: Option<&str>) -> Result<Box<Self>, WriterErr> {
        let writer = Writer::new(dump_path)?;
        let trace = None;

        Ok(Box::new(Self {
            writer,
            trace,
            inner,
        }))
    }

    fn dump(&mut self) -> Result<(), WriterErr> {
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

impl<'t, 'l, 's> HasTracer for ParserCore<'t, 'l, 's> {
    type Tracer = ParserTracer<'t, 'l, 's>;

    fn tracer(self, dump_path: Option<&str>) -> Result<Box<Self::Tracer>, WriterErr> {
        ParserTracer::new(self, dump_path)
    }
}
