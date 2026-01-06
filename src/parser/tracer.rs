use crate::writer::{Tracer, HasTracer, Writer, WriterErr};

use super::{ParserCore, Parser};


pub struct ParserTracer<'t, 'l, 's> {
    writer: Writer,
    trace: Option<Vec<usize>>,
    inner: ParserCore<'t, 'l, 's>,
}

impl<'t, 'l, 's> ParserTracer<'t, 'l, 's> {
    fn new(inner: ParserCore<'t, 'l, 's>, dump_path: Option<&str>) -> Box<Self> {
        let writer = Writer::new(dump_path);
        let trace = None;

        Box::new(Self {
            writer,
            trace,
            inner,
        })
    }
}

impl<'t: 'l, 'l: 's, 's> Parser<'t, 'l, 's> for ParserTracer<'t, 'l, 's> {
    fn parse(&mut self) -> Option<Vec<usize>> {
        self.trace = self.inner.parse();

        self.trace.clone()
    }
}

impl<'t, 'l, 's> Tracer<ParserCore<'t, 'l, 's>> for ParserTracer<'t, 'l, 's> {
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

    fn before_drop(&mut self) -> Option<Result<(), WriterErr>> {
        Some(self.dump())
    }
}

impl<'t: 'l, 'l: 's, 's> HasTracer for ParserCore<'t, 'l, 's> {
    type Tracer = ParserTracer<'t, 'l, 's>;

    fn tracer(self, dump_path: Option<&str>) -> Box<ParserTracer<'t, 'l, 's>> {
        ParserTracer::new(self, dump_path)
    }
}

impl<'t, 'l, 's> Tracer<ParserCore<'t, 'l, 's>> for ParserCore<'t, 'l, 's> {}
