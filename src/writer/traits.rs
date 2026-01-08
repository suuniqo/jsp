use crate::diag::DiagLevel;

use super::WriterErr;


pub trait Tracer<T> {
    fn dump(&mut self) -> Result<(), WriterErr> {
        Ok(())
    }

    fn before_drop(&mut self) -> Option<Result<(), WriterErr>> {
        None
    }

    fn finish(mut self: Box<Self>, threshold: DiagLevel, level: DiagLevel) -> Result<(), WriterErr> {
        if level <= threshold {
            return Ok(())
        }

        self.before_drop().transpose().map(|_| ())
    }
}

pub trait HasTracer: Sized {
    type Tracer: Tracer<Self>;

    fn tracer(self, dump_path: Option<&str>) -> Box<Self::Tracer>;
}
