use super::WriterErr;


pub trait Tracer<T> {
    fn dump(&mut self) -> Result<(), WriterErr> {
        Ok(())
    }

    fn before_drop(&mut self) -> Option<Result<(), WriterErr>> {
        None
    }
}

pub trait HasTracer: Sized {
    type Tracer: Tracer<Self>;

    fn tracer(self, dump_path: Option<&str>) -> Box<Self::Tracer>;
}
