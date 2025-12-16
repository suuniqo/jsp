use super::WriterErr;


pub trait Tracer<T> {
    fn new(inner: T, dump_path: Option<&str>) -> Result<Box<Self>, WriterErr>
    where
        Self: Sized;

    fn dump(&mut self) -> Result<(), WriterErr>;
}

pub trait HasTracer: Sized {
    type Tracer: Tracer<Self>;

    fn tracer(self, dump_path: Option<&str>) -> Result<Box<Self::Tracer>, WriterErr>;
}
