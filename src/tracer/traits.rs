use super::WriterErr;


pub trait Tracer<T> {
    fn dump(&mut self) -> Result<(), WriterErr>;

    fn trace(mut self: Box<Self>, valid_trace: bool) -> Result<(), WriterErr> {
        if !valid_trace {
            return Ok(())
        }

        self.dump()
    }
}

pub trait HasTracer: Sized {
    type Tracer: Tracer<Self>;

    fn tracer(self, dump_path: Option<&str>) -> Box<Self::Tracer>;
}
