use super::TracerErr;


pub trait Tracer<T> {
    fn dump(&mut self) -> Result<(), TracerErr>;

    fn trace(mut self: Box<Self>, failure: bool) -> Result<(), TracerErr> {
        if failure {
            return Ok(())
        }

        self.dump()
    }
}

pub trait HasTracer: Sized {
    type Tracer: Tracer<Self>;

    fn tracer(self, dump_path: Option<&str>) -> Box<Self::Tracer>;
}
