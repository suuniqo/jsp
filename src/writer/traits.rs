use super::WriterErr;


pub trait Tracer {
    fn dump(self) -> Result<(), WriterErr>;
}
