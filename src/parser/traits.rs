use crate::writer::WriterErr;

pub trait Parser {
    fn parse(&mut self) -> Option<Vec<usize>>;
    fn before_drop(&mut self) -> Option<Result<(), WriterErr>> { None }
}
