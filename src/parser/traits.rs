use crate::{parser::ParserCore, writer::Tracer};

pub trait Parser<'t: 'l, 'l: 's, 's>: Tracer<ParserCore<'t, 'l, 's>> {
    fn parse(&mut self) -> Option<Vec<usize>>;
}
