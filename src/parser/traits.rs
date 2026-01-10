use std::error;

use crate::tracer::HasTracer;

use super::ParserCore;


pub trait Parser {
    fn parse(&mut self) -> Option<Vec<usize>>;

    fn finish(self: Box<Self>, _failure: bool) -> Result<(), Box<dyn error::Error>> {
        Ok(())
    }
}

impl dyn Parser {
    pub fn make<'t: 'l, 'l: 's, 's>(trace: &Option<Option<String>>, inner: ParserCore<'t, 'l, 's>) -> Box<dyn Parser + 's> {
        if let Some(file) = trace {
            inner.tracer(file.as_deref())
        } else {
            Box::new(inner)
        }
    }
}
