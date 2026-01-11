use std::error;

use crate::{pool::PoolLookup, tracer::HasTracer};

use super::ParserCore;


pub trait Parser {
    fn parse(&mut self) -> Option<Vec<usize>>;

    fn finish(self: Box<Self>, _failure: bool) -> Result<(), Box<dyn error::Error>> {
        Ok(())
    }
}

impl dyn Parser {
    pub fn make<'t, 'l, 's, Pool>(
        trace: &Option<Option<String>>,
        inner: ParserCore<'t, 'l, 's, Pool>
    ) -> Box<dyn Parser + 's>
    where
        't: 'l,
        'l: 's,
        Pool: PoolLookup
    {
        if let Some(file) = trace {
            inner.tracer(file.as_deref())
        } else {
            Box::new(inner)
        }
    }
}
