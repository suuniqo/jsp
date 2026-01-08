use crate::{parser::ParserCore, writer::{Tracer, HasTracer}};

pub trait Parser<'t: 'l, 'l: 's, 's>: Tracer<ParserCore<'t, 'l, 's>> {
    fn parse(&mut self) -> Option<Vec<usize>>;
}

impl<'t: 'l, 'l: 's, 's> dyn Parser<'t, 'l, 's> {
    pub fn make(trace: &Option<Option<String>>, inner: ParserCore<'t, 'l, 's>) -> Box<dyn Parser<'t, 'l, 's> + 's> {
        if let Some(file) = trace {
            inner.tracer(file.as_deref())
        } else {
            Box::new(inner)
        }
    }
}
