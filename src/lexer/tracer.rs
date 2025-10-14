use crate::{token::Token, tracer::{Tracer, TracerErr}};

use super::LexerCore;


pub struct LexerTracer<'t, 'c> {
    tracer: Tracer,
    inner: LexerCore<'t, 'c>,
}

impl<'t, 'c> LexerTracer<'t, 'c> {
    pub fn new(inner: LexerCore<'t, 'c>, dump_path: Option<&str>) -> Result<Self, TracerErr> {
        let tracer = Tracer::new(dump_path)?;

        Ok(Self {
            tracer,
            inner,
        })
    }
}

impl<'t, 'c> Iterator for LexerTracer<'t, 'c> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let Some(token) = self.inner.next() else {
            return None;
        };

        if let Err(err) = self.tracer.trace(&token.kind) {
            eprintln!("error tracing token {}: {}", token, err);
        }

        Some(token)
    }
}
