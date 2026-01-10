use std::error;

use crate::{lexer::LexerCore, pool::PoolInterner, token::Token, tracer::HasTracer};


pub trait Lexer: Iterator<Item = Token> {
    fn finish(self: Box<Self>, _failure: bool) -> Result<(), Box<dyn error::Error>> {
        Ok(())
    }
}

impl dyn Lexer {
    pub fn make<'t, P: PoolInterner>(trace: &Option<Option<String>>, inner: LexerCore<'t, P>) -> Box<dyn Lexer + 't> {
        if let Some(file) = trace {
            inner.tracer(file.as_deref())
        } else {
            Box::new(inner)
        }
    }
}
