use std::error;

use crate::{lexer::LexerCore, pool::{PoolInterner, PoolLookup}, token::Token, tracer::HasTracer};


pub trait Lexer: Iterator<Item = Token> {
    fn finish(self: Box<Self>, _failure: bool) -> Result<(), Box<dyn error::Error>> {
        Ok(())
    }
}

impl dyn Lexer {
    pub fn make<'t, Pool>(trace: &Option<Option<String>>, inner: LexerCore<'t, Pool>) -> Box<dyn Lexer + 't>
    where
        Pool: PoolLookup + PoolInterner
    {
        if let Some(file) = trace {
            inner.tracer(file.as_deref())
        } else {
            Box::new(inner)
        }
    }
}
