use crate::{lexer::LexerCore, token::Token, writer::{Tracer, HasTracer}};


pub trait Lexer<'t>: Iterator<Item = Token> + Tracer<LexerCore<'t>> {}

impl<'t> dyn Lexer<'t> {
    pub fn make(trace: &Option<Option<String>>, inner: LexerCore<'t>) -> Box<dyn Lexer<'t> + 't> {
        if let Some(file) = trace {
            inner.tracer(file.as_deref())
        } else {
            Box::new(inner)
        }
    }
}
