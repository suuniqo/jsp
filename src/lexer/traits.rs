use crate::{lexer::LexerCore, token::Token, writer::Tracer};


pub trait Lexer<'t>: Iterator<Item = Token> + Tracer<LexerCore<'t>> {}
