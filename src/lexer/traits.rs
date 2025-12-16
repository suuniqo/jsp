use crate::{token::Token, writer::WriterErr};


pub trait Lexer: Iterator<Item = Token> {
    fn before_drop(&mut self) -> Option<Result<(), WriterErr>> { None }
}
