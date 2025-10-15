use crate::token::Token;


pub trait Lexer: Iterator<Item = Token> {}
