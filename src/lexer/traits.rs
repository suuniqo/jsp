use std::error;

use crate::{diag::DiagLevel, token::Token};


pub trait Lexer: Iterator<Item = Token> {
    fn finish(self: Box<Self>, _failure: Option<DiagLevel>) -> Result<(), Box<dyn error::Error>> {
        Ok(())
    }
}
