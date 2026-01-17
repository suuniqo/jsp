use std::error;

use crate::diag::DiagLevel;


pub trait Parser {
    fn parse(&mut self) -> Option<Vec<usize>>;

    fn finish(self: Box<Self>, _failure: Option<DiagLevel>) -> Result<(), Box<dyn error::Error>> {
        Ok(())
    }
}
