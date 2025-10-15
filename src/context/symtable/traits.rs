use crate::token::TokenKind;


pub trait SymTable {
    fn intern(&mut self, bytes: &[u8]) -> usize;

    fn as_keyword(&self, pos: usize) -> Option<TokenKind>;
}

