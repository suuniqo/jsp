use crate::{lexer::Lexer, token::Token};


pub struct LexerIter<'t, 'c> {
    lexer: Lexer<'t, 'c>,
}

impl<'t, 'c> LexerIter<'t, 'c> {
    pub fn from(lexer: Lexer<'t, 'c>) -> Self {
        Self { lexer }
    }
}

impl<'t, 'c> Iterator for LexerIter<'t, 'c> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.lexer.next_token()
    }
}

impl<'t, 'c> IntoIterator for Lexer<'t, 'c> {
    type Item = Token;

    type IntoIter = LexerIter<'t, 'c>;

    fn into_iter(self) -> Self::IntoIter {
        LexerIter::from(self)
    }
}
