use std::fmt;

use super::TokenKind;


pub struct Token {
    pub kind: TokenKind,
    pub row: usize,
    pub col: usize,
}

impl Token {
    pub fn new(kind: TokenKind, row: usize, col: usize) -> Self {
        Self {
            kind,
            row,
            col,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "({}, {}, {})", self.row, self.col, self.kind)
    }
}
