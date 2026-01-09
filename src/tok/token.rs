use crate::span::Span;

use super::TokenKind;


#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self {
            kind,
            span,
        }
    }

    pub fn eof() -> Self {
        Self {
            kind: TokenKind::Eof,
            span: Span::default(),
        }
    }
}
