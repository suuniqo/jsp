use crate::span::Span;

use super::DiagKind;


pub struct Diag {
    pub kind: DiagKind,
    pub span: Span,
}

impl Diag {
    pub fn new(kind: DiagKind, span: Span) -> Self {
        Self {
            kind,
            span,
        }
    }
}
