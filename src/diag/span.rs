use crate::span::Span;

use super::DiagSever;


#[derive(Debug, Clone)]
pub struct DiagSpan {
    pub span: Span,
    pub sever: DiagSever,
    pub msg: Option<String>,
    pub highlight: bool,
}

impl DiagSpan {
    pub(super) fn new(span: Span, span_type: DiagSever, msg: Option<String>, highlight: bool) -> Self {
        Self { span, sever: span_type, msg, highlight }
    }
}

