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
    pub fn new(span: Span, sever: DiagSever, msg: Option<String>, highlight: bool) -> Self {
        Self { span, sever, msg, highlight }
    }

    pub fn new_note(span: Span, msg: &str) -> Self {
        Self {
            span,
            sever: DiagSever::Note,
            msg: Some(msg.into()),
            highlight: false,
        }
    }

    pub fn new_error(span: Span, msg: &str) -> Self {
        Self {
            span,
            sever: DiagSever::Error,
            msg: Some(msg.into()),
            highlight: true,
        }
    }
}
