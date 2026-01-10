use crate::span::Span;

mod core;
mod kind;
mod sever;
mod help;
mod level;
mod span;

pub use help::{DiagHelp, HelpAction};
pub use kind::DiagKind;
pub use sever::DiagSever;
pub use level::DiagLevel;
pub use span::DiagSpan;


#[derive(Debug, Clone)]
pub struct Diag {
    pub kind: DiagKind,
    pub spans: Vec<DiagSpan>,
    pub help: Option<DiagHelp>,
}

impl Diag {
    pub fn make(kind: DiagKind, span: Span, highlight: bool) -> Self {
        let msg = kind.msg();
        let sever = kind.sever();

        Self {
            kind,
            spans: vec![DiagSpan::new(span, sever, Some(msg), highlight)],
            help: None,
        }
    }

    pub fn add_span(&mut self, span: Span, sever: DiagSever, message: Option<String>, highlight: bool) {
        self.spans.push(DiagSpan::new(span, sever, message, highlight));
    }

    pub fn add_help(&mut self, help: DiagHelp) {
        self.help = Some(help)
    }

    pub fn with_help(mut self, help: DiagHelp) -> Diag {
        self.add_help(help);

        self
    }

    pub fn has_help(&self) -> bool {
        self.help.is_some()
    }

    pub fn main_span(&self) -> Span {
        self.spans[0].span.clone()
    }
}
