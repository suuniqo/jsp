use crate::span::Span;

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


pub type DiagRef = Box<Diag>;

#[derive(Debug, Clone)]
pub struct Diag {
    pub kind: DiagKind,
    pub spans: Vec<DiagSpan>,
    pub help: Option<DiagHelp>,
}

impl Diag {
    pub fn make(kind: DiagKind, span: Span, highlight: bool) -> DiagRef {
        let msg = kind.msg();
        let sever = kind.sever();

        Box::new(Self {
            kind,
            spans: vec![DiagSpan::new(span, sever, Some(msg), highlight)],
            help: None,
        })
    }

    pub fn add_help(&mut self, help: DiagHelp) {
        self.help = Some(help)
    }

    pub fn add_note(&mut self, span: Span, msg: &str) {
        self.spans.push(DiagSpan::new_note(span, msg));
    }

    pub fn add_error(&mut self, span: Span, msg: &str) {
        self.spans.push(DiagSpan::new_error(span, msg));
    }

    pub fn with_help(mut self: DiagRef, help: DiagHelp) -> DiagRef {
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
