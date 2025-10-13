use crate::{token::TokenKind, target::Target};

struct DiagColor;

impl DiagColor {
    pub const RESET: &'static str = "\x1b[0m";
    pub const HIGHLIGHT: &'static str = "\x1b[1;38;5;189m";
    pub const NOTE: &'static str = "\x1b[1;38;5;74m";
    pub const WARNING: &'static str = "\x1b[1;38;5;217m";
    pub const ERROR: &'static str =  "\x1b[1;38;5;211m";
}

pub enum DiagSever {
    Info,
    Warning,
    Error,
}

impl DiagSever {
    fn str(&self) -> &'static str {
        match self {
            DiagSever::Info => "note",
            DiagSever::Warning => "warning",
            DiagSever::Error => "error",
        }
    }

    fn color(&self) -> &'static str {
        match self {
            DiagSever::Info => DiagColor::NOTE,
            DiagSever::Warning => DiagColor::WARNING,
            DiagSever::Error => DiagColor::ERROR,
        }
    }
}

pub enum DiagKind {
    StrayChar(char),
    UnterminatedComment,
    UnterminatedStr(usize),
    StrOverflow((usize, usize)),
    InvEscSeq(char),
    IntOverflow(usize),
    FloatOverflow(usize),
    FloatInvFmt(usize),
}

impl DiagKind {
    fn msg(&self) -> String {
        match self {
            DiagKind::StrayChar(c) => format!("stray '{}{c}{}' in program", DiagColor::HIGHLIGHT, DiagColor::RESET),
            DiagKind::UnterminatedComment => "unterminated comment".to_string(),
            DiagKind::UnterminatedStr(_) => format!("missing terminating character '{}\"{}' on string literal", DiagColor::HIGHLIGHT, DiagColor::RESET),
            DiagKind::StrOverflow((len, _)) => format!("string literal is too long, length is {len} but the maximum is {}", TokenKind::MAX_STR_LEN),
            DiagKind::InvEscSeq(c) => format!("unknown escape sequence '{}\\{c}{}'", DiagColor::HIGHLIGHT, DiagColor::RESET),
            DiagKind::IntOverflow(_) => format!("integer literal out of range for 16-byte type"),
            DiagKind::FloatOverflow(_) => format!("float literal  out of range for 32-byte type"),
            DiagKind::FloatInvFmt(_) => format!("expected digit after '{}.{}' in float literal", DiagColor::HIGHLIGHT, DiagColor::RESET),
        }
    }

    fn payload_len(&self) -> usize {
        match self {
            DiagKind::StrayChar(_) => 1,
            DiagKind::UnterminatedComment => 2,
            DiagKind::UnterminatedStr(len) => *len,
            DiagKind::StrOverflow((len, added_len)) => *len + *added_len,
            DiagKind::InvEscSeq(_) => 2,
            DiagKind::IntOverflow(len) => *len,
            DiagKind::FloatOverflow(len) => *len,
            DiagKind::FloatInvFmt(len) => *len,
        }
    }

    fn sever(&self) -> DiagSever {
        match self {
            DiagKind::InvEscSeq(_) => DiagSever::Warning,
            DiagKind::StrayChar(_) => DiagSever::Error,
            DiagKind::UnterminatedComment => DiagSever::Error,
            DiagKind::UnterminatedStr(_) => DiagSever::Error,
            DiagKind::StrOverflow(_) => DiagSever::Error,
            DiagKind::IntOverflow(_) => DiagSever::Error,
            DiagKind::FloatOverflow(_) => DiagSever::Error,
            DiagKind::FloatInvFmt(_) => DiagSever::Error,
        }
    }
}

struct Diag {
    kind: DiagKind,
    row: usize,
    col: usize,
}

impl Diag {
    pub fn new(kind: DiagKind, row: usize, col: usize) -> Self {
        Self {
            kind,
            row,
            col,
        }
    }
}

pub struct DiagManager<'t> {
    target: &'t Target,
    diags: Vec<Diag>,
    max_row_len: usize,
}

impl<'t> DiagManager<'t> {
    pub fn new(target: &'t Target) -> Self {
        Self {
            target,
            diags: Vec::new(),
            max_row_len: 1,
        }
    }

    pub fn push(&mut self, kind: DiagKind, row: usize, col: usize) {
        let diag = Diag::new(kind, row, col);

        let row_len = Self::row_len(diag.row);

        if row_len > self.max_row_len {
            self.max_row_len = row_len;
        }

        self.diags.push(diag);
    }
    
    pub fn dump(&self) {
        let padding_len = self.max_row_len - (self.max_row_len % 4) + 4;

        for diag in self.diags.iter() {
            let padding_row = " ".repeat(padding_len);
            let padding_ctx = " ".repeat(diag.col - 1);

            let len = diag.kind.payload_len();
            let underline = "~".repeat(if len > 0 { len - 1 } else { 0 });

            let context = self.target.nth_line(diag.row - 1)
                .unwrap_or_else(|| unreachable!("diagnostic <{}> with invalid line {}", diag.kind.msg(), diag.row));

            println!("{}{}:{}:{}: {}{}: {}{}",
                DiagColor::HIGHLIGHT, self.target.path(), diag.row, diag.col,
                diag.kind.sever().color(), diag.kind.sever().str(), DiagColor::RESET, diag.kind.msg()
            );

            println!("{}{} | {}{}{}{}{}",
                &padding_row[DiagManager::row_len(diag.row)..], diag.row, &context[..padding_ctx.len()],
                diag.kind.sever().color(), &context[padding_ctx.len()..padding_ctx.len()+underline.len()+1], DiagColor::RESET,
                &context[padding_ctx.len()+underline.len()+1..]
            );

            println!("{} | {}{}^{}{}",
                padding_row, padding_ctx,
                diag.kind.sever().color(), underline, DiagColor::RESET
            );
        }
    }

    fn row_len(row: usize) -> usize {
        (row as f64).log10() as usize + 1
    }
}
