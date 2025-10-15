use std::fmt;

use crate::token::TokenKind;

use super::color::DiagColor;
use super::sever::DiagSever;


#[derive(Clone, Copy)]
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
    pub fn payload_len(&self) -> usize {
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

    pub fn sever(&self) -> DiagSever {
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

impl fmt::Display for DiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagKind::StrayChar(c) => write!(f, "illegal character '{}{c}{}' in program", DiagColor::HIGHLIGHT, DiagColor::RESET),
            DiagKind::UnterminatedComment => write!(f, "unterminated comment"),
            DiagKind::UnterminatedStr(_) => write!(f, "missing terminating character '{}\"{}' on string literal", DiagColor::HIGHLIGHT, DiagColor::RESET),
            DiagKind::StrOverflow((len, _)) => write!(f, "string literal is too long, length is {len} but the maximum is {}", TokenKind::MAX_STR_LEN),
            DiagKind::InvEscSeq(c) => write!(f, "unknown escape sequence '{}\\{c}{}'", DiagColor::HIGHLIGHT, DiagColor::RESET),
            DiagKind::IntOverflow(_) => write!(f, "integer literal out of range for 16-byte type"),
            DiagKind::FloatOverflow(_) => write!(f, "float literal  out of range for 32-byte type"),
            DiagKind::FloatInvFmt(_) => write!(f, "expected digit after '{}.{}' in float literal", DiagColor::HIGHLIGHT, DiagColor::RESET),
        }
    }
}
