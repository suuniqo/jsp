use std::fmt;

use crate::token::TokenKind;

use super::color::DiagColor;
use super::sever::DiagSever;


#[derive(Clone, Copy)]
pub enum DiagKind {
    StrayChar(char),
    UntermComm,
    UntermStr(usize),
    OverflowStr((usize, usize)),
    InvEscSeq(char),
    OverflowInt(usize),
    OverflowFloat(usize),
    InvFmtFloat(usize),
}

impl DiagKind {
    pub fn payload_len(&self) -> usize {
        match self {
            DiagKind::StrayChar(_) => 1,
            DiagKind::UntermComm => 2,
            DiagKind::UntermStr(len) => *len,
            DiagKind::OverflowStr((len, added_len)) => *len + *added_len,
            DiagKind::InvEscSeq(_) => 2,
            DiagKind::OverflowInt(len) => *len,
            DiagKind::OverflowFloat(len) => *len,
            DiagKind::InvFmtFloat(len) => *len,
        }
    }

    pub fn sever(&self) -> DiagSever {
        match self {
            DiagKind::InvEscSeq(_) => DiagSever::Warning,
            DiagKind::StrayChar(_) => DiagSever::Error,
            DiagKind::UntermComm => DiagSever::Error,
            DiagKind::UntermStr(_) => DiagSever::Error,
            DiagKind::OverflowStr(_) => DiagSever::Error,
            DiagKind::OverflowInt(_) => DiagSever::Error,
            DiagKind::OverflowFloat(_) => DiagSever::Error,
            DiagKind::InvFmtFloat(_) => DiagSever::Error,
        }
    }
}

impl fmt::Display for DiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagKind::StrayChar(c) => write!(f, "illegal character '{}{c}{}' in program", DiagColor::HIGHLIGHT, DiagColor::RESET),
            DiagKind::UntermComm => write!(f, "unterminated comment"),
            DiagKind::UntermStr(_) => write!(f, "missing terminating character '{}\"{}' on string literal", DiagColor::HIGHLIGHT, DiagColor::RESET),
            DiagKind::OverflowStr((len, _)) => write!(f, "string literal is too long, length is {len} but the maximum is {}", TokenKind::MAX_STR_LEN),
            DiagKind::InvEscSeq(c) => write!(f, "unknown escape sequence '{}\\{c}{}'", DiagColor::HIGHLIGHT, DiagColor::RESET),
            DiagKind::OverflowInt(_) => write!(f, "integer literal out of range for 16-byte type"),
            DiagKind::OverflowFloat(_) => write!(f, "float literal out of range for 32-byte type"),
            DiagKind::InvFmtFloat(_) => write!(f, "expected digit after '{}.{}' in float literal", DiagColor::HIGHLIGHT, DiagColor::RESET),
        }
    }
}
