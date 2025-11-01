use std::fmt;

use crate::{token::TokenKind, color::Color};


#[derive(Clone, Copy)]
pub enum DiagKind {
    StrayChar(char),
    UntermComm,
    UntermStr,
    MalformedStr(char),
    OverflowStr(usize),
    InvEscSeq(char),
    OverflowInt,
    OverflowFloat,
    InvFmtFloat,
}

impl fmt::Display for DiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagKind::StrayChar(c) => write!(f, "illegal character '{}{}{}' in program", Color::HIGHLIGHT, c.escape_default(), Color::RESET),
            DiagKind::UntermComm => write!(f, "unterminated block comment"),
            DiagKind::UntermStr => write!(f, "missing terminating character '{}\"{}' on string literal", Color::HIGHLIGHT, Color::RESET),
            DiagKind::OverflowStr(len) => write!(f, "string literal is too long, length is {len} but the maximum is {}", TokenKind::MAX_STR_LEN),
            DiagKind::InvEscSeq(c) => write!(f, "unknown escape sequence '{}\\{c}{}'", Color::HIGHLIGHT, Color::RESET),
            DiagKind::OverflowInt => write!(f, "integer literal out of range for 16-byte type"),
            DiagKind::OverflowFloat => write!(f, "float literal out of range for 32-byte type"),
            DiagKind::InvFmtFloat => write!(f, "expected digit after '{}.{}' in float literal", Color::HIGHLIGHT, Color::RESET),
            DiagKind::MalformedStr(c) => write!(f, "malformed string literal, contains control character {}{}{}", Color::HIGHLIGHT, c.escape_default(), Color::RESET),
        }
    }
}
