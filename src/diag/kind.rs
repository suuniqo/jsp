use std::fmt;

use crate::{token::TokenKind, color::Color};


#[derive(Clone)]
pub enum DiagKind {
    StrayChar(char),
    UntermComm,
    UntermStr,
    MalformedStr(char),
    OverflowStr(usize),
    InvEscSeq(char),
    OverflowInt,
    OverflowFloat,
    InvFmtFloat(String),
    UnexpectedTok((TokenKind, bool, Vec<TokenKind>))
}

impl DiagKind {
    pub fn afterword(&self) -> String {
        match self {
            DiagKind::StrayChar(_) => "here".to_string(),
            DiagKind::UntermComm => "started here".to_string(),
            DiagKind::UntermStr => "started here".to_string(),
            DiagKind::MalformedStr(_) => "remove this character".to_string(),
            DiagKind::OverflowStr(len) => format!("length is {} but maximum is {}", len, TokenKind::MAX_STR_LEN),
            DiagKind::InvEscSeq(c) => format!("interpreted as \\\\{}", c),
            DiagKind::OverflowInt => format!("maximum is {}", i16::MAX),
            DiagKind::OverflowFloat => format!("maximum is {:e}", f32::MAX),
            DiagKind::InvFmtFloat(num) => format!("perhaps you meant '{}.0'", num),
            DiagKind::UnexpectedTok((_, before, _)) => format!("{} this token", if *before { "before" } else { "after" }),
        }
    }
}

impl fmt::Display for DiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagKind::StrayChar(c) => write!(f, "illegal character '{}{}{}' in program", Color::HIGHLIGHT, c.escape_default(), Color::RESET),
            DiagKind::UntermComm => write!(f, "unterminated block comment"),
            DiagKind::UntermStr => write!(f, "missing terminating character '{}\"{}' on string literal", Color::HIGHLIGHT, Color::RESET),
            DiagKind::OverflowStr(_) => write!(f, "string literal is too long"),
            DiagKind::InvEscSeq(c) => write!(f, "unknown escape sequence '{}\\{c}{}'", Color::HIGHLIGHT, Color::RESET),
            DiagKind::OverflowInt => write!(f, "integer literal out of range for 16-byte type"),
            DiagKind::OverflowFloat => write!(f, "float literal out of range for 32-byte type"),
            DiagKind::InvFmtFloat(_) => write!(f, "expected digit after '{}.{}' in float literal", Color::HIGHLIGHT, Color::RESET),
            DiagKind::MalformedStr(c) => write!(f, "malformed string literal, contains control character '{}{}{}'", Color::HIGHLIGHT, c.escape_default(), Color::RESET),
            DiagKind::UnexpectedTok((found, before, expected)) => {
                if expected.is_empty() {
                    write!(f, "unexpected token '{}{}{}'", Color::HIGHLIGHT, found.lexeme_concrete(), Color::RESET)
                } else {
                    let mut msg = String::new();

                    for (i, tok) in expected.iter().enumerate() {
                        msg.push_str(&format!("'{}{}{}'", Color::HIGHLIGHT, tok.lexeme_general(), Color::RESET));

                        if i + 2 < expected.len() {
                            msg.push_str(", ");
                        } else if i + 2 == expected.len() {
                            msg.push_str(" or ");
                        }
                    }

                    write!(f, "expected {} {} '{}{}{}'", msg, if *before { "before" } else { "after" }, Color::HIGHLIGHT, found.lexeme_concrete(), Color::RESET)
                }
            },
        }
    }
}
