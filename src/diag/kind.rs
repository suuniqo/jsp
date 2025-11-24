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
    UnexpectedTok((TokenKind, Vec<TokenKind>))
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
            DiagKind::UnexpectedTok((_, expected)) => {
                if expected.is_empty() {
                    return "here".to_string();
                }

                let mut msg = String::from("expected one of ");

                for (i, tok) in expected.iter().enumerate() {
                    msg.push_str(&format!("'{}'", tok.lexeme()));

                    if i != expected.len() - 1 {
                        msg.push_str(", ");
                    }
                }

                msg
            },
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
            DiagKind::UnexpectedTok((kind, _)) => write!(f, "unexpected token '{}{}{}'", Color::HIGHLIGHT, kind.lexeme(), Color::RESET),
        }
    }
}
