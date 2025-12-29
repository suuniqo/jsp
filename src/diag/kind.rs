use std::{collections::HashSet, fmt};

use crate::{color::Color, grammar::MetaSym, token::TokenKind};

#[derive(Clone)]
pub enum DiagKind {
    // lexer
    StrayChar(char),
    UntermComm,
    UntermStr(String),
    MalformedStr(char, String),
    OverflowStr(String),
    InvEscSeq(char),
    OverflowInt,
    OverflowFloat,
    InvFmtFloat(f32),

    // parser
    UnexpectedTok(TokenKind, HashSet<MetaSym>),
    MismatchedDelim(TokenKind),
    UnclosedDelim,
    KeywordAsId(TokenKind),
    MissingSemi,
    TrailingComma,
    MissingVarType,
    MissingRetType,
    MissingParamList,
    EmptyParamList,
}

impl DiagKind {
    pub fn msg(&self) -> String {
        match self {
            DiagKind::StrayChar(_) => "here".to_string(),
            DiagKind::UntermComm => "started here".to_string(),
            DiagKind::UntermStr(_) => "started here".to_string(),
            DiagKind::MalformedStr(..) => "help: remove this character".to_string(),
            DiagKind::OverflowStr(str) => format!("length is {} but maximum is {}", str.len(), TokenKind::MAX_STR_LEN),
            DiagKind::InvEscSeq(c) => format!("interpreted as \\\\{}", c),
            DiagKind::OverflowInt => format!("maximum is {}", i16::MAX),
            DiagKind::OverflowFloat => format!("maximum is {:e}", f32::MAX),
            DiagKind::InvFmtFloat(_) => "expected digit after '.'".to_string(),
            DiagKind::UnexpectedTok(_, _) => "expected here".to_string(),
            DiagKind::MismatchedDelim(_) => "mismatched closing delimiter".to_string(),
            DiagKind::UnclosedDelim => "expected closing delimiter".to_string(),
            DiagKind::KeywordAsId(_) => "expected identifier".to_string(),
            DiagKind::MissingSemi => "expected `;`".to_string(),
            DiagKind::TrailingComma => "here".to_string(),
            DiagKind::MissingVarType => "expected type".to_string(),
            DiagKind::MissingRetType => "expected return type".to_string(),
            DiagKind::MissingParamList => "expected parameter list".to_string(),
            DiagKind::EmptyParamList => "empty parameter list".to_string(),
        }
    }
}

impl fmt::Display for DiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagKind::StrayChar(c) => write!(f, "illegal character `{}{}{}` in program", Color::HIGHLIGHT, c.escape_default(), Color::RESET),
            DiagKind::UntermComm => write!(f, "unterminated block comment"),
            DiagKind::UntermStr(_) => write!(f, "missing terminating character `{}\"{}` on string literal", Color::HIGHLIGHT, Color::RESET),
            DiagKind::OverflowStr(_) => write!(f, "string literal is too long"),
            DiagKind::InvEscSeq(c) => write!(f, "unknown escape sequence `{}\\{c}{}`", Color::HIGHLIGHT, Color::RESET),
            DiagKind::OverflowInt => write!(f, "integer literal out of range for 16-byte type"),
            DiagKind::OverflowFloat => write!(f, "float literal out of range for 32-byte type"),
            DiagKind::InvFmtFloat(_) => write!(f, "missing decimal part after `{}.{}` in float literal", Color::HIGHLIGHT, Color::RESET),
            DiagKind::MalformedStr(c, _) => write!(f, "malformed string literal, contains control character `{}{}{}`", Color::HIGHLIGHT, c.escape_default(), Color::RESET),
            DiagKind::UnexpectedTok(found, expected) => {
                if expected.is_empty() {
                    write!(f, "unexpected `{}{}{}`", Color::HIGHLIGHT, found.lexeme_concrete(), Color::RESET)
                } else {
                    let mut msg = String::new();

                    for (i, tok) in expected.iter().enumerate() {
                        msg.push_str(&format!("`{}{}{}`", Color::HIGHLIGHT, tok, Color::RESET));

                        if i + 2 < expected.len() {
                            msg.push_str(", ");
                        } else if i + 2 == expected.len() {
                            msg.push_str(" or ");
                        }
                    }

                    write!(
                        f,
                        "expected {}, found `{}{}{}`",
                        msg,
                        Color::HIGHLIGHT, found.lexeme_concrete(), Color::RESET
                    )
                }
            },
            DiagKind::MismatchedDelim(kind) => write!(
                f,
                "mismatched closing delimiter `{}{}{}`",
                Color::HIGHLIGHT, kind.lexeme_concrete(), Color::RESET),
            DiagKind::UnclosedDelim => write!(f, "unclosed delimiter"),
            DiagKind::KeywordAsId(kw) => write!(
                f,
                "keyword `{}{}{}` used as an identifier",
                Color::HIGHLIGHT, kw.lexeme_concrete(), Color::RESET),
            DiagKind::MissingSemi => write!(
                f,
                "missing `{};{}` at the end of a statement",
                Color::HIGHLIGHT,
                Color::RESET
            ),
            DiagKind::TrailingComma => write!(f, "trailing comma on a function parameter list"),
            DiagKind::MissingVarType => write!(f, "missing type in variable declaration"),
            DiagKind::MissingRetType => write!(f, "missing return type in function declaration"),
            DiagKind::MissingParamList => write!(f, "missing parameter list in function declaration"),
            DiagKind::EmptyParamList => write!(f, "empty parameter list in function declaration"),
        }
    }
}
