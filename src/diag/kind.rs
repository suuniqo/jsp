use std::collections::HashSet;

use crate::{grammar::{MetaSym, Quoted}, token::TokenKind};

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
    TrailingCommaParam,
    TrailingCommaArg,
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
            DiagKind::InvFmtFloat(_) => "expected digit after `.`".to_string(),
            DiagKind::UnexpectedTok(_, expected) => {
                if expected.len() == 0 {
                    "here".to_string()
                } else if expected.len() == 1 && let Some(candidate) = expected.iter().next() {
                    format!("expected {}", Quoted(candidate))
                } else {
                    let mut msg = String::new();

                    for (i, sym) in expected.iter().enumerate() {
                        msg.push_str(&format!("{}", Quoted(sym)));

                        if i + 2 < expected.len() {
                            msg.push_str(", ");
                        } else if i + 2 == expected.len() {
                            msg.push_str(" or ");
                        }
                    }

                    format!("expected {msg}")
                }
            },
            DiagKind::MismatchedDelim(_) => "mismatched closing delimiter".to_string(),
            DiagKind::UnclosedDelim => "expected closing delimiter".to_string(),
            DiagKind::KeywordAsId(_) => "expected an identifier".to_string(),
            DiagKind::MissingSemi => "expected `;`".to_string(),
            DiagKind::TrailingCommaParam => "here".to_string(),
            DiagKind::TrailingCommaArg => "here".to_string(),
            DiagKind::MissingVarType => "expected type".to_string(),
            DiagKind::MissingRetType => "expected return type".to_string(),
            DiagKind::MissingParamList => "expected parameter list".to_string(),
            DiagKind::EmptyParamList => "empty parameter list".to_string(),
        }
    }
}
