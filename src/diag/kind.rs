use std::{collections::HashSet, rc::Rc};

use crate::{grammar::{MetaSym, Quoted}, langtype::Type, token::TokenKind};

#[derive(Debug, Clone)]
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

    // semantic analyzer
    MismatchedRetType(Type, Type),
    UnexpectedRetType(Type),
    ExpectedRetType,
    Redefinition,
    MismatchedTypes(Type, Vec<Type>),
    UndefinedId(Rc<str>),
    StrayRet,
}

impl DiagKind {
    pub fn msg(&self) -> String {
        match self {
            DiagKind::StrayChar(_) => "here".into(),
            DiagKind::UntermComm => "started here".into(),
            DiagKind::UntermStr(_) => "started here".into(),
            DiagKind::MalformedStr(..) => "help: remove this character".into(),
            DiagKind::OverflowStr(str) => format!("length is {} but maximum is {}", str.len(), TokenKind::MAX_STR_LEN),
            DiagKind::InvEscSeq(c) => format!("interpreted as \\\\{}", c),
            DiagKind::OverflowInt => format!("maximum is {}", i16::MAX),
            DiagKind::OverflowFloat => format!("maximum is {:e}", f32::MAX),
            DiagKind::InvFmtFloat(_) => "expected digit after `.`".into(),
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
            DiagKind::MismatchedDelim(_) => "mismatched closing delimiter".into(),
            DiagKind::UnclosedDelim => "expected closing delimiter".into(),
            DiagKind::KeywordAsId(_) => "expected an identifier".into(),
            DiagKind::MissingSemi => "expected `;`".into(),
            DiagKind::TrailingCommaParam => "here".into(),
            DiagKind::TrailingCommaArg => "here".into(),
            DiagKind::MissingVarType => "expected type".into(),
            DiagKind::MissingRetType => "expected return type".into(),
            DiagKind::MissingParamList => "expected parameter list".into(),
            DiagKind::EmptyParamList => "empty parameter list".into(),
            DiagKind::MismatchedRetType(found, expected) => format!("expected `{expected}`, found `{found}`"),
            DiagKind::UnexpectedRetType(found) => format!("unexpected `{found}`"),
            DiagKind::ExpectedRetType => "no return statements found".into(),
            DiagKind::Redefinition => "later redefined".into(),
            DiagKind::MismatchedTypes(found, expected) => {
                if expected.len() == 0 {
                    "here".to_string()
                } else {
                    let mut msg = String::new();

                    for (i, sym) in expected.iter().enumerate() {
                        msg.push_str(&format!("`{}`", sym));

                        if i + 2 < expected.len() {
                            msg.push_str(", ");
                        } else if i + 2 == expected.len() {
                            msg.push_str(" or ");
                        }
                    }

                    format!("expected {msg}, found `{found}`")
                }
            },
            DiagKind::UndefinedId(_) => format!("help: try defining it first"),
            DiagKind::StrayRet => format!("cannot be used outside a function"),
        }
    }
}
