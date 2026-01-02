use std::fmt;

use crate::token::TokenKind;


#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NotTerm {
    E,
    R,
    RR,
    U,
    UU,
    EE,
    V,
    S,
    L,
    Q,
    X,
    B,
    T,
    M,
    F,
    F1,
    F2,
    F3,
    H,
    A,
    K,
    C,
    P,
    PP,
}

impl fmt::Display for NotTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let repr = match self {
            NotTerm::E => "expression",
            NotTerm::R => "expression",
            NotTerm::RR => "expression",
            NotTerm::U => "expression",
            NotTerm::UU => "expression",
            NotTerm::EE => "expression",
            NotTerm::V => "expression",
            NotTerm::S => "statement",
            NotTerm::L => "arguments",
            NotTerm::Q => "arg",
            NotTerm::X => "expression",
            NotTerm::B => "statement",
            NotTerm::T => "type",
            NotTerm::M => return write!(f, ""),
            NotTerm::F => "function",
            NotTerm::F1 => "type",
            NotTerm::F2 => "name",
            NotTerm::F3 => return write!(f, "(parameters)"),
            NotTerm::H => "type",
            NotTerm::A => "parameters",
            NotTerm::K => "parameter declaration",
            NotTerm::C => "statement",
            NotTerm::P => "block",
            NotTerm::PP => "program",
        };

        write!(f, "{}", repr)
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Term {
    If,
    Do,
    While,
    Int,
    Float,
    Str,
    Bool,
    Void,
    Let,
    Func,
    Ret,

    Read,
    Write,
    True,
    False,

    FloatLit,
    IntLit,
    StrLit,
    Id,

    Assign,
    AndAssign,

    Comma,
    Semi,
    LParen,
    RParen,
    LBrack,
    RBrack,

    Sub,
    Sum,
    Mul,

    And,
    Not,

    Lt,
    Eq,
}

impl Term {
    pub fn from_token_kind(kind: &TokenKind) -> Option<Self> {
        Some(match kind {
            TokenKind::If => Term::If,
            TokenKind::Do => Term::Do,
            TokenKind::While => Term::While,
            TokenKind::Int => Term::Int,
            TokenKind::Float => Term::Float,
            TokenKind::Str => Term::Str,
            TokenKind::Bool => Term::Bool,
            TokenKind::Void => Term::Void,
            TokenKind::Let => Term::Let,
            TokenKind::Func => Term::Func,
            TokenKind::Ret => Term::Ret,
            TokenKind::Read => Term::Read,
            TokenKind::Write => Term::Write,
            TokenKind::True => Term::True,
            TokenKind::False => Term::False,
            TokenKind::FloatLit(_) => Term::FloatLit,
            TokenKind::IntLit(_) => Term::IntLit,
            TokenKind::StrLit(_) => Term::StrLit,
            TokenKind::Id(..) => Term::Id,
            TokenKind::Assign => Term::Assign,
            TokenKind::AndAssign => Term::AndAssign,
            TokenKind::Comma => Term::Comma,
            TokenKind::Semi => Term::Semi,
            TokenKind::LParen => Term::LParen,
            TokenKind::RParen => Term::RParen,
            TokenKind::LBrack => Term::LBrack,
            TokenKind::RBrack => Term::RBrack,
            TokenKind::Sub => Term::Sub,
            TokenKind::Sum => Term::Sum,
            TokenKind::Mul => Term::Mul,
            TokenKind::And => Term::And,
            TokenKind::Not => Term::Not,
            TokenKind::Lt => Term::Lt,
            TokenKind::Eq => Term::Eq,
            TokenKind::Eof => return None,
        })
    }

    pub fn is_keyword(&self) -> bool {
        match self {
            Self::If
            | Self::Do
            | Self::While
            | Self::Int
            | Self::Float
            | Self::Str
            | Self::Bool
            | Self::Void
            | Self::Let
            | Self::Func
            | Self::Ret
            | Self::Read
            | Self::Write
            | Self::True
            | Self::False => true,
            _ => false,
        }
    }

    pub fn is_left_delim(&self) -> bool {
        matches!(self, Self::LBrack | Self::LParen)
    }

    pub fn is_right_delim(&self) -> bool {
        matches!(self, Self::RBrack | Self::RParen)
    }

    pub fn delim_match(&self, other: &Self) -> bool {
        *self == Self::LParen && *other == Self::RParen
            || *self == Self::LBrack && *other == Self::RBrack
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let repr = match self {
            Term::Bool => "boolean",
            Term::Do => "do",
            Term::Float => "float",
            Term::Func => "function",
            Term::If => "if",
            Term::Int => "int",
            Term::Let => "let",
            Term::Read => "read",
            Term::Ret => "return",
            Term::Str => "string",
            Term::Void => "void",
            Term::While => "while",
            Term::Write => "write",
            Term::True => "true",
            Term::False => "false",

            Term::FloatLit => "float literal",
            Term::IntLit => "int literal",
            Term::StrLit => "string literal",
            Term::Id => "identifier",

            Term::Assign => "=",
            Term::AndAssign => "&=",
            Term::Comma => ",",
            Term::Semi => ";",
            Term::LParen => "(",
            Term::RParen => ")",
            Term::LBrack => "{",
            Term::RBrack => "}",

            Term::Sum => "+",
            Term::Sub => "-",
            Term::Mul => "*",

            Term::And => "&&",
            Term::Not => "!",
            Term::Lt => "<",
            Term::Eq => "==",
        };

        write!(f, "{}", repr)
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[allow(dead_code)]
pub enum GramSym {
    T(Term),
    N(NotTerm),
}

impl fmt::Display for GramSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GramSym::T(term) => write!(f, "{}", term),
            GramSym::N(not_term) => write!(f, "{}", not_term),
        }
    }
}

