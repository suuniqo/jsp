use std::fmt;

use crate::{token::TokenKind, metasym::MetaSym};


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
        matches!(self, Self::If
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
            | Self::False)
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

impl MetaSym {
    pub fn as_term(&self) -> Option<Term> {
        Some(match self {
            MetaSym::Id => Term::Id,
            MetaSym::Assign => Term::Assign,
            MetaSym::Comma => Term::Comma,
            MetaSym::Semi => Term::Semi,
            MetaSym::LParen => Term::LParen,
            MetaSym::RParen => Term::RParen,
            MetaSym::LBrack => Term::LBrack,
            MetaSym::RBrack => Term::RBrack,
            MetaSym::While => Term::While,
            MetaSym::If => Term::If,
            MetaSym::Do => Term::Do,
            MetaSym::Let => Term::Let,
            MetaSym::Read => Term::Read,
            MetaSym::Write => Term::Write,
            MetaSym::Ret => Term::Ret,
            MetaSym::Func => Term::Func,
            _ => return None,
        })
    }

    pub fn from_term(term: &Term) -> Self {
        match term {
            Term::If => Self::If,
            Term::Do => Self::Do,
            Term::Let => Self::Let,
            Term::Read => Self::Read,
            Term::Write => Self::Write,
            Term::Func => Self::Func,
            Term::While => Self::While,
            Term::Ret => Self::Ret,

            Term::Int
            | Term::Float
            | Term::Str
            | Term::Bool
            | Term::Void => Self::Type,

            Term::True
            | Term::False
            | Term::FloatLit
            | Term::IntLit
            | Term::StrLit => Self::Expr,

            Term::Id => Self::Id,

            Term::Assign
            | Term::AndAssign => Self::Assign,

            Term::Comma => Self::Comma,
            Term::Semi => Self::Semi,
            Term::LParen => Self::LParen,
            Term::RParen => Self::RParen,
            Term::LBrack => Self::LBrack,
            Term::RBrack => Self::RBrack,

            Term::Mul
            | Term::Sum
            | Term::And
            | Term::Lt
            | Term::Eq => Self::OperBinary,

            Term::Sub
            | Term::Not => Self::OperUnary,
        }
    }

    pub fn from_not_term(not_term: &NotTerm) -> Option<Self> {
        Some(match not_term {
            NotTerm::E
            | NotTerm::R
            | NotTerm::RR
            | NotTerm::U
            | NotTerm::UU
            | NotTerm::EE
            | NotTerm::V => Self::Expr,

            NotTerm::S
            | NotTerm::C
            | NotTerm::B => Self::Stmnt,


            NotTerm::F1
            | NotTerm::H => Self::FuncType,

            NotTerm::F3
            | NotTerm::A => Self::FuncParams,

            NotTerm::L => Self::FuncArgs,
            NotTerm::T => Self::Type,
            NotTerm::F => Self::FuncBlock,
            NotTerm::F2 => Self::FuncId,
            NotTerm::K => Self::FuncParam,

            NotTerm::Q
            | NotTerm::P
            | NotTerm::PP
            | NotTerm::X
            | NotTerm::M => return None,
        })
    }

    pub fn from_sym(sym: &GramSym) -> Option<Self> {
        match sym {
            GramSym::T(term) => Some(Self::from_term(term)),
            GramSym::N(not_term) => Self::from_not_term(not_term),
        }
    }
}
