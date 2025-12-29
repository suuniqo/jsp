use std::{collections::HashSet, fmt};

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

impl NotTerm {
    fn show_before(&self) -> bool {
        match self {
            NotTerm::E => false,
            NotTerm::R => false,
            NotTerm::RR => false,
            NotTerm::U => false,
            NotTerm::UU => false,
            NotTerm::EE => false,
            NotTerm::V => false,
            NotTerm::A => false,
            NotTerm::F1 => false,
            NotTerm::F2 => false,
            NotTerm::F3 => false,
            _ => true,
        }
    }
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
            TokenKind::Id(_) => Term::Id,
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

    fn show_before(&self) -> bool {
        match self {
            Term::If => true,
            Term::Do => true,
            Term::While => false,
            Term::Int => true,
            Term::Float => true,
            Term::Str => true,
            Term::Bool => true,
            Term::Void => true,
            Term::Let => true,
            Term::Func => true,
            Term::Ret => true,
            Term::Read => true,
            Term::Write => true,
            Term::True => true,
            Term::False => true,
            Term::FloatLit => true,
            Term::IntLit => true,
            Term::StrLit => true,
            Term::Id => true,
            Term::Assign => true,
            Term::AndAssign => true,
            Term::Comma => false,
            Term::Semi => false,
            Term::LParen => false,
            Term::RParen => false,
            Term::LBrack => false,
            Term::RBrack => false,
            Term::Sub => true,
            Term::Sum => true,
            Term::Mul => true,
            Term::And => true,
            Term::Not => true,
            Term::Lt => true,
            Term::Eq => true,
        }
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

impl GramSym {
    pub fn show_before(&self) -> bool {
        match self {
            GramSym::T(term) => term.show_before(),
            GramSym::N(not_term) => not_term.show_before(),
        }
    }

    pub fn abstracted(terminals: Vec<Term>) -> Vec<GramSym> {
        let mut abstracted = Vec::new();

        for term in terminals {
            if matches!(term, Term::IntLit | Term::FloatLit | Term::StrLit | Term::True | Term::False) {
                abstracted.push(GramSym::N(NotTerm::E));
                continue;
            }

            if matches!(term, Term::Bool | Term::Int | Term::Float | Term::Str) {
                abstracted.push(GramSym::N(NotTerm::T));
                continue;
            }

            if matches!(term, Term::Void) {
                abstracted.push(GramSym::N(NotTerm::A));
                continue;
            }

            abstracted.push(GramSym::T(term));
        }

        abstracted
    }

    pub fn from_token_kind(kind: &TokenKind) -> Option<Self> {
        Some(GramSym::T(Term::from_token_kind(kind)?))
    }

    pub fn are_spaced(curr: &GramSym, next: &GramSym) -> bool {
            !matches!(
               curr,
               GramSym::N(NotTerm::M)
               | GramSym::T(Term::LParen)
               | GramSym::T(Term::LBrack)
            ) && !matches!(
               next,
               GramSym::T(Term::RParen)
               | GramSym::T(Term::RBrack)
               | GramSym::T(Term::Semi)
               | GramSym::T(Term::Comma)
               | GramSym::N(NotTerm::M)
            )
    }

    pub fn space(syms: &Vec<GramSym>) -> String {
        let mut fmtted = String::new();

        for w in syms.windows(2) {
            let curr = &w[0];
            let next = &w[1];

            fmtted.push_str(&curr.to_string());

            if Self::are_spaced(curr, next) {
                fmtted.push(' ');
            }
        }

        if let Some(last) = syms.last() {
            fmtted.push_str(&last.to_string());
        }

        fmtted
    }
}

impl fmt::Display for GramSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GramSym::T(term) => write!(f, "{}", term),
            GramSym::N(not_term) => write!(f, "{}", not_term),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MetaSym {
    Stmnt,
    Func,
    Expr,
    
    FuncArgs,
    FuncParams,
    FuncType,
    FuncId,
    FuncParam,

    Type,
    Id,
    Assign,

    Comma,
    Semi,
    LParen,
    RParen,
    LBrack,
    RBrack,
    While,

    OperBinary,
    OperUnary,
}

impl MetaSym {
    pub fn build_expected(term: HashSet<Term>, non_term: HashSet<NotTerm>) -> HashSet<MetaSym> {
        let mut redundant = HashSet::new();

        for nt in non_term.iter().filter(|nt| !matches!(nt, NotTerm::P | NotTerm::PP)) {
            let begs: HashSet<GramSym> = GRAMMAR
                .iter()
                .filter(|(lhs, _)| lhs == nt)
                .filter_map(|(_, rhs)| rhs.first().cloned())
                .filter(|sym| *sym != GramSym::N(*nt))
                .collect();

            redundant.extend(begs);
        }

        let found: HashSet<GramSym> = term
            .into_iter()
            .map(|t| GramSym::T(t))
            .chain(non_term.into_iter().map(|nt| GramSym::N(nt)))
            .collect();

        let expected = found.difference(&redundant)
            .into_iter()
            .filter_map(|sym| Self::from_sym(sym))
            .collect();

        Self::clean(expected)
    }

    pub fn from_term(term: &Term) -> Option<Self> {
        Some(match term {
            Term::If
            | Term::Do
            | Term::Let
            | Term::Ret
            | Term::Read
            | Term::Write => Self::Stmnt,

            Term::Func => Self::Func,

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
            Term::While => Self::While,

            Term::Mul
            | Term::And
            | Term::Lt
            | Term::Eq => Self::OperBinary,

            Term::Sub
            | Term::Not => Self::OperUnary,

            Term::Sum => return None,
        })
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
            NotTerm::F => Self::Func,
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
            GramSym::T(term) => Self::from_term(&term),
            GramSym::N(not_term) => Self::from_not_term(&not_term),
        }
    }

    fn clean(mut set: HashSet<Self>) -> HashSet<Self> {
        if set.contains(&Self::Semi) {
            set.clear();
            set.insert(Self::Semi);
        } else if set.contains(&Self::RParen) {
            set.clear();
            set.insert(Self::RParen);
        } else if set.contains(&Self::Expr) {
            set.clear();
            set.insert(Self::Expr);
        }

        set
    }
}

impl fmt::Display for MetaSym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let repr = match self {
            MetaSym::Stmnt => "statement",
            MetaSym::Expr => "expression",
            MetaSym::Type => "variable type",
            MetaSym::Id => "variable name",
            MetaSym::Assign => "assignment",
            MetaSym::Comma => ",",
            MetaSym::Semi => ";",
            MetaSym::LParen => "(",
            MetaSym::RParen => ")",
            MetaSym::LBrack => "{",
            MetaSym::RBrack => "}",
            MetaSym::OperBinary => "binary operator",
            MetaSym::OperUnary => "unary operator",
            MetaSym::Func => "function",
            MetaSym::FuncArgs => "argument list",
            MetaSym::FuncParams => "parameter list",
            MetaSym::FuncType => "return type",
            MetaSym::FuncId => "function name",
            MetaSym::FuncParam => "parameter",
            MetaSym::While => "while",
        };

        write!(f, "{}", repr)
    }
}

pub type Rule = (NotTerm, &'static [GramSym]);

pub const GRAMMAR: &[Rule] = &[
    (NotTerm::PP, &[GramSym::N(NotTerm::P)]),

    (NotTerm::P, &[]),
    (NotTerm::P, &[GramSym::N(NotTerm::F), GramSym::N(NotTerm::P)]),
    (NotTerm::P, &[GramSym::N(NotTerm::B), GramSym::N(NotTerm::P)]),

    (NotTerm::C, &[]),
    (NotTerm::C, &[GramSym::N(NotTerm::B), GramSym::N(NotTerm::C)]),

    (NotTerm::K, &[]),
    (NotTerm::K, &[GramSym::T(Term::Comma), GramSym::N(NotTerm::T), GramSym::T(Term::Id), GramSym::N(NotTerm::K)]),

    (NotTerm::A, &[GramSym::T(Term::Void)]),
    (NotTerm::A, &[GramSym::N(NotTerm::T), GramSym::T(Term::Id), GramSym::N(NotTerm::K)]),

    (NotTerm::H, &[GramSym::T(Term::Void)]),
    (NotTerm::H, &[GramSym::N(NotTerm::T)]),

    (NotTerm::F3, &[GramSym::T(Term::LParen), GramSym::N(NotTerm::A), GramSym::T(Term::RParen)]),
    (NotTerm::F2, &[GramSym::T(Term::Id)]),
    (NotTerm::F1, &[GramSym::N(NotTerm::H)]),

    (NotTerm::F, &[GramSym::T(Term::Func), GramSym::N(NotTerm::F1), GramSym::N(NotTerm::F2), GramSym::N(NotTerm::F3), GramSym::T(Term::LBrack), GramSym::N(NotTerm::C), GramSym::T(Term::RBrack)]),

    (NotTerm::X, &[]),
    (NotTerm::X, &[GramSym::N(NotTerm::E)]),

    (NotTerm::M, &[]),

    (NotTerm::T, &[GramSym::T(Term::Str)]),
    (NotTerm::T, &[GramSym::T(Term::Bool)]),
    (NotTerm::T, &[GramSym::T(Term::Float)]),
    (NotTerm::T, &[GramSym::T(Term::Int)]),

    (NotTerm::B, &[GramSym::T(Term::Do), GramSym::T(Term::LBrack), GramSym::N(NotTerm::C), GramSym::T(Term::RBrack), GramSym::T(Term::While), GramSym::T(Term::LParen), GramSym::N(NotTerm::E), GramSym::T(Term::RParen), GramSym::T(Term::Semi)]),
    (NotTerm::B, &[GramSym::T(Term::Let), GramSym::N(NotTerm::M), GramSym::N(NotTerm::T), GramSym::T(Term::Id), GramSym::T(Term::Assign), GramSym::N(NotTerm::E), GramSym::T(Term::Semi)]),
    (NotTerm::B, &[GramSym::T(Term::Let), GramSym::N(NotTerm::M), GramSym::N(NotTerm::T), GramSym::T(Term::Id), GramSym::T(Term::Semi)]),
    (NotTerm::B, &[GramSym::T(Term::If), GramSym::T(Term::LParen), GramSym::N(NotTerm::E), GramSym::T(Term::RParen), GramSym::N(NotTerm::S)]),
    (NotTerm::B, &[GramSym::N(NotTerm::S)]),

    (NotTerm::Q, &[]),
    (NotTerm::Q, &[GramSym::T(Term::Comma), GramSym::N(NotTerm::E), GramSym::N(NotTerm::Q)]),

    (NotTerm::L, &[]),
    (NotTerm::L, &[GramSym::N(NotTerm::E), GramSym::N(NotTerm::Q)]),

    (NotTerm::S, &[GramSym::T(Term::Ret), GramSym::N(NotTerm::X), GramSym::T(Term::Semi)]),
    (NotTerm::S, &[GramSym::T(Term::Read), GramSym::T(Term::Id), GramSym::T(Term::Semi)]),
    (NotTerm::S, &[GramSym::T(Term::Write), GramSym::N(NotTerm::E), GramSym::T(Term::Semi)]),
    (NotTerm::S, &[GramSym::T(Term::Id), GramSym::T(Term::LParen), GramSym::N(NotTerm::L), GramSym::T(Term::RParen), GramSym::T(Term::Semi)]),
    (NotTerm::S, &[GramSym::T(Term::Id), GramSym::T(Term::AndAssign), GramSym::N(NotTerm::E), GramSym::T(Term::Semi)]),
    (NotTerm::S, &[GramSym::T(Term::Id), GramSym::T(Term::Assign), GramSym::N(NotTerm::E), GramSym::T(Term::Semi)]),

    (NotTerm::V, &[GramSym::T(Term::Id)]),
    (NotTerm::V, &[GramSym::T(Term::False)]),
    (NotTerm::V, &[GramSym::T(Term::True)]),
    (NotTerm::V, &[GramSym::T(Term::StrLit)]),
    (NotTerm::V, &[GramSym::T(Term::FloatLit)]),
    (NotTerm::V, &[GramSym::T(Term::IntLit)]),
    (NotTerm::V, &[GramSym::T(Term::LParen), GramSym::N(NotTerm::E), GramSym::T(Term::RParen)]),
    (NotTerm::V, &[GramSym::T(Term::Id), GramSym::T(Term::LParen), GramSym::N(NotTerm::L), GramSym::T(Term::RParen)]),

    (NotTerm::EE, &[GramSym::N(NotTerm::V)]),
    (NotTerm::EE, &[GramSym::T(Term::Not), GramSym::N(NotTerm::EE)]),

    (NotTerm::UU, &[GramSym::N(NotTerm::EE)]),
    (NotTerm::UU, &[GramSym::N(NotTerm::UU), GramSym::T(Term::Mul), GramSym::N(NotTerm::EE)]),

    (NotTerm::U, &[GramSym::N(NotTerm::UU)]),
    (NotTerm::U, &[GramSym::N(NotTerm::U), GramSym::T(Term::Sum), GramSym::N(NotTerm::UU)]),

    (NotTerm::RR, &[GramSym::N(NotTerm::U)]),
    (NotTerm::RR, &[GramSym::N(NotTerm::RR), GramSym::T(Term::Lt), GramSym::N(NotTerm::U)]),

    (NotTerm::R, &[GramSym::N(NotTerm::RR)]),
    (NotTerm::R, &[GramSym::N(NotTerm::R), GramSym::T(Term::Eq), GramSym::N(NotTerm::RR)]),

    (NotTerm::E, &[GramSym::N(NotTerm::R)]),
    (NotTerm::E, &[GramSym::N(NotTerm::E), GramSym::T(Term::And), GramSym::N(NotTerm::R)]),

    // So that draco works with unary operators
    (NotTerm::EE, &[GramSym::T(Term::Sum), GramSym::N(NotTerm::EE)]),
    (NotTerm::EE, &[GramSym::T(Term::Sub), GramSym::N(NotTerm::EE)]),
];
