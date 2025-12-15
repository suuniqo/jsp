#[repr(u8)]
#[derive(Debug, Clone, Copy)]
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

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
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


#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum GramSym {
    T(Term),
    N(NotTerm),
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
