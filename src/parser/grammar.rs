use crate::token::TokenKind;

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

#[derive(Debug, Clone)]
pub enum GramSym {
    T(TokenKind),
    NT(NotTerm),
    Lambda,
}

pub type Rule = (NotTerm, &'static [GramSym]);

pub const GRAMMAR: &[Rule] = &[
    (NotTerm::E, &[GramSym::NT(NotTerm::E), GramSym::T(TokenKind::And), GramSym::NT(NotTerm::R)]),
    (NotTerm::E, &[GramSym::NT(NotTerm::R)]),

    (NotTerm::R, &[GramSym::NT(NotTerm::R), GramSym::T(TokenKind::Eq), GramSym::NT(NotTerm::RR)]),
    (NotTerm::R, &[GramSym::NT(NotTerm::RR)]),

    (NotTerm::RR, &[GramSym::NT(NotTerm::RR), GramSym::T(TokenKind::Lt), GramSym::NT(NotTerm::U)]),
    (NotTerm::RR, &[GramSym::NT(NotTerm::U)]),

    (NotTerm::U, &[GramSym::NT(NotTerm::U), GramSym::T(TokenKind::Sum), GramSym::NT(NotTerm::UU)]),
    (NotTerm::U, &[GramSym::NT(NotTerm::UU)]),

    (NotTerm::UU, &[GramSym::NT(NotTerm::UU), GramSym::T(TokenKind::Mul), GramSym::NT(NotTerm::EE)]),
    (NotTerm::UU, &[GramSym::NT(NotTerm::EE)]),

    (NotTerm::EE, &[GramSym::T(TokenKind::Not), GramSym::NT(NotTerm::EE)]),
    (NotTerm::EE, &[GramSym::NT(NotTerm::V)]),

    (NotTerm::V, &[GramSym::T(TokenKind::Id(0)), GramSym::T(TokenKind::LParen), GramSym::NT(NotTerm::L), GramSym::T(TokenKind::RParen)]),
    (NotTerm::V, &[GramSym::T(TokenKind::LParen), GramSym::NT(NotTerm::E), GramSym::T(TokenKind::RParen)]),
    (NotTerm::V, &[GramSym::T(TokenKind::IntLit(0))]),
    (NotTerm::V, &[GramSym::T(TokenKind::FloatLit(0.0))]),
    (NotTerm::V, &[GramSym::T(TokenKind::StrLit(String::new()))]),
    (NotTerm::V, &[GramSym::T(TokenKind::True)]),
    (NotTerm::V, &[GramSym::T(TokenKind::False)]),
    (NotTerm::V, &[GramSym::T(TokenKind::Id(0))]),

    (NotTerm::S, &[GramSym::T(TokenKind::Id(0)), GramSym::T(TokenKind::Assign), GramSym::NT(NotTerm::E), GramSym::T(TokenKind::Semi)]),
    (NotTerm::S, &[GramSym::T(TokenKind::Id(0)), GramSym::T(TokenKind::AndAssign), GramSym::NT(NotTerm::E), GramSym::T(TokenKind::Semi)]),
    (NotTerm::S, &[GramSym::T(TokenKind::Id(0)), GramSym::T(TokenKind::LParen), GramSym::NT(NotTerm::L), GramSym::T(TokenKind::RParen), GramSym::T(TokenKind::Semi)]),
    (NotTerm::S, &[GramSym::T(TokenKind::Write), GramSym::NT(NotTerm::E), GramSym::T(TokenKind::Semi)]),
    (NotTerm::S, &[GramSym::T(TokenKind::Read), GramSym::T(TokenKind::Id(0)), GramSym::T(TokenKind::Semi)]),
    (NotTerm::S, &[GramSym::T(TokenKind::Ret), GramSym::NT(NotTerm::X), GramSym::T(TokenKind::Semi)]),

    (NotTerm::L, &[GramSym::NT(NotTerm::E), GramSym::NT(NotTerm::Q)]),
    (NotTerm::L, &[GramSym::Lambda]),

    (NotTerm::Q, &[GramSym::T(TokenKind::Comma), GramSym::NT(NotTerm::E), GramSym::NT(NotTerm::Q)]),
    (NotTerm::Q, &[GramSym::Lambda]),

    (NotTerm::B, &[GramSym::NT(NotTerm::S)]),
    (NotTerm::B, &[GramSym::T(TokenKind::If), GramSym::T(TokenKind::LParen), GramSym::NT(NotTerm::E), GramSym::T(TokenKind::RParen), GramSym::NT(NotTerm::S)]),
    (NotTerm::B, &[GramSym::T(TokenKind::Let), GramSym::NT(NotTerm::M), GramSym::NT(NotTerm::T), GramSym::T(TokenKind::Id(0)), GramSym::T(TokenKind::Semi)]),
    (NotTerm::B, &[GramSym::T(TokenKind::Let), GramSym::NT(NotTerm::M), GramSym::NT(NotTerm::T), GramSym::T(TokenKind::Id(0)), GramSym::T(TokenKind::Assign), GramSym::NT(NotTerm::E), GramSym::T(TokenKind::Semi)]),
    (NotTerm::B, &[GramSym::T(TokenKind::Let), GramSym::NT(NotTerm::M), GramSym::NT(NotTerm::T), GramSym::T(TokenKind::Id(0)), GramSym::T(TokenKind::AndAssign), GramSym::NT(NotTerm::E), GramSym::T(TokenKind::Semi)]),
    (NotTerm::B, &[GramSym::T(TokenKind::Do), GramSym::T(TokenKind::LBrack), GramSym::NT(NotTerm::C), GramSym::T(TokenKind::RBrack), GramSym::T(TokenKind::While), GramSym::T(TokenKind::LParen), GramSym::NT(NotTerm::E), GramSym::T(TokenKind::RParen), GramSym::T(TokenKind::Semi)]),

    (NotTerm::T, &[GramSym::T(TokenKind::Int)]),
    (NotTerm::T, &[GramSym::T(TokenKind::Float)]),
    (NotTerm::T, &[GramSym::T(TokenKind::Bool)]),
    (NotTerm::T, &[GramSym::T(TokenKind::Str)]),

    (NotTerm::M, &[GramSym::Lambda]),

    (NotTerm::X, &[GramSym::NT(NotTerm::E)]),
    (NotTerm::X, &[GramSym::Lambda]),

    (NotTerm::F, &[GramSym::T(TokenKind::Func), GramSym::NT(NotTerm::F1), GramSym::NT(NotTerm::F2), GramSym::NT(NotTerm::F3), GramSym::T(TokenKind::LBrack), GramSym::NT(NotTerm::C), GramSym::T(TokenKind::RBrack)]),

    (NotTerm::F1, &[GramSym::NT(NotTerm::H)]),
    (NotTerm::F2, &[GramSym::T(TokenKind::Id(0))]),
    (NotTerm::F3, &[GramSym::T(TokenKind::LParen), GramSym::NT(NotTerm::A), GramSym::T(TokenKind::RParen)]),

    (NotTerm::H, &[GramSym::NT(NotTerm::T)]),
    (NotTerm::H, &[GramSym::T(TokenKind::Void)]),

    (NotTerm::A, &[GramSym::NT(NotTerm::T), GramSym::T(TokenKind::Id(0)), GramSym::NT(NotTerm::K)]),
    (NotTerm::A, &[GramSym::T(TokenKind::Void)]),

    (NotTerm::K, &[GramSym::T(TokenKind::Comma), GramSym::NT(NotTerm::T), GramSym::T(TokenKind::Id(0)), GramSym::NT(NotTerm::K)]),
    (NotTerm::K, &[GramSym::Lambda]),


    (NotTerm::C, &[GramSym::NT(NotTerm::B), GramSym::NT(NotTerm::C)]),
    (NotTerm::C, &[GramSym::Lambda]),

    (NotTerm::P, &[GramSym::NT(NotTerm::B), GramSym::NT(NotTerm::P)]),
    (NotTerm::P, &[GramSym::NT(NotTerm::F), GramSym::NT(NotTerm::P)]),
    (NotTerm::P, &[GramSym::Lambda]),

    (NotTerm::PP, &[GramSym::NT(NotTerm::P)]),
];
