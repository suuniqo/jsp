use crate::token::TokenKind;

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

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum GramSym {
    T(TokenKind),
    NT(NotTerm),
}

pub type Rule = (NotTerm, &'static [GramSym]);

pub const GRAMMAR: &[Rule] = &[
    (NotTerm::PP, &[GramSym::NT(NotTerm::P)]),
    (NotTerm::P, &[]),
    (NotTerm::P, &[GramSym::NT(NotTerm::F), GramSym::NT(NotTerm::P)]),
    (NotTerm::P, &[GramSym::NT(NotTerm::B), GramSym::NT(NotTerm::P)]),
    (NotTerm::C, &[]),
    (NotTerm::C, &[GramSym::NT(NotTerm::B), GramSym::NT(NotTerm::C)]),

    (NotTerm::K, &[]),
    (NotTerm::K, &[GramSym::T(TokenKind::Comma), GramSym::NT(NotTerm::T), GramSym::T(TokenKind::Id(0)), GramSym::NT(NotTerm::K)]),

    (NotTerm::A, &[GramSym::T(TokenKind::Void)]),
    (NotTerm::A, &[GramSym::NT(NotTerm::T), GramSym::T(TokenKind::Id(0)), GramSym::NT(NotTerm::K)]),

    (NotTerm::H, &[GramSym::T(TokenKind::Void)]),
    (NotTerm::H, &[GramSym::NT(NotTerm::T)]),

    (NotTerm::F3, &[GramSym::T(TokenKind::LParen), GramSym::NT(NotTerm::A), GramSym::T(TokenKind::RParen)]),
    (NotTerm::F2, &[GramSym::T(TokenKind::Id(0))]),
    (NotTerm::F1, &[GramSym::NT(NotTerm::H)]),

    (NotTerm::F, &[GramSym::T(TokenKind::Func), GramSym::NT(NotTerm::F1), GramSym::NT(NotTerm::F2), GramSym::NT(NotTerm::F3), GramSym::T(TokenKind::LBrack), GramSym::NT(NotTerm::C), GramSym::T(TokenKind::RBrack)]),

    (NotTerm::X, &[]),
    (NotTerm::X, &[GramSym::NT(NotTerm::E)]),

    (NotTerm::M, &[]),

    (NotTerm::T, &[GramSym::T(TokenKind::Str)]),
    (NotTerm::T, &[GramSym::T(TokenKind::Bool)]),
    (NotTerm::T, &[GramSym::T(TokenKind::Float)]),
    (NotTerm::T, &[GramSym::T(TokenKind::Int)]),

    (NotTerm::B, &[GramSym::T(TokenKind::Do), GramSym::T(TokenKind::LBrack), GramSym::NT(NotTerm::C), GramSym::T(TokenKind::RBrack), GramSym::T(TokenKind::While), GramSym::T(TokenKind::LParen), GramSym::NT(NotTerm::E), GramSym::T(TokenKind::RParen), GramSym::T(TokenKind::Semi)]),
    (NotTerm::B, &[GramSym::T(TokenKind::Let), GramSym::NT(NotTerm::M), GramSym::NT(NotTerm::T), GramSym::T(TokenKind::Id(0)), GramSym::T(TokenKind::AndAssign), GramSym::NT(NotTerm::E), GramSym::T(TokenKind::Semi)]),
    (NotTerm::B, &[GramSym::T(TokenKind::Let), GramSym::NT(NotTerm::M), GramSym::NT(NotTerm::T), GramSym::T(TokenKind::Id(0)), GramSym::T(TokenKind::Assign), GramSym::NT(NotTerm::E), GramSym::T(TokenKind::Semi)]),
    (NotTerm::B, &[GramSym::T(TokenKind::Let), GramSym::NT(NotTerm::M), GramSym::NT(NotTerm::T), GramSym::T(TokenKind::Id(0)), GramSym::T(TokenKind::Semi)]),
    (NotTerm::B, &[GramSym::T(TokenKind::If), GramSym::T(TokenKind::LParen), GramSym::NT(NotTerm::E), GramSym::T(TokenKind::RParen), GramSym::NT(NotTerm::S)]),
    (NotTerm::B, &[GramSym::NT(NotTerm::S)]),

    (NotTerm::Q, &[]),
    (NotTerm::Q, &[GramSym::T(TokenKind::Comma), GramSym::NT(NotTerm::E), GramSym::NT(NotTerm::Q)]),

    (NotTerm::L, &[]),
    (NotTerm::L, &[GramSym::NT(NotTerm::E), GramSym::NT(NotTerm::Q)]),

    (NotTerm::S, &[GramSym::T(TokenKind::Ret), GramSym::NT(NotTerm::X), GramSym::T(TokenKind::Semi)]),
    (NotTerm::S, &[GramSym::T(TokenKind::Read), GramSym::T(TokenKind::Id(0)), GramSym::T(TokenKind::Semi)]),
    (NotTerm::S, &[GramSym::T(TokenKind::Write), GramSym::NT(NotTerm::E), GramSym::T(TokenKind::Semi)]),
    (NotTerm::S, &[GramSym::T(TokenKind::Id(0)), GramSym::T(TokenKind::LParen), GramSym::NT(NotTerm::L), GramSym::T(TokenKind::RParen), GramSym::T(TokenKind::Semi)]),
    (NotTerm::S, &[GramSym::T(TokenKind::Id(0)), GramSym::T(TokenKind::AndAssign), GramSym::NT(NotTerm::E), GramSym::T(TokenKind::Semi)]),
    (NotTerm::S, &[GramSym::T(TokenKind::Id(0)), GramSym::T(TokenKind::Assign), GramSym::NT(NotTerm::E), GramSym::T(TokenKind::Semi)]),

    (NotTerm::V, &[GramSym::T(TokenKind::Id(0))]),
    (NotTerm::V, &[GramSym::T(TokenKind::False)]),
    (NotTerm::V, &[GramSym::T(TokenKind::True)]),
    (NotTerm::V, &[GramSym::T(TokenKind::StrLit(String::new()))]),
    (NotTerm::V, &[GramSym::T(TokenKind::FloatLit(0.0))]),
    (NotTerm::V, &[GramSym::T(TokenKind::IntLit(0))]),
    (NotTerm::V, &[GramSym::T(TokenKind::LParen), GramSym::NT(NotTerm::E), GramSym::T(TokenKind::RParen)]),
    (NotTerm::V, &[GramSym::T(TokenKind::Id(0)), GramSym::T(TokenKind::LParen), GramSym::NT(NotTerm::L), GramSym::T(TokenKind::RParen)]),

    (NotTerm::EE, &[GramSym::NT(NotTerm::V)]),
    (NotTerm::EE, &[GramSym::T(TokenKind::Not), GramSym::NT(NotTerm::EE)]),
    (NotTerm::EE, &[GramSym::T(TokenKind::Sum), GramSym::NT(NotTerm::EE)]),
    (NotTerm::EE, &[GramSym::T(TokenKind::Sub), GramSym::NT(NotTerm::EE)]),

    (NotTerm::UU, &[GramSym::NT(NotTerm::EE)]),
    (NotTerm::UU, &[GramSym::NT(NotTerm::UU), GramSym::T(TokenKind::Mul), GramSym::NT(NotTerm::EE)]),

    (NotTerm::U, &[GramSym::NT(NotTerm::UU)]),
    (NotTerm::U, &[GramSym::NT(NotTerm::U), GramSym::T(TokenKind::Sum), GramSym::NT(NotTerm::UU)]),

    (NotTerm::RR, &[GramSym::NT(NotTerm::U)]),
    (NotTerm::RR, &[GramSym::NT(NotTerm::RR), GramSym::T(TokenKind::Lt), GramSym::NT(NotTerm::U)]),

    (NotTerm::R, &[GramSym::NT(NotTerm::RR)]),
    (NotTerm::R, &[GramSym::NT(NotTerm::R), GramSym::T(TokenKind::Eq), GramSym::NT(NotTerm::RR)]),

    (NotTerm::E, &[GramSym::NT(NotTerm::R)]),
    (NotTerm::E, &[GramSym::NT(NotTerm::E), GramSym::T(TokenKind::And), GramSym::NT(NotTerm::R)]),
];
