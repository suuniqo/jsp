use std::{collections::HashSet, fmt};

use super::{Term, NotTerm, GramSym, Grammar};


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MetaSym {
    Stmnt,
    Expr,
    
    FuncBlock,
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

    Func,
    While,
    If,
    Do,
    Let,
    Read,
    Write,
    Ret,

    OperBinary,
    OperUnary,
}

impl MetaSym {
    pub fn build_expected(term: HashSet<Term>, non_term: HashSet<NotTerm>) -> HashSet<MetaSym> {
        let mut redundant = HashSet::new();

        for nt in non_term.iter().filter(|nt| !matches!(nt, NotTerm::P | NotTerm::PP)) {
            let begs: HashSet<GramSym> = Grammar::RULES
                .iter()
                .filter(|(lhs, _)| lhs == nt)
                .filter_map(|(_, rhs)| rhs.first().cloned())
                .filter(|sym| *sym != GramSym::N(*nt))
                .collect();

            redundant.extend(begs);
        }

        let found: HashSet<GramSym> = term
            .into_iter()
            .map(GramSym::T)
            .chain(non_term.into_iter().map(GramSym::N))
            .collect();

        let expected = found.difference(&redundant)
            .filter_map(Self::from_sym)
            .collect();

        Self::clean(expected)
    }

    pub fn from_insert(term: &Term) -> Self {
        if *term == Term::Void {
            Self::FuncParam
        } else {
            Self::from_term(term)
        }
    }

    pub fn build_insertion(terminals: Vec<Term>) -> Vec<Self> {
        terminals.iter().map(Self::from_insert).collect()
    }

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

    fn clean(mut set: HashSet<Self>) -> HashSet<Self> {
        if set.contains(&Self::Expr) {
            set.clear();
            set.insert(Self::Expr);
        }

        if set.contains(&Self::Write)
            && set.contains(&Self::Read)
            && set.contains(&Self::Let)
            && set.contains(&Self::Do)
            && set.contains(&Self::If) {

            set.remove(&Self::Write);
            set.remove(&Self::Read);
            set.remove(&Self::Let);
            set.remove(&Self::Do);
            set.remove(&Self::If);
            set.remove(&Self::Ret);
            set.remove(&Self::Id);
            set.insert(Self::Stmnt);
        }

        if set.contains(&Self::RParen) {
            set.remove(&Self::OperBinary);
            set.remove(&Self::LParen);
        }

        if set.contains(&Self::Func) && set.contains(&Self::Stmnt) {
            set.remove(&Self::Func);
            set.insert(Self::FuncBlock);
        }

        set
    }

    pub fn show_before(&self) -> bool {
        match self {
            MetaSym::Stmnt => true,
            MetaSym::FuncBlock => true,
            MetaSym::Expr => true,
            MetaSym::FuncArgs => false,
            MetaSym::FuncParams => false,
            MetaSym::FuncType => false,
            MetaSym::FuncId => false,
            MetaSym::FuncParam => false,
            MetaSym::Type => true,
            MetaSym::Id => false,
            MetaSym::Assign => false,
            MetaSym::Comma => false,
            MetaSym::Semi => false,
            MetaSym::LParen => false,
            MetaSym::RParen => false,
            MetaSym::LBrack => false,
            MetaSym::RBrack => false,
            MetaSym::Func => true,
            MetaSym::While => false,
            MetaSym::If => true,
            MetaSym::Do => true,
            MetaSym::Let => true,
            MetaSym::Read => true,
            MetaSym::Write => true,
            MetaSym::Ret => true,
            MetaSym::OperBinary => false,
            MetaSym::OperUnary => false,
        }

    }

    pub fn are_spaced(curr: &MetaSym, next: &MetaSym) -> bool {
            !matches!( curr,
               MetaSym::LParen
               | MetaSym::LBrack
            ) && !matches!(
               next,
               MetaSym::RParen
               | MetaSym::RBrack
               | MetaSym::Semi
               | MetaSym::Comma
            )
    }
}

pub struct Quoted<'a>(pub &'a MetaSym);

impl fmt::Display for Quoted<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self.0 {
            MetaSym::Stmnt => "a statement",
            MetaSym::FuncBlock => "a function",
            MetaSym::Expr => "an expression",
            MetaSym::Type => "a type",
            MetaSym::Id => "an identifier",
            MetaSym::Assign => "an assignment",
            MetaSym::Comma => "`,`",
            MetaSym::Semi => "`;`",
            MetaSym::LParen => "`(`",
            MetaSym::RParen => "`)`",
            MetaSym::LBrack => "`{`",
            MetaSym::RBrack => "`}`",
            MetaSym::OperBinary => "a binary operator",
            MetaSym::OperUnary => "an unary operator",
            MetaSym::FuncArgs => "an argument list",
            MetaSym::FuncParams => "a parameter list",
            MetaSym::FuncType => "a return type",
            MetaSym::FuncId => "a function name",
            MetaSym::FuncParam => "a parameter",
            MetaSym::Func => "`function`",
            MetaSym::While => "`while`",
            MetaSym::If => "`if`",
            MetaSym::Do => "`do`",
            MetaSym::Let => "`let`",
            MetaSym::Read => "`read`",
            MetaSym::Write => "`write`",
            MetaSym::Ret => "`return`",
        })
    }
}

pub struct Insert<'a>(pub &'a MetaSym);

impl fmt::Display for Insert<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self.0 {
            MetaSym::Stmnt => "statement",
            MetaSym::FuncBlock => "function void foo(void) {}",
            MetaSym::Expr => "expression",
            MetaSym::FuncArgs => "(foo)",
            MetaSym::FuncParams => "(void)",
            MetaSym::FuncType => "type",
            MetaSym::FuncId => "foo",
            MetaSym::FuncParam => "void",
            MetaSym::Type => "type",
            MetaSym::Id => "foo",
            MetaSym::Assign => "=",
            MetaSym::Comma => ",",
            MetaSym::Semi => ";",
            MetaSym::LParen => "(",
            MetaSym::RParen => ")",
            MetaSym::LBrack => "{",
            MetaSym::RBrack => "}",
            MetaSym::Func => "function",
            MetaSym::While => "while",
            MetaSym::If => "if",
            MetaSym::Do => "do",
            MetaSym::Let => "let",
            MetaSym::Read => "read",
            MetaSym::Write => "write",
            MetaSym::Ret => "return",
            MetaSym::OperBinary => "*",
            MetaSym::OperUnary => "-",
        },)
    }
}

pub struct Insertion<'a>(
    pub Option<MetaSym>,
    pub &'a Vec<MetaSym>,
    pub Option<MetaSym>
);

impl fmt::Display for Insertion<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Insertion(before, syms, after) = self;

        let Some(first) = syms.first() else {
            return Ok(());
        };
        
        if before.as_ref().is_some_and(|before| MetaSym::are_spaced(before, first)) {
            write!(f, " ")?;
        }

        for w in syms.windows(2) {
            let curr = &w[0];
            let next = &w[1];

            write!(f, "{}", Insert(curr))?;

            if MetaSym::are_spaced(curr, next) {
                write!(f, " ")?;
            }
        }

        if let Some(last) = syms.last() {
            write!(f, "{}", Insert(last))?;

            if after.as_ref().is_some_and(|after| MetaSym::are_spaced(last, after)) {
                write!(f, " ")?;
            }
        }

        Ok(())
    }
}
