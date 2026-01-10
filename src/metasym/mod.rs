use std::fmt;


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
