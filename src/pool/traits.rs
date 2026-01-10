use std::rc::Rc;

use crate::token::TokenKind;


pub trait PoolLookup: 'static {
    fn lookup(&self, pos: usize) -> Option<&Rc<str>>;
}

pub trait PoolInterner: 'static {
    fn intern(&mut self, lexeme: &str) -> usize;
}

impl TokenKind {
    pub fn lexeme(&self, pool: &impl PoolLookup) -> String {
        let str = match self {
            TokenKind::Bool => "boolean",
            TokenKind::Do => "do",
            TokenKind::Float => "float",
            TokenKind::Func => "function",
            TokenKind::If => "if",
            TokenKind::Int => "int",
            TokenKind::Let => "let",
            TokenKind::Read => "read",
            TokenKind::Ret => "return",
            TokenKind::Str => "string",
            TokenKind::Void => "void",
            TokenKind::While => "while",
            TokenKind::Write => "write",
            TokenKind::True => "true",
            TokenKind::False => "false",

            TokenKind::FloatLit(inner) => if *inner == f32::MAX {
                return self.lexeme_general().to_string()
            } else {
                return inner.to_string()
            },
            TokenKind::IntLit(inner) => if *inner == i16::MAX {
                return self.lexeme_general().to_string();
            } else {
                return inner.to_string();
            },
            TokenKind::StrLit(inner) => return format!("\"{}\"", inner),
            TokenKind::Id(pool_id) => return pool.lookup(*pool_id).expect("identifier with wrong pool id").to_string(),

            TokenKind::Assign => "=",
            TokenKind::AndAssign => "&=",
            TokenKind::Comma => ",",
            TokenKind::Semi => ";",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBrack => "{",
            TokenKind::RBrack => "}",

            TokenKind::Sum => "+",
            TokenKind::Sub => "-",
            TokenKind::Mul => "*",
            TokenKind::And => "&&",
            TokenKind::Not => "!",
            TokenKind::Lt => "<",
            TokenKind::Eq => "==",

            TokenKind::Eof => "end of file",
        };

        str.to_string()
    }
}
