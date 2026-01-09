use std::{rc::Rc, collections::{HashMap, hash_map::Entry}};

use crate::tok::TokenKind;


#[derive(PartialEq, Eq)]
pub struct StrPool {
    vec: Vec<Rc<str>>,
    map: HashMap<Rc<str>, usize>,
}

impl StrPool {
    pub fn new() -> Self {
        Self {
            vec: Vec::new(),
            map: HashMap::new(),
        }
    }

    pub fn intern(&mut self, lexeme: &str) -> usize {
        match self.map.entry(Rc::from(lexeme)) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let pos = self.vec.len();
                let rc = entry.key().clone();

                self.vec.push(rc.clone());
                entry.insert(pos);

                pos
            },
        }
    }

    pub fn get(&self, pos: usize) -> Option<&Rc<str>> {
        self.vec.get(pos)
    }
}

impl TokenKind {
    pub fn lexeme(&self, pool: &StrPool) -> String {
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
            TokenKind::Id(pool_id) => return pool.get(*pool_id).expect("identifier with wrong pool id").to_string(),

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

