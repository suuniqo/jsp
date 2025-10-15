use std::{collections::{HashMap, hash_map::Entry}, rc::Rc};

use crate::{context::symtable::traits::SymTable, token::{TokenKind, KEYWORDS}};

use super::symbol::Symbol;


pub struct SymTableCore {
    vec: Vec<Symbol>,
    map: HashMap<Symbol, usize>,
    kws: HashMap<usize, &'static TokenKind>,

}

impl SymTableCore {
    pub fn with_keywords() -> Self {
        let mut symtable = Self {
            vec: Vec::with_capacity(TokenKind::KEYWORDS_LEN),
            map: HashMap::new(),
            kws: HashMap::new(),
        };

        for (i, kw) in KEYWORDS.iter().enumerate() {
            if let Some(lexeme) = kw.lexeme() {
                let lexeme = Rc::new(lexeme);

                symtable.intern(&lexeme);
                symtable.kws.insert(i, kw);
            }
        }

        symtable
    }

    pub(super) fn intern_ref(&mut self, bytes: &[u8]) -> ((Symbol, usize), bool) {
        match self.map.entry(Symbol::from_bytes(bytes)) {
            Entry::Occupied(entry) => ((entry.key().clone(), *entry.get()), false),
            Entry::Vacant(entry) => {
                let pos = self.vec.len();
                let ptr = entry.key().clone();
                let ret = (ptr.clone(), pos);

                self.vec.push(ptr);
                entry.insert(pos);

                (ret, true)
            },
        }
    }
}

impl SymTable for SymTableCore {
    fn intern(&mut self, bytes: &[u8]) -> usize {
        match self.map.entry(Symbol::from_bytes(bytes)) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let pos = self.vec.len();
                let ptr = entry.key().clone();

                self.vec.push(ptr);
                entry.insert(pos);

                pos
            },
        }
    }

    fn as_keyword(&self, pos: usize) -> Option<TokenKind> {
        self.kws
            .get(&pos)
            .map(|kw| *kw)
            .cloned()
    }
}
