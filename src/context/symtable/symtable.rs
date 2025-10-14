use std::{collections::{HashMap, hash_map::Entry}, rc::Rc};

use crate::token::{TokenKind, KEYWORDS};


pub struct SymTable {
    vec: Vec<Rc<[u8]>>,
    map: HashMap<Rc<[u8]>, usize>,
    kws: HashMap<usize, &'static TokenKind>,
}

impl SymTable {
    pub fn new() -> Self {
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

    pub fn intern(&mut self, bytes: &[u8]) -> usize {
        match self.map.entry(Rc::from(bytes)) {
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

    pub fn as_keyword(&self, pos: usize) -> Option<TokenKind> {
        self.kws
            .get(&pos)
            .map(|kw| *kw)
            .cloned()
    }
}
