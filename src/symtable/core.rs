use std::{rc::Rc, collections::{HashMap, hash_map::Entry}};

use super::{symbol::Symbol, traits::SymTable};


pub struct SymTableCore {
    vec: Vec<Symbol>,
    map: HashMap<Symbol, usize>,

}

impl SymTableCore {
    pub fn new() -> Self {
        Self {
            vec: Vec::new(),
            map: HashMap::new(),
        }
    }
}

impl SymTable for SymTableCore {
    fn intern(&mut self, lexeme: &str) -> (usize, Rc<str>) {
        let symbol = Symbol::from_lexeme(lexeme);

        match self.map.entry(symbol.clone()) {
            Entry::Occupied(entry) => (*entry.get(), symbol.lexeme()),
            Entry::Vacant(entry) => {
                let pos = self.vec.len();
                let ptr = entry.key().clone();

                self.vec.push(ptr);
                entry.insert(pos);

                (pos, symbol.lexeme())
            },
        }
    }

    fn get(&self, pos: usize) -> Option<&Symbol> {
        self.vec.get(pos)
    }
}
