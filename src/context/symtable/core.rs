use std::collections::{HashMap, hash_map::Entry};

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

    fn get(&self, pos: usize) -> Option<&Symbol> {
        self.vec.get(pos)
    }
}
