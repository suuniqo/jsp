use std::{rc::Rc, collections::{HashMap, hash_map::Entry}};

use super::traits::{PoolLookup, PoolInterner};


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
}

impl PoolInterner for StrPool {
    fn intern(&mut self, lexeme: &str) -> usize {
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
}

impl PoolLookup for StrPool {
    fn lookup(&self, pos: usize) -> Option<&Rc<str>> {
        self.vec.get(pos)
    }
}
