use std::{rc::Rc, collections::{HashMap, hash_map::Entry}};


#[derive(PartialEq, Eq)]
pub struct StrPool {
    vec: Vec<Rc<str>>,
    map: HashMap<Rc<str>, usize>,
}

impl StrPool {
    pub fn new() -> Self {
        let scope = Self {
            vec: Vec::new(),
            map: HashMap::new(),
        };

        scope
    }

    pub fn intern(&mut self, lexeme: &str) -> (usize, Rc<str>) {
        match self.map.entry(Rc::from(lexeme)) {
            Entry::Occupied(entry) => (*entry.get(), entry.key().clone()),
            Entry::Vacant(entry) => {
                let pos = self.vec.len();
                let rc = entry.key().clone();

                self.vec.push(rc.clone());
                entry.insert(pos);

                (pos, rc)
            },
        }
    }

    pub fn get(&self, pos: usize) -> Option<&Rc<str>> {
        self.vec.get(pos)
    }
}
