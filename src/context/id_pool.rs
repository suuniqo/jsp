use std::{collections::HashMap, rc::Rc};

pub struct IdPool {
    vec: Vec<Rc<[u8]>>,
    map: HashMap<Rc<[u8]>, usize>,
}

impl IdPool {
    pub fn new(kws: &[&[u8]]) -> Self {
        let mut pool = Self {
            vec: Vec::with_capacity(kws.len()),
            map: HashMap::new(),
        };

        for kw in kws {
            pool.intern(kw);
        }

        pool
    }

    pub fn intern(&mut self, bytes: &[u8]) -> usize {
        if let Some(&pos) = self.map.get(bytes) {
            return pos;
        }

        let pos = self.vec.len();
        let ptr = Rc::from(bytes);

        self.vec.push(Rc::clone(&ptr));
        self.map.insert(ptr, pos);

        pos
    }

    pub fn get(&self, pos: usize) -> Option<&Rc<[u8]>> {
        self.vec.get(pos)
    }
}
