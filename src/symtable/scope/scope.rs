use std::{collections::{HashMap, hash_map::Entry}, fmt};

use crate::{langtype::LangType, span::Span, symtable::pool::StrPool};

use super::Sym;


/// Stores the scope symbols, mapping each
/// lexeme pool position to the scope position
#[derive(Debug)]
pub struct Scope {
    idx: usize,
    id: Option<usize>,
    vec: Vec<Sym>,
    map: HashMap<usize, usize>,
}

impl Scope {
    pub fn new(idx: usize, id: Option<usize>) -> Self {
        let scope = Self {
            idx,
            id,
            vec: Vec::new(),
            map: HashMap::new(),
        };

        scope
    }

    pub fn id(&self) -> Option<usize> {
        self.id
    }

    pub fn change_type(&mut self, pool_id: usize, lang_type: LangType) {
        let pos = self.map.get(&pool_id)
            .expect("failed to fetch an id to change type");

        self.vec[*pos].lang_type = lang_type;
    }

    pub fn intern(&mut self, pool_id: usize, lang_type: LangType, span: Option<Span>) -> (bool, Sym) {
        match self.map.entry(pool_id) {
            Entry::Occupied(entry) => (false, self.vec[*entry.get()].clone()),
            Entry::Vacant(entry) => {
                let len = self.vec.len();

                let offset = if let Some(last) = self.vec.last() {
                    last.offset + last.lang_type.size()
                } else {
                    0
                };

                entry.insert(len);
                self.vec.push(Sym::new(lang_type, offset, pool_id, span));

                (true, self.vec[len].clone())
            },
        }
    }

    /// Searches an entry by it's pool id
    /// There can't be collisions as variables can't be redeclared
    pub fn get(&self, pos: usize) -> Option<&Sym> {
        self.vec.get(pos)
    }

    pub fn find(&self, pool_id: usize) -> Option<&Sym> {
        let pos = self.map.get(&pool_id)?;

        self.get(*pos)
    }

    pub fn fmt(&self, f: &mut fmt::Formatter<'_>, pool: &StrPool) -> fmt::Result {
        writeln!(f, "table #{}:", self.idx)?;

        self.vec
            .iter()
            .try_for_each(|sym| sym.fmt(f, pool))?;

        Ok(())
    }
}
