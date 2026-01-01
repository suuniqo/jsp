use std::{collections::{HashMap, hash_map::Entry}, fmt};

use crate::{langtype::LangType, span::Span, symtable::pool::StrPool};

use super::Sym;


/// Stores the scope symbols, mapping each
/// lexeme pool position to the scope position
pub struct Scope {
    idx: usize,
    vec: Vec<Sym>,
    map: HashMap<usize, usize>,
}

impl Scope {
    pub fn new(idx: usize) -> Self {
        let scope = Self {
            idx,
            vec: Vec::new(),
            map: HashMap::new(),
        };

        scope
    }

    pub fn intern(&mut self, pool_id: usize, ltype: LangType, cause: Span) -> (bool, Sym) {
        match self.map.entry(pool_id) {
            Entry::Occupied(entry) => (false, self.vec[*entry.get()].clone()),
            Entry::Vacant(entry) => {
                let len = self.vec.len();

                let offset = if let Some(last) = self.vec.last() {
                    last.offset + last.ltype.size()
                } else {
                    0
                };

                entry.insert(len);
                self.vec.push(Sym::new(ltype, offset, pool_id, cause));

                (true, self.vec[len].clone())
            },
        }
    }

    /// Searches an entry by it's pool id
    /// There can't be collisions as variables can't be redeclared
    pub fn get(&self, pos: usize) -> Option<&Sym> {
        self.vec.get(pos)
    }

    pub fn fmt(&self, f: &mut fmt::Formatter<'_>, pool: &StrPool) -> fmt::Result {
        writeln!(f, "table: #{}:", self.idx)?;

        self.vec
            .iter()
            .try_for_each(|sym| sym.fmt(f, pool))?;

        Ok(())
    }
}
