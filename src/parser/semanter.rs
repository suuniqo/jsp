use crate::{diag::Diag, langtype::LangType, span::Span, symtable::SymTable, token::Token};

pub enum Attribute {
    /// Stores return type
    Unit(Option<LangType>),
    /// Stores pool id
    Id(usize),
    /// Stores type
    Type(LangType, Span),
}

pub struct Semanter<'s> {
    symtable: &'s mut dyn SymTable,
    stack: Vec<Attribute>
}

impl<'s> Semanter<'s> {
    pub fn new(symtable: &'s mut dyn SymTable) -> Self {
        Self {
            symtable,
            stack: Vec::new(),
        }
    }

    pub fn push(token: Token) {
        todo!()
    }

    pub fn reduce(rule: usize) -> Result<(), Diag> {
        todo!()
    }
}
