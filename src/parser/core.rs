use crate::{context::{Context, symtable::SymTable}, lexer::Lexer};

pub struct ParserCore<'t, 'c, T: SymTable, L: Lexer> {
    lexer: L,
    ctx: &'c mut Context<'t, T>
}
