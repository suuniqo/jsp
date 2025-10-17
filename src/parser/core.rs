use crate::{context::{Context, symtable::SymTable}, lexer::Lexer};

struct ParserCore<'t, 'c, T: SymTable, L: Lexer> {
    lexer: L,
    ctx: &'c mut Context<'t, T>
}
