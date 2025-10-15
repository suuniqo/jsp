use std::{env, process};

use crate::{context::{diag::DiagManager, symtable::SymTable, Context}, lexer::{LexerTracer, Lexer}, target::Target};

mod token;
mod target;
mod context;
mod tracer;
mod lexer;


fn fetch_args() -> (String, String, Option<String>) {
    let mut args = env::args();

    let name = args
        .next()
        .unwrap_or_else(|| unreachable!("couldn't fetch program name"));

    let src = args
        .next()
        .unwrap_or_else(|| {
            eprintln!("{name}: usage: {name} source_file [destination_file]");
            process::exit(1);
        });

    let dst_tok = args
        .next();

    (name, src, dst_tok)
}

fn main() {
    let (name, src, dst_tok) = fetch_args();

    let target = Target::from_path(src)
        .unwrap_or_else(|err| {
            eprintln!("{}: {}", name, err);
            process::exit(1);
        });

    let mut ctx = Context::new(
        SymTable::new(),
        DiagManager::new(&target),
    );

    let lexer = Lexer::new(&mut ctx, &target);

    let consumer = LexerTracer::new(lexer, dst_tok.as_deref())
        .unwrap_or_else(|err| {
            eprintln!("{}: {}", name, err);
            process::exit(1);
        });

    consumer.for_each(|_| {});

    ctx.diags.dump();
}
