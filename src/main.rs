use std::{env, process};

use crate::{context::{diag::DiagManager, symtable::{SymTableCore, SymTableTracer}, Context}, lexer::{LexerTracer, LexerCore}, target::Target};

mod token;
mod target;
mod context;
mod writer;
mod lexer;


fn fetch_args() -> (String, String, Option<String>, Option<String>) {
    let mut args = env::args();

    let name = args
        .next()
        .expect("couldn't fetch program name");

    let src = args
        .next()
        .unwrap_or_else(|| {
            eprintln!("{name}: usage: {name} source_file [destination_file]");
            process::exit(1);
        });

    let dst_sym = args
        .next();


    let dst_tok = args
        .next();

    (name, src, dst_sym, dst_tok)
}

fn main() {
    let (name, src, dst_sym, dst_tok) = fetch_args();

    let target = Target::from_path(src)
        .unwrap_or_else(|err| {
            eprintln!("{}: {}", name, err);
            process::exit(1);
        });

    let symtable = SymTableTracer::new(SymTableCore::with_keywords(), dst_sym.as_deref())
        .unwrap_or_else(|err| {
            eprintln!("{}: {}", name, err);
            process::exit(1);
        });


    let mut ctx = Context::new(
        symtable,
        DiagManager::new(&target),
    );

    let lexer = LexerTracer::new(LexerCore::new(&mut ctx, &target), dst_tok.as_deref())
        .unwrap_or_else(|err| {
            eprintln!("{}: {}", name, err);
            process::exit(1);
        });

    lexer.for_each(|_| {});
}
