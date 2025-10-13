use crate::{context::{diagnostics::DiagManager, id_pool::IdPool, Context}, lexer::{consumer::LexerConsumer, Lexer}, target::Target, token::TokenKind};

use std::{env, process};

pub mod token;
pub mod target;
pub mod context;

pub mod lexer;

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

    let dst = args
        .next();

    (name, src, dst)
}

fn main() {
    let (name, src, dst) = fetch_args();

    let target = Target::from_path(src)
        .unwrap_or_else(|err| {
            eprintln!("{name}: {}", err.msg());
            process::exit(1);
        });

    let mut ctx = Context::new(
        IdPool::new(&TokenKind::KEYWORDS),
        DiagManager::new(&target),
    );

    let lexer = Lexer::new(&mut ctx, &target);

    let consumer = LexerConsumer::new(lexer, dst.as_deref())
        .unwrap_or_else(|err| {
            eprintln!("{name}: {}", err.msg());
            process::exit(1);
        });

    consumer.dump()
        .unwrap_or_else(|err| {
            eprintln!("{name}: {}", err.msg());
            process::exit(1);
        });

    ctx.diags.dump();
}
