use std::{cell::RefCell, env, process, rc::Rc};

use crate::{lexer::{LexerCore, LexerTracer}, parser::{Parser, ParserCore, ParserTracer}, reporter::Reporter, symtable::{SymTableCore, SymTableTracer}, target::Target};

mod target;
mod window;
mod lexer;
mod parser;
mod reporter;
mod symtable;
mod writer;
mod color;
mod token;
mod diag;
mod span;


fn fetch_args() -> (String, String, Option<String>, Option<String>, Option<String>) {
    let mut args = env::args();

    let name = args
        .next()
        .expect("couldn't fetch program name");

    let src = args
        .next()
        .unwrap_or_else(|| {
            eprintln!("{name}: usage: {name} source_file [trace_dst_symtable [trace_dst_lexer [trace_dst_parser]]");
            process::exit(1);
        });

    let dst_sym = args
        .next();


    let dst_lexer = args
        .next();

    let dst_parser = args
        .next();

    (name, src, dst_sym, dst_lexer, dst_parser)
}

fn main() {
    let (name, src, dst_sym, dst_lexer, dst_parser) = fetch_args();

    let target = Target::from_path(src)
        .unwrap_or_else(|err| {
            eprintln!("{}: {}", name, err);
            process::exit(1);
        });

    let symtable = Rc::new(RefCell::new(
            SymTableTracer::new(SymTableCore::new(), dst_sym.as_deref())
                .unwrap_or_else(|err| {
                    eprintln!("{}: {}", name, err);
                    process::exit(1);
                })
        ));


    let reporter = Rc::new(RefCell::new(Reporter::new(&target)));

    let mut lexer = LexerTracer::new(LexerCore::new(Rc::clone(&reporter), Rc::clone(&symtable), &target), dst_lexer.as_deref())
        .unwrap_or_else(|err| {
            eprintln!("{}: {}", name, err);
            process::exit(1);
        });

    let mut parser = ParserTracer::new(ParserCore::new(Rc::clone(&reporter), &mut lexer), dst_parser.as_deref())
        .unwrap_or_else(|err| {
            eprintln!("{}: {}", name, err);
            process::exit(1);
        });

    parser.parse();
}
