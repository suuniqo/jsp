use std::fmt;
use std::{cell::RefCell, process, rc::Rc};

use crate::cli::Cli;
use crate::lexer::{Lexer, LexerCore};
use crate::parser::{Parser, ParserCore};
use crate::reporter::Reporter;
use crate::style::Style;
use crate::symtable::{StrPool, SymTable, SymTableCore};
use crate::target::Target;
use crate::writer::HasTracer;

mod target;
mod window;
mod lexer;
mod parser;
mod reporter;
mod symtable;
mod writer;
mod style;
mod token;
mod diag;
mod span;
mod langtype;
mod grammar;
mod cli;


fn dump_err(err: impl fmt::Display) {
    eprintln!("{}error: {}{}", Style::Red, Style::Reset, err)
}

fn make_symtabl(trace: &Option<Option<String>>, inner: SymTableCore) -> Box<dyn SymTable> {
    if let Some(file) = trace {
        inner.tracer(file.as_deref())
            .unwrap_or_else(|err| {
                dump_err(err);
                process::exit(1);
            })
    } else {
        Box::new(inner)
    }
}

fn make_lexer<'t>(trace: &Option<Option<String>>, inner: LexerCore<'t>) -> Box<dyn Lexer + 't> {
    if let Some(file) = trace {
        inner.tracer(file.as_deref())
            .unwrap_or_else(|err| {
                dump_err(err);
                process::exit(1);
            })
    } else {
        Box::new(inner)
    }
}

fn make_parser<'t: 'l, 'l: 's, 's>(trace: &Option<Option<String>>, inner: ParserCore<'t, 'l, 's>) -> Box<dyn Parser + 's> {
    if let Some(file) = trace {
        inner.tracer(file.as_deref())
            .unwrap_or_else(|err| {
                dump_err(err);
                process::exit(1);
            })
    } else {
        Box::new(inner)
    }
}

fn analyze_source(cli: &Cli) {
    let target = Target::from_path(&cli.source)
        .unwrap_or_else(|err| {
            dump_err(err);
            process::exit(1);
        });

    let reporter = Rc::new(RefCell::new(Reporter::new(&target, cli.quiet)));
    let strpool = Rc::new(RefCell::new(StrPool::new()));

    let lexer = LexerCore::new(Rc::clone(&reporter), Rc::clone(&strpool), &target);
    let mut lexer = make_lexer(&cli.lexer_trace, lexer);

    let symtable = SymTableCore::new(strpool);
    let mut symtable = make_symtabl(&cli.symtb_trace, symtable);

    let parser = ParserCore::new(Rc::clone(&reporter), lexer.as_mut(), symtable.as_mut());
    let mut parser = make_parser(&cli.parse_trace, parser);

    parser.parse();

    if !reporter.borrow().found_err() {
        parser.before_drop();   drop(parser);
        symtable.before_drop(); drop(symtable);
        lexer.before_drop();    drop(lexer);
    }

    reporter.borrow().finnish();
}

fn main() {
    analyze_source(&Cli::parse_args());
}
