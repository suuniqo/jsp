use std::{cell::RefCell, fmt, rc::Rc};

use crate::{
    cli::Cli,
    style::Style,
    result::AnalysisResult,

    target::Target,
    reporter::Reporter,
    strpool::StrPool,

    lexer::{Lexer, LexerCore},
    symtable::{SymTable, SymTableCore},
    parser::{Parser, ParserCore},
};

mod cli;
mod style;
mod result;

mod target;
mod reporter;
mod strpool;

mod lexer;
mod parser;
mod symtable;

mod writer;
mod token;
mod diag;
mod span;

mod langtype;
mod grammar;


fn dump_err(err: impl fmt::Display) {
    eprintln!("{}error: {}{}", Style::Red, Style::Reset, err)
}

fn analyze(cli: Cli) -> AnalysisResult {
    let target = match Target::from_path(&cli.source) {
        Ok(target) => target,
        Err(err) => {
            dump_err(err);
            return AnalysisResult::IOError;
        }
    };

    let strpool = Rc::new(RefCell::new(StrPool::new()));

    let reporter = Rc::new(RefCell::new(Reporter::new(&target, strpool.clone(), cli.quiet)));

    let mut lexer = <dyn Lexer>::make(
        &cli.lexer_trace,
        LexerCore::new(Rc::clone(&reporter), Rc::clone(&strpool), &target),
    );

    let mut symtable = <dyn SymTable>::make(
        &cli.symtb_trace,
        SymTableCore::new(strpool)
    );

    let mut parser = <dyn Parser>::make(
        &cli.parse_trace,
        ParserCore::new(Rc::clone(&reporter), lexer.as_mut(), symtable.as_mut()),
    );

    parser.parse();

    reporter.borrow().finish();

    if reporter.borrow().found_err() {
        return AnalysisResult::CodeError;
    }
    
    if let Err(err) = parser.finish() {
        dump_err(err);
        return AnalysisResult::IOError;
    }

    if let Err(err) = symtable.finish() {
        dump_err(err);
        return AnalysisResult::IOError;
    }

    if let Err(err) = lexer.finish() {
        dump_err(err);
        return AnalysisResult::IOError;
    }

    AnalysisResult::Success
}

fn main() -> AnalysisResult {
    analyze(Cli::parse_args())
}
