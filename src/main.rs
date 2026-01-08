use std::{cell::RefCell, fmt, rc::Rc};

use crate::{
    cli::Cli,
    style::Style,

    writer::Tracer,
    result::AnalysisResult,

    target::Target,
    reporter::Reporter,

    lexer::{Lexer, LexerCore},
    symtable::{StrPool, SymTable, SymTableCore},
    parser::{Parser, ParserCore},
};

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
mod result;


fn dump_err(err: impl fmt::Display) {
    eprintln!("{}error: {}{}", Style::Red, Style::Reset, err)
}

fn finish<'a, I>(mut comp: Box<dyn Tracer<I> + 'a>) -> Result<(), AnalysisResult> {
    comp.before_drop().transpose().map(|_| ()).map_err(|err| {
        dump_err(err);
        AnalysisResult::IOError
    })
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

    if !reporter.borrow().found_err() {
        if let Err(result) = finish(parser)   { return result }
        if let Err(result) = finish(symtable) { return result }
        if let Err(result) = finish(lexer)    { return result }
    }

    reporter.borrow().finish();

    AnalysisResult::Success
}

fn main() -> AnalysisResult {
    analyze(Cli::parse_args())
}
