use std::{cell::RefCell, fmt, rc::Rc};

use crate::{
    cli::Config,
    style::Style,
    result::AnalysisResult,

    trg::Target,
    pool::StrPool,
    report::Reporter,

    lexer::{Lexer, LexerCore},
    parser::{Parser, ParserCore},
    symtable::{SymTable, SymTableCore},
};

mod cli;
mod style;
mod result;

mod trg;
mod pool;
mod report;

mod lexer;
mod parser;
mod symtable;

mod tok;
mod diag;
mod span;
mod write;

mod ltype;
mod gram;


fn dump_err(err: impl fmt::Display) {
    eprintln!("{}error: {}{}", Style::Red, Style::Reset, err)
}

fn analyze(config: Config) -> AnalysisResult {
    let target = match Target::from_path(&config.source) {
        Ok(target) => target,
        Err(err) => {
            dump_err(err);
            return AnalysisResult::IOError;
        }
    };

    let strpool = Rc::new(RefCell::new(StrPool::new()));

    let reporter = Rc::new(RefCell::new(Reporter::new(&target, strpool.clone(), config.quiet)));

    let mut lexer = <dyn Lexer>::make(
        &config.lexer_trace,
        LexerCore::new(Rc::clone(&reporter), Rc::clone(&strpool), &target),
    );

    let mut symtable = <dyn SymTable>::make(
        &config.symtb_trace,
        SymTableCore::new(strpool)
    );

    let mut parser = <dyn Parser>::make(
        &config.parse_trace,
        ParserCore::new(Rc::clone(&reporter), lexer.as_mut(), symtable.as_mut()),
    );

    parser.parse();

    let level = reporter.borrow().finish();

    if let Err(err) = parser.finish(level.valid_syntactic()) {
        dump_err(err);
        return AnalysisResult::IOError;
    }

    if let Err(err) = symtable.finish(level.valid_semantic()) {
        dump_err(err);
        return AnalysisResult::IOError;
    }

    if let Err(err) = lexer.finish(level.valid_lexical()) {
        dump_err(err);
        return AnalysisResult::IOError;
    }

    if level.valid_all() {
        AnalysisResult::Success
    } else {
        AnalysisResult::CodeError
    }
}

fn main() -> AnalysisResult {
    analyze(Config::build())
}
