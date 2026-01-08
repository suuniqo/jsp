use std::{cell::RefCell, fmt, rc::Rc};

use crate::{
    cli::Cli, diag::DiagLevel, lexer::{Lexer, LexerCore}, parser::{Parser, ParserCore}, reporter::Reporter, result::AnalysisResult, strpool::StrPool, style::Style, symtable::{SymTable, SymTableCore}, target::Target
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

    let level = reporter.borrow().finish();

    if let Err(err) = parser.finish(DiagLevel::Syntactic, level) {
        dump_err(err);
        return AnalysisResult::IOError;
    }

    if let Err(err) = symtable.finish(DiagLevel::Semantic, level) {
        dump_err(err);
        return AnalysisResult::IOError;
    }

    if let Err(err) = lexer.finish(DiagLevel::Lexical, level) {
        dump_err(err);
        return AnalysisResult::IOError;
    }

    if level < DiagLevel::None {
        AnalysisResult::CodeError
    } else {
        AnalysisResult::Success
    }
}

fn main() -> AnalysisResult {
    analyze(Cli::parse_args())
}
