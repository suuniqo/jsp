use std::error;

use crate::{
    cli::Config,
    style::Style,

    result::AnalysisResult,
    factory::Factory,

    target::Target,
};

mod cli;
mod style;

mod result;
mod factory;

mod pool;
mod target;
mod reporter;

mod lexer;
mod parser;
mod symtable;

mod token;
mod diag;
mod span;
mod tracer;

mod types;
mod metasym;


fn dump_err(err: &dyn error::Error) {
    eprintln!("{}error: {}{}", Style::Red, Style::Reset, err)
}

fn analyze(target: &Target, config: &Config) -> AnalysisResult {
    let pool = Factory::pool();

    let reporter = Factory::reporter(target, pool.clone(), config.quiet);

    let mut lexer = Factory::lexer(
        &config.lexer_trace, reporter.clone(), pool.clone(), target,
    );

    let mut symtable = Factory::symtable(
        &config.symtb_trace, pool,
    );

    let mut parser = Factory::parser(
        &config.parse_trace, reporter.clone(), lexer.as_mut(), symtable.as_mut(),
    );

    parser.parse();

    let failure = reporter.borrow().finish();

    if let Err(err) = parser.finish(failure) {
        dump_err(err.as_ref());
        return AnalysisResult::IOError;
    }

    if let Err(err) = symtable.finish(failure) {
        dump_err(err.as_ref());
        return AnalysisResult::IOError;
    }

    if let Err(err) = lexer.finish(failure) {
        dump_err(err.as_ref());
        return AnalysisResult::IOError;
    }

    if failure.is_none() {
        AnalysisResult::Success
    } else {
        AnalysisResult::CodeError
    }
}

fn main() -> AnalysisResult {
    let config = Config::build();

    let mut result  = AnalysisResult::Success;
    let mut sources = config.sources.iter();

    while let Some(path) = sources.next()
        && result == AnalysisResult::Success
    {
        let target = match Target::from_path(path) {
            Ok(target) => target,
            Err(err) => {
                dump_err(&err);
                return AnalysisResult::IOError;
            }
        };

        result = analyze(&target, &config);
    }

    result
}
