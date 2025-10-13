use std::{fs::OpenOptions, io::{self, Write}};

use crate::lexer::Lexer;


pub enum LexerConsumerErr {
    FailedOpen(String),
    FailedWrite(String),
}

impl LexerConsumerErr {
    pub fn msg(&self) -> String {
        match self {
            LexerConsumerErr::FailedOpen(s) => format!("error opening destination file: {s}"),
            LexerConsumerErr::FailedWrite(s) => format!("error writing destination file: {s}"),
        }
    }
}

pub struct LexerConsumer<'t, 'c> {
    dump: Box<dyn Write>,
    lexer: Lexer<'t, 'c>
}

impl<'t, 'c> LexerConsumer<'t, 'c> {
    pub fn new(lexer: Lexer<'t, 'c>, dump_path: Option<&str>) -> Result<Self, LexerConsumerErr> {
        let dump: Box<dyn Write> = if let Some(dump_path) = dump_path {
            Box::new(
                OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(dump_path)
                .map_err(|e| LexerConsumerErr::FailedOpen(e.to_string()))?
            )
        } else {
            Box::new(io::stdout())
        };

        Ok(Self {
            lexer,
            dump,
        })
    }

    pub fn dump(mut self) -> Result<(), LexerConsumerErr> {
        for token in self.lexer.into_iter() {
            writeln!(self.dump, "{}", token.kind)
                .map_err(|e| LexerConsumerErr::FailedWrite(e.to_string()))?;
        }

        Ok(())
    }
}
