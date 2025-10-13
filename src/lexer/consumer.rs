use std::{fs::{File, OpenOptions}, io::Write};

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
    dump: File,
    lexer: Lexer<'t, 'c>
}

impl<'t, 'c> LexerConsumer<'t, 'c> {
    pub fn new(lexer: Lexer<'t, 'c>, dump_path: &str) -> Result<Self, LexerConsumerErr> {
        let dump = OpenOptions::new()
            .create(true)
            .write(true)
            .open(dump_path)
            .map_err(|e| LexerConsumerErr::FailedOpen(e.to_string()))?;

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
