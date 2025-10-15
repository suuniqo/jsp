use std::{io, fs, fmt};

use super::WriterErr;


pub struct Writer {
    file: Box<dyn io::Write>,
}

impl Writer {
    pub fn new(path: Option<&str>) -> Result<Self, WriterErr> {
        let file: Box<dyn io::Write> = if let Some(path) = path {
            Box::new(
                fs::OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(path)
                .map_err(|e| WriterErr::Io((e, path.to_string())))?
            )
        } else {
            Box::new(io::stdout())
        };

        Ok(Self { file } )
    }

    pub fn write<T: fmt::Display>(&mut self, item: &T) -> Result<(), WriterErr> {
        writeln!(self.file, "{}", item)
            .map_err(|e| WriterErr::Format(e))?;

        Ok(())
    }
}
