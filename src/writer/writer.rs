use std::{io, fs, fmt};

use super::WriterErr;


pub struct Writer {
    path: Option<String>,
    file: Option<Box<dyn io::Write>>,
}

impl Writer {
    pub fn new(path: Option<&str>) -> Self {
        Self { path: path.map(|str| str.into()), file: None }
    }

    pub fn write(&mut self, args: fmt::Arguments) -> Result<(), WriterErr> {
        let file = self.file.get_or_insert({
            if let Some(path) = &self.path {
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
            }
        });

        write!(file, "{}", args)
            .map_err(|e| WriterErr::Write(e))?;

        Ok(())
    }
}
