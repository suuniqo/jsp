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
        let file = match &mut self.file {
            Some(file) => file,
            None => {
                let new_file: Box<dyn io::Write> = if let Some(path) = &self.path {
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

                self.file.insert(new_file)
            }
        };

        write!(file, "{}", args)
            .map_err(|e| WriterErr::Write(e))?;

        Ok(())
    }
}
