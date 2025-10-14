use std::{io, fs, fmt};

use super::TracerErr;


pub struct Tracer {
    file: Box<dyn io::Write>,
}

impl Tracer {
    pub fn new(path: Option<&str>) -> Result<Self, TracerErr> {
        let file: Box<dyn io::Write> = if let Some(path) = path {
            Box::new(
                fs::OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(path)
                .map_err(|e| TracerErr::Io((e, path.to_string())))?
            )
        } else {
            Box::new(io::stdout())
        };

        Ok(Self { file } )
    }

    pub fn trace<T: fmt::Display>(&mut self, item: &T) -> Result<(), TracerErr> {
        writeln!(self.file, "{}", item)
            .map_err(|e| TracerErr::Format(e))?;

        Ok(())
    }
}
