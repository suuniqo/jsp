use std::{fmt, error};


#[derive(Debug)]
pub enum TracerErr {
    Io((std::io::Error, String)),
    Write(std::io::Error),
}

impl error::Error for TracerErr {}

impl fmt::Display for TracerErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io((e, path)) => write!(f, "failed to open {}: {}", path, e),
            Self::Write(e) => write!(f, "failed to write on screen: {}", e),
        }
    }
}
