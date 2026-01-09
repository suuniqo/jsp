use std::fmt;


#[derive(Debug)]
pub enum WriterErr {
    Io((std::io::Error, String)),
    Write(std::io::Error),
}

impl fmt::Display for WriterErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io((e, path)) => write!(f, "failed to open {}: {}", path, e),
            Self::Write(e) => write!(f, "failed to write on screen: {}", e),
        }
    }
}
