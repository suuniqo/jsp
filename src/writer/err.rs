use std::fmt;


#[derive(Debug)]
pub enum WriterErr {
    Io((std::io::Error, String)),
    Format(std::io::Error),
}

impl fmt::Display for WriterErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io((e, path)) => write!(f, "{}: I/O error: {}", path, e),
            Self::Format(e) => write!(f, "Formatting error: {}", e),
        }
    }
}
