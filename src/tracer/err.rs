use std::fmt;


pub enum TracerErr {
    Io((std::io::Error, String)),
    Format(std::io::Error),
}

impl fmt::Display for TracerErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io((e, path)) => write!(f, "{}: I/O error: {}", path, e),
            Self::Format(e) => write!(f, "Formatting error: {}", e),
        }
    }
}
