use std::{io, fmt};


pub enum TargetErr {
    WrongExt(String),
    OpenFailed(io::Error),
    EmptyFile,
}

impl fmt::Display for TargetErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TargetErr::WrongExt(str) => writeln!(f, "wrong file extension: found '{}' but expected 'txt' or 'javascript'", str),
            TargetErr::OpenFailed(e) => writeln!(f, "error opening target file: {}", e),
            TargetErr::EmptyFile => writeln!(f, "target file is empty"),
        }
    }
}
