use std::{collections, io, fmt};


pub enum TargetErr {
    WrongExt(String),
    OpenFailed(io::Error),
    QueryFailed(io::Error),
    ReadFailed(io::Error),
    AllocFailed(collections::TryReserveError),
}

impl fmt::Display for TargetErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TargetErr::WrongExt(str) => writeln!(f, "wrong file extension: found '{}' but expected 'txt' or 'javascript'", str),
            TargetErr::OpenFailed(e) => writeln!(f, "error opening target file: {}", e),
            TargetErr::QueryFailed(e) => writeln!(f, "error querying target file: {}", e),
            TargetErr::ReadFailed(e) => writeln!(f, "error reading target file: {}", e),
            TargetErr::AllocFailed(e) => writeln!(f, "error storing target file contents: {}", e),
        }
    }
}
