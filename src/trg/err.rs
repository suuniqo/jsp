use std::{io, fmt};


pub enum TargetErr {
    WrongExt(String),
    OpenFailed(io::Error),
    EmptyFile,
}

impl fmt::Display for TargetErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TargetErr::WrongExt(str) => write!(f, "wrong file extension: found {} but expected `txt` or `javascript`",
                if str.is_empty() { "none".to_string() } else { format!("`{}`", str)}
            ),
            TargetErr::OpenFailed(e) => write!(f, "failed to open target file: {}", e),
            TargetErr::EmptyFile => write!(f, "target file is empty"),
        }
    }
}
