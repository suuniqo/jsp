use std::{fs, io::Read, str, borrow};

pub enum TargetErr {
    OpenFailed(String),
    QueryFailed(String),
    ReadFailed(String),
    AllocFailed(String),
}

impl TargetErr {
    pub fn msg(&self) -> String {
        match self {
            TargetErr::OpenFailed(s) => format!("error opening target file: {s}"),
            TargetErr::QueryFailed(s) => format!("error querying target file: {s}"),
            TargetErr::ReadFailed(s) => format!("error reading target file: {s}"),
            TargetErr::AllocFailed(s) => format!("error storing target file contents: {s}"),
        }
    }
}

pub struct Target {
    path: String,
    bytes: Vec<u8>,
}

impl Target {
    pub fn from_path(path: String) -> Result<Self, TargetErr> {

        let mut file = fs::File::open(&path)
            .map_err(|e| TargetErr::OpenFailed(e.to_string()))?;

        let size = file
            .metadata()
            .map_err(|e| TargetErr::QueryFailed(e.to_string()))?
            .len();

        let mut bytes = Vec::new();

        bytes.try_reserve_exact(size as usize)
             .map_err(|e| TargetErr::AllocFailed(e.to_string()))?;

        file.read_to_end(&mut bytes)
            .map_err(|e| TargetErr::ReadFailed(e.to_string()))?;

        Ok(Self {
            path,
            bytes,
        })
    }

    pub fn path(&self) -> &str {
        &self.path
    }

    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }

    pub fn nth_line(&self, idx: usize) -> Option<borrow::Cow<'_, str>> {
        self.bytes
            .split(|&b| b == b'\n')
            .nth(idx)
            .map(|bytes| String::from_utf8_lossy(bytes))
    }
}
