use std::{borrow, fs, io::Read, path::Path, str};

use super::TargetErr;

pub struct Target {
    path: String,
    bytes: Vec<u8>,
}

impl Target {
    const EXT: [&'static str; 2] = ["txt", "javascript"];

    pub fn from_path(path: String) -> Result<Self, TargetErr> {
        let extension = match Path::new(&path).extension() {
            Some(ext) => ext.to_str().unwrap_or(""),
            None => "",
        };

        if !Self::EXT.contains(&extension) {
            return Err(TargetErr::WrongExt(extension.to_string()));
        }

        let mut file = fs::File::open(&path)
            .map_err(|e| TargetErr::OpenFailed(e))?;

        let size = file
            .metadata()
            .map_err(|e| TargetErr::QueryFailed(e))?
            .len();

        let mut bytes = Vec::new();

        bytes.try_reserve_exact(size as usize)
             .map_err(|e| TargetErr::AllocFailed(e))?;

        file.read_to_end(&mut bytes)
            .map_err(|e| TargetErr::ReadFailed(e))?;

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
