use std::{fs, path::Path, str};

use super::TargetErr;

pub struct Target {
    path: String,
    src: String,
    offsets: Vec<usize>,
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

        let src = fs::read_to_string(&path)
            .map_err(|e| TargetErr::OpenFailed(e))?;

        if src.is_empty() {
            return Err(TargetErr::EmptyFile);
        }

        let mut offsets = vec![0];

        for (i, byte) in src.bytes().enumerate() {
            if byte == b'\n' {
                offsets.push(i+1);
            }
        }

        if *offsets.last().unwrap() != src.len() {
            offsets.push(src.len());
        }

        Ok(Self {
            path,
            src,
            offsets,
        })
    }

    pub fn path(&self) -> &str {
        &self.path
    }

    pub fn src(&self) -> &str {
        self.src.as_str()
    }

    pub fn line_offsets(&self) -> &Vec<usize> {
        &self.offsets
    }

    pub fn nth_line(&self, idx: usize) -> Option<(&str, usize)> {
        if idx >= self.offsets.len() - 1 {
            None
        } else {
            Some((&self.src[self.offsets[idx]..self.offsets[idx+1]], self.offsets[idx]))
        }
    }
}
