use std::{fs, ops::RangeBounds, path::Path, str};

use super::TargetErr;

pub struct Target {
    path: String,
    src: String,
    offsets: Vec<usize>,
}

impl Target {
    const EXT: [&'static str; 2] = ["txt", "javascript"];

    pub fn from_path(path: &str) -> Result<Self, TargetErr> {
        let extension = match Path::new(&path).extension() {
            Some(ext) => ext.to_str().unwrap_or(""),
            None => "",
        };

        let src = fs::read_to_string(&path)
            .map_err(|e| TargetErr::OpenFailed(e))?;

        if !Self::EXT.contains(&extension) {
            return Err(TargetErr::WrongExt(extension.to_string()));
        }

        if src.is_empty() {
            return Err(TargetErr::EmptyFile);
        }

        let mut offsets = vec![0];

        for (i, byte) in src.bytes().enumerate() {
            if byte == b'\n' {
                offsets.push(i+1);
            }
        }

        if *offsets.last().expect("source file is empty") != src.len() {
            offsets.push(src.len());
        }

        Ok(Self {
            path: path.to_string(),
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


    pub fn slice<R>(&self, r: R) -> Option<&str>
    where
        R: RangeBounds<usize>,
    {
        let start = match r.start_bound() {
            std::ops::Bound::Included(&n) => n,
            std::ops::Bound::Excluded(&n) => n + 1,
            std::ops::Bound::Unbounded => 0,
        };

        let end = match r.end_bound() {
            std::ops::Bound::Included(&n) => n + 1,
            std::ops::Bound::Excluded(&n) => n,
            std::ops::Bound::Unbounded => self.src.len(),
        };

        self.src.get(start..end)
    }

    pub fn line_offsets(&self) -> &Vec<usize> {
        &self.offsets
    }

    pub fn nth_line(&self, idx: usize) -> Option<(&str, usize)> {
        if idx >= self.offsets.len() - 1 {
            None
        } else {
            let slice = &self.src[self.offsets[idx]..self.offsets[idx+1]];
            let slice = slice.strip_suffix('\n').unwrap_or(slice);

            Some((slice, self.offsets[idx]))
        }
    }

    pub fn lines(&self) -> usize {
        self.offsets.len() - 1
    }
}
