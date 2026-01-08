use std::fmt;

use crate::diag::DiagSever;


pub enum Style {
    None,
    Reset,
    High,
    Red,
    Orange,
    Blue,
}

pub enum Color {
    Red,
    Orange,
    Blue,
    White,
}

impl Color {
    pub const fn code(&self) -> u8 {
        match self {
            Color::Red    => 211,   // 9
            Color::Orange => 217,   // 11
            Color::Blue   => 117,   // 12
            Color::White  => 189,   // 15
        }
    }
}

impl fmt::Display for Style {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Style::None => write!(f, ""),
            Style::Reset => write!(f, "\x1b[0m"),
            Style::High => write!(f, "\x1b[1;38;5;{}m", Color::White.code()),
            Style::Red => write!(f, "\x1b[1;38;5;{}m", Color::Red.code()),
            Style::Orange => write!(f, "\x1b[1;38;5;{}m", Color::Orange.code()),
            Style::Blue => write!(f, "\x1b[1;38;5;{}m", Color::Blue.code()),
        }
    }
}

impl DiagSever {
    pub fn color(&self) -> Style {
        match self {
            DiagSever::Warning => Style::Orange,
            DiagSever::Error => Style::Red,
            DiagSever::Note => Style::Blue,
        }
    }
}
