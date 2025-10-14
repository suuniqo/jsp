use std::fmt;

use super::color::DiagColor;


pub enum DiagSever {
    Note,
    Warning,
    Error,
}

impl DiagSever {
    pub fn color(&self) -> &'static str {
        match self {
            DiagSever::Note => DiagColor::BLUE,
            DiagSever::Warning => DiagColor::ORANGE,
            DiagSever::Error => DiagColor::RED,
        }
    }
}

impl fmt::Display for DiagSever {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagSever::Note => write!(f, "note"),
            DiagSever::Warning => write!(f, "warning"),
            DiagSever::Error => write!(f, "error"),
        }
    }
}
