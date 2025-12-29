use std::fmt;

use super::DiagKind;


#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum DiagSever {
    Note,
    Warning,
    Error,
}

impl DiagKind {
    pub fn sever(&self) -> DiagSever {
        match self {
            DiagKind::InvEscSeq(_) => DiagSever::Warning,
            _ => DiagSever::Error,
        }
    }
}

impl fmt::Display for DiagSever {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagSever::Warning => write!(f, "warning"),
            DiagSever::Error => write!(f, "error"),
            DiagSever::Note => write!(f, "note"),
        }
    }
}
