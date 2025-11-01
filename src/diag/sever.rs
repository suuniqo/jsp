use std::fmt;

use super::DiagKind;


pub enum DiagSever {
    Note,
    Warning,
    Error,
}

impl DiagKind {
    pub fn sever(&self) -> DiagSever {
        match self {
            DiagKind::InvEscSeq(_) => DiagSever::Warning,
            DiagKind::StrayChar(_) => DiagSever::Error,
            DiagKind::UntermComm => DiagSever::Error,
            DiagKind::UntermStr => DiagSever::Error,
            DiagKind::OverflowStr(_) => DiagSever::Error,
            DiagKind::OverflowInt => DiagSever::Error,
            DiagKind::OverflowFloat => DiagSever::Error,
            DiagKind::InvFmtFloat => DiagSever::Error,
            DiagKind::MalformedStr(_) => DiagSever::Error,
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
