use std::fmt;

use super::DiagKind;


#[repr(u8)]
#[derive(PartialEq, Eq)]
pub enum DiagSever {
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
            DiagKind::InvFmtFloat(_) => DiagSever::Error,
            DiagKind::MalformedStr(_) => DiagSever::Error,
            DiagKind::UnexpectedTok(_) => DiagSever::Error,
        }
    }
}

impl fmt::Display for DiagSever {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagSever::Warning => write!(f, "warning"),
            DiagSever::Error => write!(f, "error"),
        }
    }
}
