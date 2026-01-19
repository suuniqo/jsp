use std::process::{ExitCode, Termination};

#[repr(u8)]
#[derive(PartialEq, Eq)]
pub enum AnalysisResult {
    Success,
    CodeError,
    IOError,
}

impl AnalysisResult {
    pub fn is_success(&self) -> bool {
        *self == AnalysisResult::Success
    }
}

impl Termination for AnalysisResult {
    fn report(self) -> ExitCode {
        ExitCode::from(match self {
            Self::Success   => 0,
            Self::CodeError => 1,
            Self::IOError   => 2,
        })
    }
}
