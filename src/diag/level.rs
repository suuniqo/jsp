use super::DiagKind;


#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DiagLevel {
    Lexical,
    Syntactic,
    Semantic,
    None,
}

impl DiagKind {
    pub fn level(&self) -> DiagLevel {
        match self {
            DiagKind::InvEscSeq(_)          => DiagLevel::None,

            DiagKind::StrayChar(_)          => DiagLevel::Lexical,
            DiagKind::UntermComm            => DiagLevel::Lexical,
            DiagKind::UntermStr(_)          => DiagLevel::Lexical,
            DiagKind::MalformedStr(..)      => DiagLevel::Lexical,
            DiagKind::OverflowStr(_)        => DiagLevel::Lexical,
            DiagKind::OverflowInt           => DiagLevel::Lexical,
            DiagKind::OverflowFloat         => DiagLevel::Lexical,
            DiagKind::InvFmtFloat(_)        => DiagLevel::Lexical,

            DiagKind::UnexpectedTok(..)     => DiagLevel::Syntactic,
            DiagKind::MismatchedDelim(..)   => DiagLevel::Syntactic,
            DiagKind::UnclosedDelim         => DiagLevel::Syntactic,
            DiagKind::KeywordAsId(..)       => DiagLevel::Syntactic,
            DiagKind::MissingSemi           => DiagLevel::Syntactic,
            DiagKind::TrailingCommaParam    => DiagLevel::Syntactic,
            DiagKind::TrailingCommaArg      => DiagLevel::Syntactic,
            DiagKind::MissingVarType        => DiagLevel::Syntactic,
            DiagKind::MissingRetType        => DiagLevel::Syntactic,
            DiagKind::MissingParamList      => DiagLevel::Syntactic,
            DiagKind::EmptyParamList        => DiagLevel::Syntactic,

            DiagKind::MismatchedRetType(..) => DiagLevel::Semantic,
            DiagKind::UnexpectedRetType(_)  => DiagLevel::Semantic,
            DiagKind::ExpectedRetType       => DiagLevel::Semantic,
            DiagKind::Redefinition          => DiagLevel::Semantic,
            DiagKind::MismatchedTypes(..)   => DiagLevel::Semantic,
            DiagKind::UndefinedFunc(_)      => DiagLevel::Semantic,
            DiagKind::StrayRet              => DiagLevel::Semantic,
            DiagKind::InvalidCall(..)       => DiagLevel::Semantic,
        }
    }
}
