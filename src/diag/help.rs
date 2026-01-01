use crate::{grammar::{Insert, Insertion, MetaSym, Term}, span::Span, token::Token};


#[derive(Clone)]
pub enum HelpAction {
    Insert(Span, bool, String),
    Delete(Span),
    Replace(Span, String),
}

impl HelpAction {
    pub fn span(&self) -> Span {
        match self {
            HelpAction::Insert(span, ..)  => span,
            HelpAction::Delete(span)      => span,
            HelpAction::Replace(span, ..) => span,
        }.clone()
    }
}

#[derive(Clone)]
pub enum DiagHelp {
    // lexer
    InsDecimal(Span),

    // parser
    InsToken(Token, bool, Vec<MetaSym>),
    DelToken(Token),
    RepToken(Token, MetaSym),
    RepKw(Token),
    DelTrailingComma(Span),
    InsVarType(Span),
    InsRetType(Span),
    InsParamList(Span),
    InsParam(Span),
}

impl DiagHelp {
    pub fn action(&self) -> HelpAction {
        match self {
            DiagHelp::InsDecimal(span) => HelpAction::Insert(
                span.clone(),
                false,
                "0".to_string()
            ),
            DiagHelp::InsToken(token, before, syms) => {
                let term = Term::from_token_kind(&token.kind)
                    .expect("reference to eof on insertion");

                let meta = MetaSym::from_term(&term);

                let insertion = if *before {
                    Insertion(None, syms, Some(meta))
                } else {
                    Insertion(Some(meta), syms, None)
                }.to_string();

                HelpAction::Insert(token.span.clone(), *before, insertion)
            },
            DiagHelp::DelToken(token) => HelpAction::Delete(
                token.span.clone()
            ),
            DiagHelp::RepToken(token, sym) => HelpAction::Replace(
                token.span.clone(),
                Insert(sym).to_string(),
            ),
            DiagHelp::RepKw(token) => HelpAction::Replace(
                token.span.clone(),
                format!("my_{}", token.kind.lexeme()),
            ),
            DiagHelp::DelTrailingComma(span) => HelpAction::Delete(span.clone()),
            DiagHelp::InsVarType(span) => HelpAction::Insert(
                span.clone(),
                true,
                "type ".to_string()
            ),
            DiagHelp::InsRetType(span) => HelpAction::Insert(
                span.clone(),
                true,
                "type ".to_string()
            ),
            DiagHelp::InsParamList(span) => HelpAction::Insert(
                span.clone(),
                false,
                "(void)".to_string(),
            ),
            DiagHelp::InsParam(span) => HelpAction::Insert(
                span.clone(),
                true,
                "void".to_string()
            ),
        }
    }
}
