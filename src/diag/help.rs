use std::fmt;

use crate::{color::Color, grammar::GramSym, span::Span, token::Token};


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
    InsToken(Token, bool, Vec<GramSym>),
    DelToken(Token),
    RepToken(Token, GramSym),
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
            DiagHelp::InsToken(token, before, gram_syms) => {
                let kind_sym = &GramSym::from_token_kind(&token.kind)
                    .expect("insertion references eof");

                let mut insertion = GramSym::space(gram_syms);

                if *before && GramSym::are_spaced(gram_syms.last().expect("insertion is empty"), kind_sym) {
                    insertion.push(' ');
                } else if !*before && GramSym::are_spaced(kind_sym, gram_syms.first().expect("insertion is empty")) {
                    insertion = format!(" {}", insertion);
                }

                HelpAction::Insert(token.span.clone(), *before, insertion)
            },
            DiagHelp::DelToken(token) => HelpAction::Delete(
                token.span.clone()
            ),
            DiagHelp::RepToken(token, gram_sym) => HelpAction::Replace(
                token.span.clone(),
                gram_sym.to_string()
            ),
            DiagHelp::RepKw(token) => HelpAction::Replace(
                token.span.clone(),
                format!("not_{}", token.kind.lexeme_concrete()),
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
                "void ".to_string()
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

impl fmt::Display for DiagHelp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagHelp::InsDecimal(..) => write!(f, "add a decimal part"),
            DiagHelp::InsToken(found, before, insertion) => {
                if insertion.is_empty() {
                    unreachable!("insertion can't be empty");
                } else {
                    let mut msg = String::new();

                    for (i, tok) in insertion.iter().enumerate() {
                        msg.push_str(&format!("`{}{}{}`", Color::HIGHLIGHT, tok, Color::RESET));

                        if i + 2 < insertion.len() {
                            msg.push_str(", ");
                        } else if i + 2 == insertion.len() {
                            msg.push_str(" and ");
                        }
                    }

                    write!(
                        f,
                        "insert {} {} `{}{}{}`",
                        msg,
                        if *before { "before" } else { "after" },
                        Color::HIGHLIGHT, found.kind.lexeme_concrete(), Color::RESET,
                    )
                }
            },
            DiagHelp::DelToken(found) => write!(
                f,
                "remove the unnecessary `{}{}{}`",
                Color::HIGHLIGHT, found.kind.lexeme_concrete(), Color::RESET
            ),
            DiagHelp::RepToken(found, rep) => write!(
                f,
                "replace `{}{}{}` by `{}{}{}`",
                Color::HIGHLIGHT, found.kind.lexeme_concrete(), Color::RESET, Color::HIGHLIGHT, rep, Color::RESET
            ),
            DiagHelp::RepKw(_) => write!(f, "change the name to use it as an identifier"),
            DiagHelp::DelTrailingComma(_) => write!(f, "remove the trailing comma"),
            DiagHelp::InsVarType(_) => write!(f, "add the missing type"),
            DiagHelp::InsRetType(_) => write!(f, "add a return type"),
            DiagHelp::InsParamList(..) => write!(f, "add a parameter list"),
            DiagHelp::InsParam(_) => write!(f, "perhaps you meant to have no parameters"),
        }
    }
}
