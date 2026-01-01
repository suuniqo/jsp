use std::cell::RefCell;
use std::collections::HashSet;
use std::{iter, usize};
use std::rc::Rc;

use itertools::{Itertools, MultiPeek};

use crate::diag::{DiagHelp, DiagSever};
use crate::grammar::{GRAMMAR, GramSym, MetaSym, Term};
use crate::parser::heuristic::Heuristic;
use crate::span::Span;
use crate::symtable::SymTable;

use crate::{
    diag::{Diag, DiagKind},
    token::{Token, TokenKind},
};

use crate::{lexer::Lexer, reporter::Reporter};

use super::{
    action::{Action, ACTION_TABLE, GOTO_TABLE},
    semanter::Semanter,
    Parser,
};

type LexerChained<'l> = iter::Chain<&'l mut (dyn Lexer + 'l), iter::Once<Token>>;

pub struct ParserCore<'t, 'l, 's> {
    reporter: Rc<RefCell<Reporter<'t>>>,
    lexer: MultiPeek<LexerChained<'l>>,
    semanter: Semanter<'s>,
    prev: Option<Token>,
    panic: bool,
    delims: Vec<(TokenKind, Option<Span>)>,
}

impl<'t, 'l, 's> ParserCore<'t, 'l, 's> {
    pub fn new(
        reporter: Rc<RefCell<Reporter<'t>>>,
        lexer: &'l mut dyn Lexer,
        symtable: &'s mut dyn SymTable,
    ) -> Self {
        Self {
            reporter,
            lexer: lexer.chain(iter::once(Token::eof())).multipeek(),
            semanter: Semanter::new(symtable),
            prev: None,
            panic: false,
            delims: Vec::new(),
        }
    }

    fn next(&mut self) {
        self.prev = self.lexer.next();
    }

    fn report(&mut self, diag: Diag) {
        if !self.panic {
            self.reporter.borrow_mut().push(diag);
            self.panic = true;
        }
    }

    fn positional_info(
        insertion: &Vec<MetaSym>,
        err_curr: &Token,
        err_prev: &Option<Token>,
    ) -> (Token, bool) {
        let before = if err_curr.kind == TokenKind::Eof {
            false
        } else if err_prev.is_none() {
            true
        } else {
            insertion
                .first()
                .map(|sym| sym.show_before())
                .unwrap_or(true)
        };

        let token = if before {
            err_curr.clone()
        } else if let Some(err_prev) = err_prev {
            err_prev.clone()
        } else {
            unreachable!("lexer must never be empty");
        };

        (token, before)
    }

    fn generate_diag(
        &self,
        expected: HashSet<MetaSym>,
        insertion: &Vec<MetaSym>,
        curr: &Token,
        prev: &Option<Token>
    ) -> (Diag, Option<DiagHelp>) {
        let curr_span = if curr.kind == TokenKind::Eof && let Some(prev) = prev {
            Span::new(prev.span.end, prev.span.end)
        } else {
            curr.span.clone()
        };

        let make_fallback = |expected, curr: &Token, prev: &Option<Token>| {
            let mut diag =  Diag::make(
                DiagKind::UnexpectedTok(curr.kind.clone(), expected),
                curr_span.clone(),
                false,
            );

            if let Some(prev) = prev {
                diag.add_span(prev.span.clone(), DiagSever::Note, Some("after this".to_string()), false);
            }

            diag
        };

        let Some(candidate) = insertion.first() else {
            return (make_fallback(expected, curr, prev), None);
        };

        if candidate.as_term().is_some_and(|term| term.is_right_delim()) {
            if let Some(curr_term) = Term::from_token_kind(&curr.kind) && curr_term.is_right_delim() {
                let mut diag = Diag::make(DiagKind::MismatchedDelim(curr.kind.clone()), curr_span, false);

                if let Some(last) = self.delims.last() && let Some(span) = &last.1 {
                    diag.add_span(
                        span.clone(),
                        DiagSever::Error,
                        Some("unclosed delimiter".to_string()),
                        false
                    );
                }

                return (diag, None);
                
            } else if expected.len() == 1 {
                let mut diag = Diag::make(DiagKind::UnclosedDelim, curr_span, false);

                if let Some(last) = self.delims.last() && let Some(span) = &last.1 {
                    diag.add_span(
                        span.clone(),
                        DiagSever::Note,
                        Some("unclosed delimiter".to_string()),
                        false
                    );
                }

                return (diag, None);
            }
        }

        if let Some(curr_term) = Term::from_token_kind(&curr.kind) && curr_term.is_keyword() && *candidate == MetaSym::Id {
            let diag = Diag::make(DiagKind::KeywordAsId(curr.kind.clone()), curr_span, true);

            return (diag, Some(DiagHelp::RepKw(curr.clone())))
        }

        if *candidate == MetaSym::Semi {
            let mut diag = Diag::make(DiagKind::MissingSemi, curr_span, false);

            if let Some(prev) = prev {
                diag.add_span(prev.span.clone(), DiagSever::Note, Some("after this".to_string()), false);
            }

            return (diag, None)
        }

        if let Some(prev) = prev
            && prev.kind == TokenKind::Comma
            && curr.kind == TokenKind::RParen {

            if *candidate == MetaSym::Type {
                let diag = Diag::make(DiagKind::TrailingCommaParam, prev.span.clone(), true);

                return (diag, Some(DiagHelp::DelTrailingComma(prev.span.clone())))
            }

            if *candidate == MetaSym::Expr {
                let diag = Diag::make(DiagKind::TrailingCommaArg, prev.span.clone(), true);

                return (diag, Some(DiagHelp::DelTrailingComma(prev.span.clone())))
            }

        }

        if matches!(curr.kind, TokenKind::Id(_)) {
            if expected.contains(&MetaSym::Type) {
                let diag = Diag::make(DiagKind::MissingVarType, curr_span.clone(), false);

                return (diag, Some(DiagHelp::InsVarType(curr_span)))
            }

            if expected.contains(&MetaSym::FuncType) {
                let diag = Diag::make(DiagKind::MissingRetType, curr_span.clone(), false);

                return (diag, Some(DiagHelp::InsRetType(curr_span)))
            }
        }

        if let Some(prev) = prev
            && matches!(prev.kind, TokenKind::Id(_))
            && curr.kind == TokenKind::LBrack
            && insertion.contains(&MetaSym::FuncParam)
            && expected.contains(&MetaSym::LParen) {
            
            let diag = Diag::make(DiagKind::MissingParamList, curr_span, false);

            let help = Some(DiagHelp::InsParamList(prev.span.clone()));

            return (diag, help)
        }

        if let Some(prev) = prev
            && prev.kind == TokenKind::LParen
            && curr.kind == TokenKind::RParen
            && expected.contains(&MetaSym::FuncParams) {

            let diag = Diag::make(
                DiagKind::EmptyParamList,
                Span::new(prev.span.start, curr_span.end),
                false
            );

            return (diag, Some(DiagHelp::InsParam(curr_span)))
        }

        (make_fallback(expected, curr, prev), None)
    }

    fn recover_and_emit(
        &mut self,
        curr: Token,
        prev: Option<Token>,
        stack: &mut Vec<usize>,
        syms: &mut Vec<GramSym>,
    ) -> bool {
        let expected = Heuristic::expected(stack, syms);

        let deletion = Heuristic::eval_deletion(&mut self.lexer, stack, syms);
        let replacement = Heuristic::eval_replacement(&mut self.lexer, &self.delims, stack, syms);

        let (insert_cost, insertion, next_insert_state) = Heuristic::eval_insertion(
            &mut self.lexer,
            &self.delims,
            stack,
            syms
        );

        // make diag
        let (diag, mut help) = self.generate_diag(expected, &insertion, &curr, &prev);

        // add help
        if let Some((replace_cost, sym, rep_delims, rep_stack, rep_syms)) = replacement
            && insert_cost.is_none_or(|cost| replace_cost < cost) {

            self.next();

            *stack = rep_stack;
            *syms = rep_syms;
            self.delims = rep_delims;

            if help.is_none() {
                help = Some(DiagHelp::RepToken(curr, sym));
            }

        } else if let Some(delete_cost) = deletion
            && insert_cost.is_none_or(|cost| delete_cost < cost) {

            self.next();

            if help.is_none() {
                help = Some(DiagHelp::DelToken(curr));
            }

        } else if !insertion.is_empty() && let Some((new_delims, new_stack, new_syms)) = next_insert_state {
            let (token, before) = Self::positional_info(&insertion, &curr, &prev);

            *stack = new_stack;
            *syms = new_syms;

            self.delims = new_delims;

            if help.is_none() && let Some(cost) = insert_cost && cost < Heuristic::MAX_INSERTION_COST {
                help = Some(DiagHelp::InsToken(token, before, insertion));
            }
        }

        if let Some(help) = help {
            self.report(diag.with_help(help));
            return true;
        }

        self.report(diag);
        false
    }
}

impl<'t, 'l, 's> Parser for ParserCore<'t, 'l, 's> {
    fn parse(&mut self) -> Option<Vec<usize>> {
        let mut stack = vec![0];
        let mut syms = vec![];
        let mut parse = vec![];

        while let Some(curr) = self.lexer.peek() {
            let stack_idx = *stack.last()
                .expect("unexpected empty parser stack");

            let token_idx = curr.kind.idx();

            let Some(action) = ACTION_TABLE[stack_idx][token_idx] else {
                let err_curr = curr.clone();
                let err_prev = self.prev.clone();

                if !self.recover_and_emit(err_curr, err_prev, &mut stack, &mut syms) {
                    return None;
                }

                self.lexer.reset_peek();

                continue;
            };

            match action {
                Action::Shift(state_idx) => {

                    let term = Term::from_token_kind(&curr.kind)
                        .expect("illegal shift by eof");

                    stack.push(state_idx);
                    syms.push(GramSym::T(term));

                    if self.panic {
                        self.panic = Heuristic::eval_panic(&curr.kind);
                    }

                    if term.is_left_delim() {
                        self.delims.push((curr.kind.clone(), Some(curr.span.clone())));
                    } else if term.is_right_delim() {
                        self.delims.pop();
                    }

                    self.next();
                }
                Action::Reduce(rule_idx) => {
                    parse.push(rule_idx + 1);

                    let (lhs, rhs) = GRAMMAR[rule_idx];

                    stack.truncate(stack.len() - rhs.len());

                    let lhs_idx = lhs.idx();

                    let stack_idx = *stack.last()
                        .expect("unexpected empty parser stack");

                    stack.push(
                        GOTO_TABLE[stack_idx][lhs_idx]
                            .expect("if this fails there is something wrong with the goto table"),
                    );

                    syms.truncate(syms.len() - rhs.len());
                    syms.push(GramSym::N(lhs));

                    self.lexer.reset_peek();
                }
                Action::Accept => {
                    parse.push(0 + 1);
                    return Some(parse);
                }
            }
        }

        None
    }
}
