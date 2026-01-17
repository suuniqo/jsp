use std::{cell::RefCell, collections::HashSet, iter, rc::Rc};

use itertools::{Itertools, MultiPeek};

use crate::{
    diag::{DiagRef, DiagHelp, Diag, DiagKind},
    metasym::MetaSym,
    pool::PoolLookup,
    span::Span,
    symtable::SymTable,
    token::{Token, TokenKind},
    lexer::Lexer,
    reporter::Reporter
};

use super::{
    gram::{Gram, Action, Term},
    sem::SemAnalyzer,
    heur::{Fix, FixAction, Heur},
    Parser,
};


type LexerChained<'l> = iter::Chain<&'l mut (dyn Lexer + 'l), iter::Once<Token>>;

pub struct ParserCore<'t, 'l, 's, Pool: PoolLookup> {
    reporter: Rc<RefCell<Reporter<'t, Pool>>>,
    lexer: MultiPeek<LexerChained<'l>>,

    panic: bool,
    prev: Option<Token>,
    delims: Vec<(Term, Option<Span>)>,

    stack: Vec<usize>,
    semanter: SemAnalyzer<'s>,
}

impl<'t, 'l, 's, Pool: PoolLookup> ParserCore<'t, 'l, 's, Pool> {
    const MAX_RECOVERY_COST: usize = 512;

    pub fn new(
        reporter: Rc<RefCell<Reporter<'t, Pool>>>,
        lexer: &'l mut dyn Lexer,
        symtable: &'s mut dyn SymTable,
    ) -> Self {
        Self {
            reporter,
            lexer: lexer.chain(iter::once(Token::eof())).multipeek(),

            panic: false,
            prev: None,
            delims: Vec::new(),

            stack: Vec::new(),
            semanter: SemAnalyzer::new(symtable),
        }
    }

    fn next(&mut self) {
        self.prev = self.lexer.next();
    }
    
    fn stack_last(&self) -> usize {
        *self.stack.last().expect("unexpected empty parser stack: bad table")
    }

    fn prev(&self) -> Option<&Token> {
        self.prev.as_ref()
    }

    fn report(&mut self, diag: DiagRef) {
        if !self.panic {
            self.reporter.borrow_mut().emit(diag);
            self.panic = true;
        }
    }

    fn on_shift(&mut self, kind: TokenKind, span: Option<Span>) {
        self.semanter.on_shift(kind, span);
    }

    fn on_reduce(&mut self, rule_idx: usize) {
        if let Err(diags) = self.semanter.on_reduce(rule_idx) {
            for diag in diags {
                if !self.panic {
                    self.reporter.borrow_mut().emit(diag);
                }
            }
        }
    }

    fn positional_info(
        insertion: &[MetaSym],
        err_curr: &Token,
        err_prev: Option<&Token>,
    ) -> Option<(Token, bool)> {
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
            return None;
        };

        Some((token, before))
    }

    fn diag_fallback(&self, expected: HashSet<MetaSym>, curr: &Token, help: Option<(DiagHelp, usize)>) -> DiagRef {
        let mut diag =  Diag::make(
            DiagKind::UnexpectedTok(curr.kind.clone(), expected),
            curr.span.clone(),
            false,
        );

        if let Some(prev) = self.prev() {
            diag.add_note(prev.span.clone(), "after this");
        }

        if let Some((help, cost)) = help && cost < Self::MAX_RECOVERY_COST {
            diag.with_help(help)
        } else {
            diag
        }
    }

    fn diag_deletion(&self, expected: HashSet<MetaSym>, curr: Token, cost: usize) -> DiagRef {
        let help = (DiagHelp::DelToken(curr.clone()), cost);

        Self::diag_fallback(self, expected, &curr, Some(help))
    }

    fn diag_replacement(&self, expected: HashSet<MetaSym>, curr: Token, rep: MetaSym, cost: usize) -> DiagRef {
        if let Some(curr_term) = Term::from_token_kind(&curr.kind) && curr_term.is_keyword() && rep == MetaSym::Id {
            let diag = Diag::make(DiagKind::KeywordAsId(curr.kind.clone()), curr.span.clone(), true);

            return diag.with_help(DiagHelp::RepKw(curr));
        }

        let help = (DiagHelp::RepToken(curr.clone(), rep), cost);

        Self::diag_fallback(self, expected, &curr, Some(help))
    }

    fn diag_insertion_inner(&self, expected: HashSet<MetaSym>, curr: Token, ins: &[MetaSym]) -> DiagRef {
        let candidate = ins.first()
            .expect("unexpected empty insertion: incorrect eval_insertion method");

        if candidate.as_term().is_some_and(|term| term.is_right_delim()) {
            if let Some(curr_term) = Term::from_token_kind(&curr.kind) && curr_term.is_right_delim() {
                let mut diag = Diag::make(DiagKind::MismatchedDelim(curr.kind.clone()), curr.span, false);

                if let Some(last) = self.delims.last() && let Some(span) = &last.1 {
                    diag.add_error(span.clone(), "unclosed delimiter");
                }

                for (term, span) in self.delims.iter().rev() {
                    if term.delim_match(&curr_term) {
                        if let Some(span) = span {
                            diag.add_note(span.clone(), "closing delimiter possibly meant for this");
                        }

                        break;
                    }
                }

                return diag;
                
            } else if expected.len() == 1 {
                let mut diag = Diag::make(DiagKind::UnclosedDelim, curr.span, false);

                if let Some(last) = self.delims.last() && let Some(span) = &last.1 {
                    diag.add_note(span.clone(), "unclosed delimiter");
                }

                return diag;
            }
        }

        if *candidate == MetaSym::Semi {
            let mut diag = Diag::make(DiagKind::MissingSemi, curr.span, false);

            if let Some(prev) = self.prev() {
                diag.add_note(prev.span.clone(), "after this");
            }

            return diag;
        }

        if let Some(prev) = self.prev()
            && prev.kind == TokenKind::Comma
            && curr.kind == TokenKind::RParen {

            if *candidate == MetaSym::Type {
                let diag = Diag::make(DiagKind::TrailingCommaParam, prev.span.clone(), true);

                return diag.with_help(DiagHelp::DelTrailingComma(prev.span.clone()));
            }

            if *candidate == MetaSym::Expr {
                let diag = Diag::make(DiagKind::TrailingCommaArg, prev.span.clone(), true);

                return diag.with_help(DiagHelp::DelTrailingComma(prev.span.clone()));
            }
        }

        if matches!(curr.kind, TokenKind::Id(..)) {
            if expected.contains(&MetaSym::Type) {
                let diag = Diag::make(DiagKind::MissingVarType, curr.span.clone(), false);

                return diag.with_help(DiagHelp::InsVarType(curr.span));
            }

            if expected.contains(&MetaSym::FuncType) {
                let diag = Diag::make(DiagKind::MissingRetType, curr.span.clone(), false);

                return diag.with_help(DiagHelp::InsRetType(curr.span));
            }
        }

        if let Some(prev) = self.prev()
            && matches!(prev.kind, TokenKind::Id(..))
            && curr.kind == TokenKind::LBrack
            && ins.contains(&MetaSym::FuncParam)
            && expected.contains(&MetaSym::LParen) {
            
            let diag = Diag::make(DiagKind::MissingParamList, curr.span, false);

            return diag.with_help(DiagHelp::InsParamList(prev.span.clone()));
        }

        if let Some(prev) = self.prev()
            && prev.kind == TokenKind::LParen
            && curr.kind == TokenKind::RParen
            && expected.contains(&MetaSym::FuncParams) {

            let diag = Diag::make(
                DiagKind::EmptyParamList,
                Span::new(prev.span.start, curr.span.end),
                false
            );

            return diag.with_help(DiagHelp::InsParam(curr.span));
        }

        Self::diag_fallback(self, expected, &curr, None)
    }

    fn diag_insertion(&self, expected: HashSet<MetaSym>, curr: Token, ins: Vec<MetaSym>, cost: usize) -> DiagRef {
        let diag = self.diag_insertion_inner(expected, curr.clone(), &ins);

        if diag.has_help() || cost > Self::MAX_RECOVERY_COST {
            return diag;
        }
        let Some((reference, before)) = Self::positional_info(&ins, &curr, self.prev()) else {
            return diag;
        };

        let reference_sym = MetaSym::from_term(
            &Term::from_token_kind(&reference.kind).expect("reference to EOF on insertion")
        );

        diag.with_help(DiagHelp::InsToken(reference, before, reference_sym, ins))
    }

    fn norm_token(prev: Option<&Token>, curr: &Token) -> Token {
        let span = if curr.kind == TokenKind::Eof && let Some(prev) = prev {
            Span::new(prev.span.end, prev.span.end)
        } else {
            curr.span.clone()
        };

        Token::new(curr.kind.clone(), span)
    }
    
    fn apply_fix(&mut self, fix: Fix) {
        for _ in 0..fix.skips {
            self.next();
        }

        for action in fix.actions {
            match action {
                FixAction::Shift(kind, state_idx) => {
                    let term = Term::from_token_kind(&kind)
                        .expect("illegal shift by eof");

                    self.stack.push(state_idx);

                    if term.is_left_delim() {
                        self.delims.push((term, None));
                    } else if term.is_right_delim() {
                        self.delims.pop();
                    }

                    self.on_shift(kind, None);

                    self.prev = None;
                }
                FixAction::Reduce(rule_idx) => {
                    let (lhs, rhs) = Gram::RULES[rule_idx];

                    self.stack.truncate(self.stack.len() - rhs.len());

                    let lhs_idx = lhs.idx();

                    let stack_idx = self.stack_last();

                    self.stack.push(
                        Action::GOTO_TABLE[stack_idx][lhs_idx].expect("unexpected invalid goto iterm: bad table"),
                    );

                    self.on_reduce(rule_idx);

                    self.lexer.reset_peek();
                }
            }
        }
    }

    fn recover_and_emit(&mut self, curr: Token) -> bool {
        let expected = Heur::expected(&self.stack);

        let deletion    = Heur::eval_deletion(   &mut self.lexer, &self.stack);
        let insertion   = Heur::eval_insertion(  &mut self.lexer, &self.stack);
        let replacement = Heur::eval_replacement(&mut self.lexer, &self.stack);

        let curr = Self::norm_token(self.prev(), &curr);

        // make diag
        let (diag, fix) = if let Some((rep_fix, sym)) = replacement
            && insertion.as_ref().is_none_or(|(ins_fix, _)| rep_fix.cost < ins_fix.cost)
            && deletion.as_ref().is_none_or(|del_fix| rep_fix.cost < del_fix.cost) {

            (self.diag_replacement(expected, curr, sym, 0), rep_fix)

        } else if let Some(del_fix) = deletion
            && insertion.as_ref().is_none_or(|(ins_fix, _)| del_fix.cost < ins_fix.cost) {

            (self.diag_deletion(expected, curr, 0), del_fix)

        } else if let Some((ins_fix, syms)) = insertion {

            (self.diag_insertion(expected, curr, syms, 0), ins_fix)

        } else {
            let diag = Self::diag_fallback(self, expected, &curr, None);
            
            self.report(diag);
            return false;
        };

        self.apply_fix(fix);
        self.report(diag);

        true
    }
}

impl<'t: 'l, 'l: 's, 's, Pool: PoolLookup> Parser for ParserCore<'t, 'l, 's, Pool> {
    fn parse(&mut self) -> Option<Vec<usize>> {
        let mut parse = vec![];

        self.stack.push(0);

        while let Some(curr) = self.lexer.peek().cloned() {
            let stack_idx = self.stack_last();
            let token_idx = curr.kind.idx();

            let Some(action) = Action::ACTION_TABLE[stack_idx][token_idx] else {
                if !self.recover_and_emit(curr) {
                    return None;
                }

                self.lexer.reset_peek();
                continue;
            };

            match action {
                Action::Shift(state_idx) => {

                    let term = Term::from_token_kind(&curr.kind)
                        .expect("illegal shift by eof");

                    self.stack.push(state_idx);

                    if self.panic {
                        self.panic = Heur::eval_panic(&curr.kind);
                    }

                    if term.is_left_delim() {
                        self.delims.push((term, Some(curr.span.clone())));
                    } else if term.is_right_delim() {
                        self.delims.pop();
                    }

                    self.on_shift(curr.kind, Some(curr.span));

                    self.next();
                }
                Action::Reduce(rule_idx) => {
                    parse.push(rule_idx + 1);

                    let (lhs, rhs) = Gram::RULES[rule_idx];

                    self.stack.truncate(self.stack.len() - rhs.len());

                    let lhs_idx = lhs.idx();

                    let stack_idx = self.stack_last();

                    self.stack.push(
                        Action::GOTO_TABLE[stack_idx][lhs_idx].expect("unexpected invalid goto iterm: bad table"),
                    );

                    self.on_reduce(rule_idx);

                    self.lexer.reset_peek();
                }
                Action::Accept => {
                    parse.push(1);

                    self.on_reduce(0);

                    return Some(parse);
                }
            }
        }

        None
    }
}
