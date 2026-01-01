use std::{collections::HashSet, iter};

use itertools::{Itertools, MultiPeek};

use crate::lexer::Lexer;
use crate::span::Span;
use crate::token::{Token, TokenKind};
use crate::grammar::{GRAMMAR, NotTerm, Term, GramSym, MetaSym};

use super::action::{GOTO_TABLE, ACTION_TABLE, Action};

pub enum Fix {
    Skip,
    Action(Action)
}

type LexerChained<'l> = iter::Chain<&'l mut (dyn Lexer + 'l), iter::Once<Token>>;

pub struct Heuristic;

impl<'l> Heuristic {
    const MAX_RECOVERY_LEN: usize = 64;
    const MAX_INSERTION_LEN: usize = 4;

    pub const MAX_INSERTION_COST: usize = 512;

    pub fn expected(stack: &Vec<usize>, syms: &Vec<GramSym>) -> HashSet<MetaSym> {
        let delims = Vec::new();

        let paths: Vec<(usize, Vec<usize>, Vec<GramSym>)> = (0..TokenKind::COUNT)
            .filter_map(|i| {
                Self::token_shifts(i, &delims, stack, syms).map(|(_, st, sy)| (i, st, sy))
            })
            .collect();

        let term: HashSet<Term> = paths
            .iter()
            .dedup_by(|(_, st1, _), (_, st2, _)| st1 == st2)
            .filter_map(|(idx, _, _)| Term::from_token_kind(&TokenKind::from_idx(*idx)))
            .collect();

        let stack_idx = *stack.last()
            .expect("unexpected empty parser stack");

        let not_term: HashSet<NotTerm> = GOTO_TABLE[stack_idx]
            .iter()
            .enumerate()
            .filter_map(|(idx, v)| if v.is_some() { Some(NotTerm::from_idx(idx)) } else { None })
            .collect();

        MetaSym::build_expected(term, not_term)
    }

    pub fn eval_insertion(
        lexer: &mut MultiPeek<LexerChained<'l>>, 
        delims: &Vec<(TokenKind, Option<Span>)>,
        stack: &Vec<usize>,
        syms: &Vec<GramSym>,
    ) -> (Option<usize>, Vec<MetaSym>, Option<(Vec<(TokenKind, Option<Span>)>, Vec<usize>, Vec<GramSym>)>) {
        let mut curr_delims = delims.clone();
        let mut curr_stack = stack.clone();
        let mut curr_syms = syms.clone();

        let mut insertion = Vec::new();
        let mut cost = 0;

        while insertion.len() <= Self::MAX_RECOVERY_LEN {
            let mut paths: Vec<(usize, Vec<(TokenKind, Option<Span>)>, Vec<usize>, Vec<GramSym>)> = (0..TokenKind::COUNT - 1)
                .filter_map(|i| {
                    Self::token_shifts(i, &curr_delims, &curr_stack, &curr_syms).map(|(d, st, sy)| (i, d, st, sy))
                })
                .collect();

            let mut candidates = paths.clone();

            lexer.reset_peek();

            while let Some(next) = lexer.peek() {
                let next_candidates: Vec<(usize, Vec<(TokenKind, Option<Span>)>, Vec<usize>, Vec<GramSym>)> = candidates
                    .iter()
                    .filter_map(|(_i, d, st, sy)| {
                        Self::token_shifts(next.kind.idx(), d, st, sy).map(|(nd, nst, nsy)| (*_i, nd, nst, nsy))
                    })
                    .dedup_by(|(_, _, st1, _), (_, _, st2, _)| st1 == st2)
                    .collect();

                if next_candidates.is_empty() {
                    break;
                } else {
                    candidates = next_candidates;
                }

                if candidates.len() == 1 {
                    break;
                }
                if next.kind.is_sync() {
                    break;
                }
            }

            candidates.sort_by(|a, b| {
                let kind_a = TokenKind::from_idx(a.0);
                let kind_b = TokenKind::from_idx(b.0);

                kind_a.sync_score().cmp(&kind_b.sync_score())
            });

            let (i, ..) = candidates.pop()
                .expect("candidates should never be empty as the loop guarantees it and a row shouldn't be empty");

            let pos = paths
                .iter()
                .position(|(j, ..)| *j == i)
                .expect("will always match");

            let (_, new_delims, new_stack, new_syms) = paths.swap_remove(pos);

            let term = Term::from_idx(i);
            cost += term.insert_cost();
            insertion.push(term);

            curr_stack = new_stack;
            curr_syms = new_syms;
            curr_delims = new_delims;

            lexer.reset_peek();

            let next_idx = lexer
                .peek()
                .expect("cannot be empty, as it was reset and entered the loop before")
                .kind
                .idx();

            if Self::token_shifts(next_idx, &curr_delims, &curr_stack, &curr_syms).is_some() {
                break;
            }
        }

        let insert_cost = if insertion.len() > Self::MAX_INSERTION_LEN {
            None
        } else {
            Some(cost * insertion.len())
        };

        let next_state = if insertion.len() > Self::MAX_RECOVERY_LEN {
            None
        } else {
            Some((curr_delims, curr_stack, curr_syms))
        };

        (insert_cost, MetaSym::build_insertion(insertion), next_state)
    }

    pub fn eval_deletion(lexer: &mut MultiPeek<LexerChained<'l>>, stack: &Vec<usize>, syms: &Vec<GramSym>) -> Option<usize> {
        let mut cost = 0;
        let mut deletions = 0;

        lexer.reset_peek();

        while let Some(next) = lexer.peek() {
            let delims = Vec::new();

            if Self::token_shifts(next.kind.idx(), &delims, &stack, &syms).is_some() {
                return Some(deletions * cost);
            }

            let Some(next_kind) = Term::from_token_kind(&next.kind) else {
                return Some(usize::MAX);
            };

            cost += next_kind.delete_cost();
            deletions += 1;
        }

        None
    }

    pub fn eval_replacement(
        lexer: &mut MultiPeek<LexerChained<'l>>,
        delims: &Vec<(TokenKind, Option<Span>)>,
        stack: &Vec<usize>,
        syms: &Vec<GramSym>
    ) -> Option<(usize, MetaSym, Vec<(TokenKind, Option<Span>)>, Vec<usize>, Vec<GramSym>)> {

        let mut cost = 0;

        lexer.reset_peek();

        let Some(Some(next)) = lexer.peek().map(|tok| Term::from_token_kind(&tok.kind)) else {
            return None;
        };

        cost += next.delete_cost();

        let paths: Vec<(usize, Vec<(TokenKind, Option<Span>)>, Vec<usize>, Vec<GramSym>)> = (0..TokenKind::COUNT)
            .filter_map(|i| {
                Self::token_shifts(i, &delims, &stack, &syms).map(|(d, st, sy)| (i, d, st, sy))
            })
            .collect();

        let Some(next) = lexer.peek() else {
            return None;
        };

        let mut candidates: Vec<(usize, Vec<(TokenKind, Option<Span>)>, Vec<usize>, Vec<GramSym>)> = paths
            .into_iter()
            .filter(|(_i, d, st, sy)| Self::token_shifts(next.kind.idx(), d, st, sy).is_some())
            .dedup_by(|(.., st1, _), (.., st2, _)| st1 == st2)
            .collect();

        candidates.sort_by(|(i, ..), (j, ..)| {
            let kind_a = TokenKind::from_idx(*i);
            let kind_b = TokenKind::from_idx(*j);

            let cmp = kind_a.sync_score().cmp(&kind_b.sync_score());

            if cmp.is_eq()
                && let Some(term_a) = Term::from_token_kind(&kind_a)
                && let Some(term_b) = Term::from_token_kind(&kind_b) {

                term_b.insert_cost().cmp(&term_a.insert_cost())
            } else {
                cmp
            }
        });

        if let Some((i, delims, stack, syms)) = candidates.pop() {
            let term = Term::from_idx(i);

            return Some((cost + term.insert_cost(), MetaSym::from_insert(&term), delims, stack, syms))
        }

        None
    }

    fn reduce(
        token_idx: usize,
        stack: &Vec<usize>,
        syms: &Vec<GramSym>,
    ) -> Option<(Vec<usize>, Vec<GramSym>)> {
        let stack_idx = *stack.last()
            .expect("unexpected empty parser stack");

        let Some(action) = ACTION_TABLE[stack_idx][token_idx] else {
            return None;
        };

        let mut stack_clone = stack.clone();
        let mut syms_clone = syms.clone();

        let Action::Reduce(mut rule_idx) = action else {
            return Some((stack_clone, syms_clone));
        };

        loop {
            let (lhs, rhs) = GRAMMAR[rule_idx];

            stack_clone.truncate(stack_clone.len() - rhs.len());

            let lhs_idx = lhs.idx();
            let stack_idx = *stack_clone.last()
                .expect("unexpected empty parser stack");

            stack_clone.push(
                GOTO_TABLE[stack_idx][lhs_idx]
                    .expect("if this fails there is something wrong with the goto table"),
            );

            syms_clone.truncate(syms_clone.len() - rhs.len());
            syms_clone.push(GramSym::N(lhs));

            let stack_idx = *stack_clone.last()
                .expect("unexpected empty parser stack");

            let Some(action) = ACTION_TABLE[stack_idx][token_idx] else {
                return None;
            };

            let Action::Reduce(new_rule_idx) = action else {
                return Some((stack_clone, syms_clone));
            };

            rule_idx = new_rule_idx;
        }
    }

    fn token_shifts(
        token_idx: usize,
        delims: &Vec<(TokenKind, Option<Span>)>,
        stack: &Vec<usize>,
        syms: &Vec<GramSym>,
    ) -> Option<(Vec<(TokenKind, Option<Span>)>, Vec<usize>, Vec<GramSym>)> {
        let (mut stack_clone, mut syms_clone) = Self::reduce(token_idx, stack, syms)?;

        let mut delims_clone = delims.clone();

        let stack_idx = *stack_clone.last()
            .expect("unexpected empty parser stack");

        let Some(action) = ACTION_TABLE[stack_idx][token_idx] else {
            return None;
        };

        let Action::Shift(state_idx) = action else {
            return Some((delims_clone, stack_clone, syms_clone));
        };

        let kind = TokenKind::from_idx(token_idx);

        let term = Term::from_token_kind(&kind)
            .expect("illegal shift by eof");

        if term.is_left_delim() {
            delims_clone.push((kind.clone(), None));
        } else if term.is_right_delim() {
            delims_clone.pop();
        }

        syms_clone.push(GramSym::T(term));

        stack_clone.push(state_idx);
        return Some((delims_clone, stack_clone, syms_clone));
    }

    pub fn eval_panic(reduced: &TokenKind) -> bool {
        !reduced.is_sync()
    }
}

impl TokenKind {
    fn is_sync(&self) -> bool {
        match self {
            TokenKind::Semi
            | TokenKind::RBrack
            | TokenKind::RParen
            | TokenKind::Comma
            | TokenKind::Eof => true,
            _ => false,
        }
    }

    fn sync_score(&self) -> usize {
        match self {
            TokenKind::RParen => 5,
            TokenKind::RBrack => 5,
            TokenKind::Semi => 5,
            TokenKind::Do => 4,
            TokenKind::Assign => 4,
            TokenKind::While => 4,
            TokenKind::Int => 4,
            TokenKind::Void => 4,
            TokenKind::Float => 4,
            TokenKind::Str => 4,
            TokenKind::Bool => 4,
            TokenKind::LBrack => 3,
            TokenKind::Mul => 2,
            TokenKind::And => 2,
            TokenKind::Lt => 2,
            TokenKind::Eq => 2,
            TokenKind::Let => 2,
            TokenKind::Func => 2,
            TokenKind::True => 2,
            TokenKind::False => 2,
            TokenKind::FloatLit(_) => 2,
            TokenKind::IntLit(_) => 2,
            TokenKind::StrLit(_) => 2,
            TokenKind::Id(_) => 1,
            TokenKind::LParen => 1,
            _ => 0,
        }
    }
}

impl Term {
    fn delete_cost(&self) -> usize {
        match self {
            // never delete structural starters
            Term::If
            | Term::Do
            | Term::While
            | Term::Let
            | Term::Func
            | Term::Ret
            | Term::Read
            | Term::Write => 800,

            // important but less than statements
            Term::Int
            | Term::Float
            | Term::Str
            | Term::Bool
            | Term::Void => 500,

            // identifiers and literals
            Term::Id
            | Term::IntLit
            | Term::FloatLit
            | Term::StrLit
            | Term::True
            | Term::False => 50,

            // assignment 
            Term::Assign | Term::AndAssign => 30,

            // operators 
            Term::Sum
            | Term::Sub
            | Term::Mul
            | Term::And
            | Term::Not
            | Term::Lt
            | Term::Eq => 20,

            // delims 
            Term::LParen | Term::RParen | Term::LBrack | Term::RBrack => 40,

            // cheap junk 
            Term::Comma => 5,
            Term::Semi => 1,
        }
    }

    fn insert_cost(&self) -> usize {
        match self {
            // never auto-insert whole statements
            Term::If
            | Term::Do
            | Term::Func
            | Term::Ret
            | Term::Read
            | Term::Write => 400,

            Term::Let => 300,

            // types
            Term::Int
            | Term::Float
            | Term::Str
            | Term::Bool
            | Term::Void => 80,

            // identifiers and literals
            Term::Id
            | Term::IntLit
            | Term::FloatLit
            | Term::StrLit
            | Term::True
            | Term::False => 200,

            // assignment
            Term::Assign
            | Term::AndAssign => 50,

            // operators 
            Term::Sum
            | Term::Sub
            | Term::Mul
            | Term::And
            | Term::Not
            | Term::Lt
            | Term::Eq => 20,

            // delims (common fix)
            Term::LParen
            | Term::While
            | Term::RParen
            | Term::LBrack
            | Term::RBrack => 10,

            // cheapest fixes
            Term::Comma => 3,
            Term::Semi => 1,
        }
    }
}

