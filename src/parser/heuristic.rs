use std::{collections::HashSet, iter};

use itertools::MultiPeek;

use crate::lexer::Lexer;
use crate::token::{Token, TokenKind};
use crate::grammar::{Grammar, MetaSym, NotTerm, Term};

use super::action::{GOTO_TABLE, ACTION_TABLE, Action};

#[derive(Debug)]
pub enum FixAction {
    Shift(TokenKind, usize),
    Reduce(usize),
}

#[derive(Debug)]
pub struct Fix {
    pub cost: usize,
    pub skips: usize,
    pub actions: Vec<FixAction>,
}

impl Fix {
    fn insert(cost: usize, actions: Vec<FixAction>) -> Self {
        Self { cost, skips: 0, actions }
    }

    fn delete(cost: usize, skips: usize) -> Self {
        Self { cost, skips, actions: Vec::new() }
    }

    fn replace(cost: usize, actions: Vec<FixAction>) -> Self {
        Self { cost, skips: 1, actions }
    }
}

type LexerChained<'l> = iter::Chain<&'l mut (dyn Lexer + 'l), iter::Once<Token>>;

pub struct Heuristic;

impl<'l> Heuristic {
    pub fn expected(stack: &[usize]) -> HashSet<MetaSym> {
        let paths: Vec<(usize, Vec<usize>)> = (0..TokenKind::COUNT)
            .filter_map(|i| {
                Self::token_shifts(i, stack).map(|(st, _)| (i, st))
            })
            .collect();

        let term: HashSet<Term> = paths
            .iter()
            .filter_map(|(idx, _)| Term::from_token_kind(&TokenKind::from_idx(*idx)))
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

    const MAX_RECOVERY_LEN: usize = 32;

    pub fn eval_insertion(
        lexer: &mut MultiPeek<LexerChained<'l>>, 
        stack: &[usize],
    ) -> Option<(Fix, Vec<MetaSym>)> {
        let mut curr_stack = stack.to_vec();

        let mut actions = Vec::new();
        let mut insertion = Vec::new();
        let mut cost = 0;

        while insertion.len() <= Self::MAX_RECOVERY_LEN {
            // compute possible paths
            let mut paths: Vec<(usize, Vec<usize>, Vec<FixAction>)> = (0..TokenKind::COUNT)
                .filter_map(|i| {
                    Self::token_shifts(i, &curr_stack).map(|(st, tr)| (i, st, tr))
                })
                .collect();

            let mut candidates: Vec<(usize, Vec<usize>)> = paths
                .iter()
                .map(|(i, st, _)| (*i, st.clone()))
                .collect();

            lexer.reset_peek();

            // retain best candidates, stopping while full or by encountering sync symbol
            while let Some(next) = lexer.peek() {
                let next_candidates: Vec<(usize, Vec<usize>)> = candidates
                    .iter()
                    .filter_map(|(_i, st)| {
                        Self::token_shifts(next.kind.idx(), st).map(|(nst, _)| (*_i, nst))
                    })
                    .collect();

                if next_candidates.is_empty()
                    || (candidates.iter().any(|(i, _)| TokenKind::from_idx(*i).is_sync())
                    && !next_candidates.iter().any(|(i, _)| TokenKind::from_idx(*i).is_sync()))
                {
                    break;
                } else {
                    candidates = next_candidates;
                }

                if candidates.len() == 1 || next.kind.is_sync() {
                    break;
                }
            }

            // sort and extract by sync score
            candidates.sort_by(|a, b| {
                let kind_a = TokenKind::from_idx(a.0);
                let kind_b = TokenKind::from_idx(b.0);

                let cmp = kind_a.sync_score().cmp(&kind_b.sync_score());

                if cmp.is_eq()
                    && let Some(term_a) = Term::from_token_kind(&kind_a)
                    && let Some(term_b) = Term::from_token_kind(&kind_b) {

                    term_b.insert_cost().cmp(&term_a.insert_cost())
                } else {
                    cmp
                }
            });

            // ensure insertion never is empty
            let Some((i, ..)) = candidates.pop() else {
                return None;
            };

            let pos = paths
                .iter()
                .position(|(j, ..)| *j == i)
                .expect("will always match");

            let (_, new_stack, next_trace) = paths.swap_remove(pos);

            let term = Term::from_idx(i);

            // update state
            curr_stack = new_stack;

            insertion.push(term);
            actions.extend(next_trace);

            cost += term.insert_cost();

            lexer.reset_peek();

            let next_idx = lexer
                .peek()
                .expect("cannot be empty, as it was reset and entered the loop before")
                .kind
                .idx();

            // stop at resync
            if Self::token_shifts(next_idx, &curr_stack).is_some() {
                return Some((Fix::insert(cost * insertion.len(), actions), MetaSym::build_insertion(insertion)))
            }
        }

        None
    }

    pub fn eval_deletion(lexer: &mut MultiPeek<LexerChained<'l>>, stack: &[usize]) -> Option<Fix> {
        let mut cost = 0;
        let mut skips = 0;

        lexer.reset_peek();

        while let Some(next) = lexer.peek() {
            if Self::token_shifts(next.kind.idx(), &stack).is_some() {
                return Some(Fix::delete(skips * cost, skips));
            }

            let Some(next_kind) = Term::from_token_kind(&next.kind) else {
                break;
            };

            cost += next_kind.delete_cost();
            skips += 1;
        }

        None
    }

    pub fn eval_replacement(
        lexer: &mut MultiPeek<LexerChained<'l>>,
        stack: &[usize],
    ) -> Option<(Fix, MetaSym)> {

        lexer.reset_peek();

        let Some(Some(next)) = lexer.peek().map(|tok| Term::from_token_kind(&tok.kind)) else {
            return None;
        };

        let delete_cost = next.delete_cost();

        let paths: Vec<(usize, Vec<usize>, Vec<FixAction>)> = (0..TokenKind::COUNT)
            .filter_map(|i| Self::token_shifts(i, &stack).map(|(st, tr)| (i, st, tr)))
            .collect();

        let Some(next) = lexer.peek() else {
            return None;
        };

        let mut candidates: Vec<(usize, Vec<FixAction>)> = paths
            .into_iter()
            .filter_map(|(_i, st, _tr)| Self::token_shifts(next.kind.idx(), &st).map(|(..)| (_i, _tr)))
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

        if let Some((i, trace)) = candidates.pop() {
            let term = Term::from_idx(i);

            return Some((Fix::replace(delete_cost + term.insert_cost(), trace), MetaSym::from_insert(&term)))
        }

        None
    }

    fn reduce(
        token_idx: usize,
        stack: &[usize],
    ) -> Option<(Vec<usize>, Vec<FixAction>)> {
        let stack_idx = *stack.last()
            .expect("unexpected empty parser stack");

        let Some(action) = ACTION_TABLE[stack_idx][token_idx] else {
            return None;
        };

        let mut stack_clone = stack.to_vec();
        let mut trace = Vec::new();

        let Action::Reduce(mut rule_idx) = action else {
            return Some((stack_clone, trace));
        };

        loop {
            let (lhs, rhs) = Grammar::RULES[rule_idx];

            stack_clone.truncate(stack_clone.len() - rhs.len());

            let lhs_idx = lhs.idx();
            let stack_idx = *stack_clone.last()
                .expect("unexpected empty parser stack");

            stack_clone.push(
                GOTO_TABLE[stack_idx][lhs_idx]
                    .expect("if this fails there is something wrong with the goto table"),
            );

            trace.push(FixAction::Reduce(rule_idx));

            let stack_idx = *stack_clone.last()
                .expect("unexpected empty parser stack");

            let Some(action) = ACTION_TABLE[stack_idx][token_idx] else {
                return None;
            };

            let Action::Reduce(new_rule_idx) = action else {
                return Some((stack_clone, trace));
            };

            rule_idx = new_rule_idx;
        }
    }

    fn token_shifts(
        token_idx: usize,
        stack: &[usize],
    ) -> Option<(Vec<usize>, Vec<FixAction>)> {
        let (mut stack_clone, mut trace) = Self::reduce(token_idx, stack)?;

        let stack_idx = *stack_clone.last()
            .expect("unexpected empty parser stack");

        let Some(action) = ACTION_TABLE[stack_idx][token_idx] else {
            return None;
        };

        let Action::Shift(state_idx) = action else {
            return Some((stack_clone, trace));
        };

        trace.push(FixAction::Shift(TokenKind::from_idx(token_idx), state_idx));

        stack_clone.push(state_idx);
        return Some((stack_clone, trace));
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
            TokenKind::Id(..) => 1,
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
            Term::LParen | Term::RParen | Term::LBrack | Term::RBrack => 80,

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
            | Term::Bool => 80,

            Term::Void => 50,

            // identifiers and literals
            Term::Id => 210,

            Term::IntLit
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
            | Term::And
            | Term::Not
            | Term::Lt
            | Term::Eq => 20,

            Term::Mul => 19,

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

