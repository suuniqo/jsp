use std::cell::RefCell;
use std::iter;
use std::rc::Rc;

use crate::{lexer::Lexer, reporter::Reporter, span::Span};
use crate::{token::{Token, TokenKind}, diag::{Diag, DiagKind}};

use super::{Parser, grammar::GRAMMAR, action::{Action, ACTION_TABLE, GOTO_TABLE}};

pub struct ParserCore<'t, 'l, L: Lexer> {
    reporter: Rc<RefCell<Reporter<'t>>>,
    lexer: &'l mut L,
}

impl<'t, 'l, L: Lexer> ParserCore<'t, 'l, L> {
    pub fn new(reporter: Rc<RefCell<Reporter<'t>>>, lexer: &'l mut L) -> Self {
        Self {
            reporter,
            lexer,
        }
    }

    pub fn expected_tokens(stack: Vec<usize>) -> Vec<TokenKind> {
        let stack_idx = *stack.last().unwrap();

        let mut expected_idx = Vec::new();
        let mut possible_idx = Vec::new();

        for (idx, action) in ACTION_TABLE[stack_idx].iter().enumerate() {
            let Some(action) = action else {
                continue;
            };

            match action {
                Action::Reduce(rule_idx) => possible_idx.push((idx, rule_idx)),
                Action::Shift(_) => expected_idx.push(idx),
                Action::Accept => (),
            }
        }

        for (token_idx, rule_idx) in possible_idx {
            let mut stack_clone = stack.clone();
            let mut rule_idx = *rule_idx;

            loop {
                let (lhs, rhs) = GRAMMAR[rule_idx];

                stack_clone.truncate(stack_clone.len() - rhs.len());

                let lhs_idx = lhs.idx();
                let stack_idx = *stack_clone.last().unwrap();

                stack_clone.push(GOTO_TABLE[stack_idx][lhs_idx]
                    .expect("if this fails there is something wrong with the goto table"));

                let stack_idx = *stack_clone.last().unwrap();

                let Some(action) = ACTION_TABLE[stack_idx][token_idx] else {
                    break;
                };

                match action {
                    Action::Reduce(new_rule_idx) => rule_idx = new_rule_idx,
                    Action::Shift(_) => {
                        expected_idx.push(token_idx);
                        break;
                    },
                    Action::Accept => break,
                }
            }
        }

        expected_idx
            .iter()
            .map(|idx| TokenKind::from_idx(*idx))
            .collect()
    }
}

impl<'t, 'l, L: Lexer> Parser for ParserCore<'t, 'l, L> {
    fn parse(&mut self) -> Option<Vec<usize>> {
        let mut stack = vec![0usize];
        let mut parse = vec![];

        let eof = Token::new(TokenKind::Eof, Span::default());

        let mut prev_token = None;
        let mut lexer = self.lexer.chain(iter::once(eof)).peekable();

        while let Some(token) = lexer.peek() {

            let stack_idx = *stack.last().unwrap();
            let token_idx = token.kind.idx();

            let Some(action) = ACTION_TABLE[stack_idx][token_idx] else {
                let expected_tok = Self::expected_tokens(stack.clone());

                let token = lexer.next().unwrap();

                lexer.for_each(|_| {});

                let prev_token = prev_token.unwrap_or(token.clone());

                let (span, kind, before) = if token.kind == TokenKind::Eof {
                    (prev_token.span, prev_token.kind, false)
                } else {
                    (token.span, token.kind, true)
                };

                self.reporter.borrow_mut().push(Diag::new(
                    DiagKind::UnexpectedTok((kind, before, expected_tok)),
                    span,
                ));

                return None;
            };

            match action {
                Action::Shift(state_idx) => {
                    prev_token = Some(token.clone());

                    stack.push(state_idx);
                    lexer.next();
                },
                Action::Reduce(rule_idx) => {
                    parse.push(rule_idx + 1);

                    let (lhs, rhs) = GRAMMAR[rule_idx];

                    stack.truncate(stack.len() - rhs.len());

                    let lhs_idx = lhs.idx();
                    let stack_idx = *stack.last().unwrap();

                    stack.push(GOTO_TABLE[stack_idx][lhs_idx]
                        .expect("if this fails there is something wrong with the goto table"));
                },
                Action::Accept => {
                    parse.push(0 + 1);
                    return Some(parse)
                },
            }
        }

        None
    }
}
