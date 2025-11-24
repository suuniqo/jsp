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
}

impl<'t, 'l, L: Lexer> Parser for ParserCore<'t, 'l, L> {
    fn parse(&mut self) -> Option<Vec<usize>> {
        let mut stack = vec![0usize];
        let mut parse = vec![];

        let eof = Token::new(TokenKind::Eof, Span::default());

        let mut lexer = self.lexer.chain(iter::once(eof)).peekable();

        while let Some(token) = lexer.peek() {
            let stack_idx = *stack.last().unwrap();
            let token_idx = token.kind.idx();

            let Some(action) = ACTION_TABLE[stack_idx][token_idx] else {
                let expected = ACTION_TABLE[stack_idx]
                    .iter()
                    .enumerate()
                    .filter_map(|(idx, action)| {
                        let Some(action) = action else {
                            return None;
                        };

                        match *action {
                            Action::Reduce(_) => None,
                            Action::Shift(_) => Some(TokenKind::from_idx(idx)),
                            Action::Accept => None,
                        }
                    }).collect();

                let token = lexer.next().unwrap();

                lexer.for_each(|_| {});

                self.reporter.borrow_mut().push(Diag::new(
                    DiagKind::UnexpectedTok((token.kind, expected)),
                    token.span,
                ));


                return None;
            };

            match action {
                Action::Shift(state_idx) => {
                    stack.push(state_idx);
                    lexer.next();
                },
                Action::Reduce(rule_idx) => {
                    parse.push(rule_idx);

                    let (lhs, rhs) = GRAMMAR[rule_idx];

                    stack.truncate(stack.len() - rhs.len());

                    let lhs_idx = lhs.idx();
                    let stack_idx = *stack.last().unwrap();

                    stack.push(GOTO_TABLE[stack_idx][lhs_idx]
                        .expect("if this fails there is something wrong with the goto table"));
                },
                Action::Accept => return Some(parse),
            }
        }

        None
    }
}
