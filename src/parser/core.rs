use std::cell::RefCell;
use std::iter::{self, Peekable};
use std::rc::Rc;

use crate::symtable::SymTable;
use crate::{lexer::Lexer, reporter::Reporter};
use crate::{token::{Token, TokenKind}, diag::{Diag, DiagKind}};

use super::{Parser, grammar::GRAMMAR, action::{Action, ACTION_TABLE, GOTO_TABLE}};

type LexerChained<'l> = iter::Chain<&'l mut (dyn Lexer + 'l), iter::Once<Token>>;

pub struct ParserCore<'t, 'l, 's> {
    reporter: Rc<RefCell<Reporter<'t>>>,
    lexer: Peekable<LexerChained<'l>>,
    symtable: &'s mut dyn SymTable,
    prev: Option<Token>,
}

impl<'t, 'l, 's> ParserCore<'t, 'l, 's> {
    pub fn new(reporter: Rc<RefCell<Reporter<'t>>>, lexer: &'l mut dyn Lexer, symtable: &'s mut dyn SymTable) -> Self {
        Self {
            reporter,
            lexer: lexer.chain(iter::once(Token::eof())).peekable(),
            symtable,
            prev: None,
        }
    }

    fn next(&mut self) {
        self.prev = self.lexer.next();
    }

    fn expected_tokens(&mut self, stack: &Vec<usize>) -> Vec<TokenKind> {
        let mut candidates: Vec<(usize, Vec<usize>)> = (0..TokenKind::COUNT)
            .filter_map(|i| Self::token_shifts(i, stack).map(|s| (i, s)))
            .collect();
        
        while let Some(next) = self.lexer.peek() {
            candidates = candidates
                .into_iter()
                .filter_map(|(i, s)| Self::token_shifts(next.kind.idx(), &s).map(|s| (i, s)))
                .collect();

            if next.kind.is_sync() {
                self.next();
                break;
            }
            if candidates.iter().any(|&(i, _)| TokenKind::from_idx(i).is_sync()) {
                break;
            }

            self.next();
        }

        let expected = candidates
            .into_iter()
            .map(|(i, _)| TokenKind::from_idx(i))
            .collect();

        expected
    }

    fn token_shifts(token_idx: usize, stack: &Vec<usize>) -> Option<Vec<usize>> {
        if TokenKind::Eof.idx() == token_idx {
            return None;
        }

        let stack_idx = *stack.last().unwrap();

        let Some(action) = ACTION_TABLE[stack_idx][token_idx] else {
            return None;
        };

        let mut stack_clone = stack.clone();

        let Action::Reduce(mut rule_idx) = action else {
            let Action::Shift(state_idx) = action else {
                unreachable!("action can't be Accept, as Eof is discarded early");
            };

            stack_clone.push(state_idx);
            return Some(stack_clone);
        };

        loop {
            let (lhs, rhs) = GRAMMAR[rule_idx];

            stack_clone.truncate(stack_clone.len() - rhs.len());

            let lhs_idx = lhs.idx();
            let stack_idx = *stack_clone.last().unwrap();

            stack_clone.push(GOTO_TABLE[stack_idx][lhs_idx]
                .expect("if this fails there is something wrong with the goto table"));

            let stack_idx = *stack_clone.last().unwrap();

            let Some(action) = ACTION_TABLE[stack_idx][token_idx] else {
                return None;
            };

            let Action::Reduce(new_rule_idx) = action else {
                let Action::Shift(state_idx) = action else {
                    unreachable!("action can't be Accept, as Eof is discarded early");
                };

                stack_clone.push(state_idx);
                return Some(stack_clone);
            };

            rule_idx = new_rule_idx;
        }
    }

    fn generate_error(expected: Vec<TokenKind>, err_curr: Token, err_prev: Option<Token>) -> Diag {
        let before = if err_curr.kind == TokenKind::Eof {
            false
        } else if err_prev.is_none() {
            true
        } else {
            expected[0].show_before()
        };

        let (span, kind) = if before {
            (err_curr.span, err_curr.kind)
        } else if let Some(err_prev) = err_prev {
            (err_prev.span, err_prev.kind)
        } else {
            unreachable!("lexer must never be empty");
        };

        Diag::new(DiagKind::UnexpectedTok((kind, before, expected)), span)
    }
}

impl TokenKind {
    fn is_sync(&self) -> bool {
        match self {
            TokenKind::Semi | TokenKind::RBrack | TokenKind::Eof => true,
            _ => false,
        }
    }
}

impl<'t, 'l, 's> Parser for ParserCore<'t, 'l, 's> {
    fn parse(&mut self) -> Option<Vec<usize>> {
        let mut stack = vec![0];
        let mut parse = vec![];

        while let Some(curr) = self.lexer.peek() {

            let stack_idx = *stack.last().unwrap();
            let token_idx = curr.kind.idx();

            let Some(action) = ACTION_TABLE[stack_idx][token_idx] else {
                let err_curr = curr.clone();
                let err_prev = self.prev.clone();

                let expected = self.expected_tokens(&stack);

                self.reporter
                    .borrow_mut()
                    .push(Self::generate_error(expected, err_curr, err_prev));

                let curr_idx = self.lexer
                    .peek()
                    .expect("should be at least EOF, as its catalogued as sync")
                    .kind
                    .idx();

                while let Some(last) = stack.last().copied() {
                    if ACTION_TABLE[last][curr_idx].is_some() {
                        break;
                    }

                    stack.pop();
                }

                if stack.is_empty() {
                    stack.push(0);
                }

                continue;
            };

            match action {
                Action::Shift(state_idx) => {
                    stack.push(state_idx);
                    self.next();
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
