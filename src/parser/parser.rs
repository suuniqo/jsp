use std::iter;

use crate::{context::{Context, symtable::SymTable}, lexer::Lexer, span::Span};
use crate::{token::{Token, TokenKind}, diag::{Diag, DiagKind}};

use super::grammar::GRAMMAR;
use super::tables::{ACTION_TABLE, GOTO_TABLE, Action};

pub struct Parser<'t, 'c, 'l, T: SymTable, L: Lexer> {
    ctx: &'c mut Context<'t, T>,
    lex: &'l mut L,
}

impl<'t, 'c, 'l, T: SymTable, L: Lexer> Parser<'t, 'c, 'l, T, L> {
    pub fn new(ctx: &'c mut Context<'t, T>, lex: &'l mut L) -> Self {
        Self {
            ctx,
            lex,
        }
    }

    pub fn parse(self) -> Option<Vec<usize>> {
        let mut stack = vec![0usize];
        let mut parse = vec![];

        let eof = Token::new(TokenKind::Eof, Span::default());

        for token in self.lex.chain(iter::once(eof)) {
            let stack_idx = *stack.last().unwrap();
            let token_idx = token.kind.idx();

            let Some(action) = ACTION_TABLE[stack_idx][token_idx] else {
                let expected = ACTION_TABLE[stack_idx]
                    .iter()
                    .filter_map(|action| {
                        let Some(action) = action else {
                            return None;
                        };

                        match *action {
                            Action::Reduce(idx) => Some(TokenKind::from_idx(idx)),
                            Action::Shift(idx) => Some(TokenKind::from_idx(idx)),
                            Action::Accept => None,
                        }
                    }).collect();
                
                self.ctx.reporter.push(Diag::new(
                    DiagKind::UnexpectedTok((token.kind, expected)),
                    token.span,
                ));

                return None;
            };

            match action {
                Action::Shift(state_idx) => stack.push(state_idx),
                Action::Reduce(rule_idx) => {
                    parse.push(rule_idx);

                    let rule = GRAMMAR[rule_idx];

                    stack.truncate(stack.len() - rule.1.len());

                    let lhs_idx = rule.0.idx();
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
