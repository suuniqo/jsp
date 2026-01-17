use crate::token::TokenKind;


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
    pub(super) fn insert(cost: usize, actions: Vec<FixAction>) -> Self {
        Self { cost, skips: 0, actions }
    }

    pub(super) fn delete(cost: usize, skips: usize) -> Self {
        Self { cost, skips, actions: Vec::new() }
    }

    pub(super) fn replace(cost: usize, actions: Vec<FixAction>) -> Self {
        Self { cost, skips: 1, actions }
    }
}
