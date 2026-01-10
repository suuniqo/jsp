use std::str;

use crate::span::Span;


#[derive(Clone)]
pub struct Window<'t> {
    chars: str::Chars<'t>,
    span: Span,
}

impl<'t> Window<'t> {
    pub fn new(slice: &'t str) -> Self {
        Self {
            chars: slice.chars(),
            span: Span::default(),
        }
    }

    pub fn span(&self) -> Span {
        self.span.clone()
    }

    pub fn peek_one(&self) -> char {
        self.chars.clone().next().unwrap_or('\0')
    }

    pub fn peek_two(&self) -> (char, char) {
        let mut iter = self.chars.clone();

        (iter.next().unwrap_or('\0'), iter.next().unwrap_or('\0'))
    }

    pub fn finished(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    pub fn consume(&mut self) -> Option<char> {
        let next = self.chars.next()?;
        
        self.span.end += next.len_utf8();

        Some(next)
    }

    pub fn collapse(&mut self) {
        self.span.start = self.span.end
    }

    pub fn consume_while<C: Fn(char) -> bool>(&mut self, predicate: C) {
        while predicate(self.peek_one()) && self.consume().is_some() { }
    }
}
