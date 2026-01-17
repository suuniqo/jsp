use super::{
    action::SemAction,
    attr::Attr,
    super::gram::Gram,
};

use crate::{
    diag::DiagRef,
    types::{TypeId, Type},
    span::Span,
    symtable::SymTable,
    token::TokenKind,
};


pub struct SemAnalyzer<'s> {
    symtable: &'s mut dyn SymTable,
    stack: Vec<Option<Attr>>,
}

impl<'s> SemAnalyzer<'s> {
    pub fn new(symtable: &'s mut dyn SymTable) -> Self {
        Self {
            symtable,
            stack: Vec::new(),
        }
    }

    pub fn on_shift(&mut self, kind: TokenKind, span: Option<Span>) {
        let attr = match kind {
            TokenKind::Void => Attr::Type(
                TypeId::new_var(Type::Void, span),
            ),
            TokenKind::Bool => Attr::Type(
                TypeId::new_var(Type::Bool, span),
            ),
            TokenKind::Float => Attr::Type(
                TypeId::new_var(Type::Float, span),
            ),
            TokenKind::Int => Attr::Type(
                TypeId::new_var(Type::Int, span),
            ),
            TokenKind::Str => Attr::Type(
                TypeId::new_var(Type::Str, span),
            ),

            TokenKind::True | TokenKind::False => Attr::Expr(
                TypeId::new_var(Type::Bool, span), None
            ),
            TokenKind::FloatLit(_) => Attr::Expr(
                TypeId::new_var(Type::Float, span), None
            ),
            TokenKind::IntLit(_) => Attr::Expr(
                TypeId::new_var(Type::Int, span), None
            ),
            TokenKind::StrLit(_) => Attr::Expr(
                TypeId::new_var(Type::Str, span), None
            ),

            TokenKind::Id(pool_id) => Attr::Id(
                pool_id, span,
            ),

            _ => Attr::Unit(None, span),
        };

        self.stack.push(Some(attr));
    }
    
    pub fn on_reduce(&mut self, rule: usize) -> Result<(), Vec<DiagRef>> {
        let (_, rhs) = Gram::RULES[rule];

        let args = self.stack.split_off(self.stack.len() - rhs.len());

        match self.handle_rule(rule, args) {
            Ok(attr) => {
                self.stack.push(attr);
                Ok(())
            },
            Err(diag) => {
                self.stack.push(None);
                Err(diag)
            },
        }
    }

    fn handle_rule(&mut self, rule_idx: usize, args: Vec<Option<Attr>>) -> Result<Option<Attr>, Vec<DiagRef>> {
        let mut action = SemAction::new(self, rule_idx);

        action.run(args)
    }

    pub(super) fn symtable(&self) -> &dyn SymTable {
        self.symtable
    }

    pub(super) fn symtable_mut(&mut self) -> &mut dyn SymTable {
        self.symtable
    }
}
