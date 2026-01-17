use std::collections::VecDeque;

use crate::{types::{TypeId, TypeVar}, span::Span};


#[derive(Debug, Clone)]
pub(super) enum Attr {
    /// Stores found return types with reasons and span
    Unit(Option<(Vec<TypeVar>, Option<Span>)>, Option<Span>),
    /// Stores pool id
    Id(usize, Option<Span>),
    /// Stores type
    Type(TypeId),
    /// Stores type and optional pool id
    Expr(TypeId, Option<usize>),
    /// Stores found parameter types with pool ids and reasons
    FuncParams(VecDeque<(usize, Option<Span>, TypeVar)>),
    /// Stores found argument types
    FuncArgs(VecDeque<(TypeVar, Option<usize>)>),
}
