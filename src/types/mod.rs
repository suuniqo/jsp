use std::{fmt, rc::Rc};

use crate::span::Span;


#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Float,
    Str,
    Bool,
    Func,
    Void,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Int => 1,
            Type::Float => 2,
            Type::Str => 64,
            Type::Bool => 1,
            Type::Func => 0,
            Type::Void => 0,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Str => write!(f, "string"),
            Type::Bool => write!(f, "boolean"),
            Type::Func => write!(f, "function"),
            Type::Void => write!(f, "void"),
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub var_type: Type,
    pub reason: Option<Span>,
}

impl TypeVar {
    pub fn new(langtype: Type, reason: Option<Span>) -> Self {
        Self{ var_type: langtype, reason }
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeFunc {
    pub ret_type: TypeVar,
    pub param_type: Rc<[TypeVar]>,
}

impl TypeFunc {
    pub fn new(ret_type: TypeVar, arg_type: &[TypeVar]) -> Self {
        Self {
            ret_type,
            param_type: arg_type.into(),
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeId {
    Var(TypeVar),
    Func(TypeFunc),
}

impl TypeId {
    pub fn new_var(langtype: Type, reason: Option<Span>) -> Self {
        Self::Var(TypeVar::new(langtype, reason))
    }

    pub fn size(&self) -> usize {
        match self {
            TypeId::Var(var) => var.var_type.size(),
            TypeId::Func(_) => 0,
        }
    }

    pub fn main_type(&self) -> Type {
        match self {
            TypeId::Var(type_var) => type_var.var_type,
            TypeId::Func(_) => Type::Func,
        }
    }
}
