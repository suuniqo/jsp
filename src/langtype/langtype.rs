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
            Type::Int => 2,
            Type::Float => 4,
            Type::Str => 128,
            Type::Bool => 2,
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
pub struct TypeFunc {
    pub ret_type: TypeVar,
    pub param_type: Rc<[TypeVar]>,
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

impl TypeFunc {
    pub fn new(ret_type: TypeVar, arg_type: &[TypeVar]) -> Self {
        Self {
            ret_type,
            param_type: arg_type.into(),
        }
    }

    pub fn add_ret(&mut self, ret_type: TypeVar) {
        self.ret_type = ret_type;
    }

    pub fn void_func(reason: Option<Span>) -> Self {
        TypeFunc::new(TypeVar::new(Type::Void, None), [TypeVar::new(Type::Void, reason)].as_slice().into())
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LangType {
    Var(TypeVar),
    Func(TypeFunc),
}

impl LangType {
    pub fn new_func(ret_type: TypeVar, arg_type: &[TypeVar]) -> Self {
        LangType::Func(TypeFunc::new(ret_type, arg_type))
    }

    pub fn void_func(reason: Option<Span>) -> Self {
        Self::Func(TypeFunc::new(TypeVar::new(Type::Void, None), [TypeVar::new(Type::Void, reason)].as_slice()))
    }

    pub fn new_var(langtype: Type, reason: Option<Span>) -> Self {
        Self::Var(TypeVar::new(langtype, reason))
    }

    pub fn size(&self) -> usize {
        match self {
            LangType::Var(var) => var.var_type.size(),
            LangType::Func(_) => 0,
        }
    }

    pub fn main_type(&self) -> Type {
        match self {
            LangType::Var(type_var) => type_var.var_type,
            LangType::Func(_) => Type::Func,
        }
    }
}
