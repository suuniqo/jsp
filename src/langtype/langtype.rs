use std::fmt;


#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
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
            Type::Bool => write!(f, "bool"),
            Type::Func => write!(f, "function"),
            Type::Void => write!(f, "void"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TypeFunc {
    pub ret_type: Type,
    pub arg_type: Vec<Type>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TypeVar {
    pub var_type: Type,
}

impl TypeVar {
    pub fn new(langtype: Type) -> Option<Self> {
        if langtype == Type::Func || langtype == Type::Void {
            None
        } else {
            Some(Self{ var_type: langtype })
        }
    }
}

impl TypeFunc {
    pub fn new(ret_type: Type, arg_type: Vec<Type>) -> Self {
        Self {
            ret_type,
            arg_type,
        }
    }
}


#[derive(Clone, PartialEq, Eq, Hash)]
pub enum LangType {
    Var(TypeVar),
    Func(TypeFunc),
}

impl LangType {
    pub fn new_func(ret_type: Type, arg_type: Vec<Type>) -> Self {
        Self::Func(TypeFunc::new(ret_type, arg_type))
    }

    pub fn new_var(langtype: Type) -> Self {
        Self::Var(TypeVar::new(langtype).expect(&format!("incorrect var type, found {}", langtype)))
    }

    pub fn size(&self) -> usize {
        match self {
            LangType::Var(var) => var.var_type.size(),
            LangType::Func(_) => 0,
        }
    }
}
