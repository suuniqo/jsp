use std::fmt;

use crate::{types::{TypeId, Type, TypeVar}, span::Span, pool::PoolLookup};


#[derive(Debug, Clone)]
pub struct Sym {
    pub lang_type: TypeId,
    pub span: Option<Span>,
    pub implicit: bool,

    pub(in super::super) pool_id: usize,
    pub(in super::super) offset: usize,
}

impl Sym {
    pub fn new(lang_type: TypeId, offset: usize, pool_id: usize, span: Option<Span>, implicit: bool) -> Self {
        Self {
            pool_id,
            offset,
            lang_type,
            implicit,
            span,
        }
    }

    pub fn explicit(lang_type: TypeId, offset: usize, pool_id: usize, span: Option<Span>) -> Self {
        Self {
            pool_id,
            offset,
            lang_type,
            implicit: false,
            span,
        }
    }

    pub fn fmt(&self, f: &mut fmt::Formatter<'_>, pool: &impl PoolLookup) -> fmt::Result {
        match &self.lang_type {
            TypeId::Var(var) => writeln!(
                f,
                "\
* lexema: '{}'
  + tipo: '{}'
  + despl: {}",
                pool.lookup(self.pool_id).expect("couldn't find symbol pool id"),
                var.var_type,
                self.offset
            ),
            TypeId::Func(func) => {
                let lexeme = pool.lookup(self.pool_id)
                    .expect("couldn't find symbol pool id");

                let len = if matches!(func.param_type.first(), Some(TypeVar { var_type: Type::Void, .. } )) {
                    0
                } else {
                    func.param_type.len()
                };

                writeln!(
                    f,
                    "\
* lexema: '{}'
  + etiqFuncion: '__tag_{}__'
  + tipo: '{}'
    + tipoRetorno: '{}'
    + numParam: {}",
                    lexeme,
                    lexeme,
                    Type::Func,
                    func.ret_type.var_type,
                    len,
                )?;

                if len > 0 {
                    for (i, arg_type) in func.param_type.iter().enumerate() {
                        writeln!(
                            f,
                            "      + tipoParam{}{}: '{}'",
                            if i + 1 < 10 { "0" } else { "" },
                            i + 1,
                            arg_type.var_type
                        )?;
                    }
                }

                Ok(())
            }
        }
    }
}
