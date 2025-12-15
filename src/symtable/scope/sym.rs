use std::fmt;

use crate::{langtype::{LangType, Type}, symtable::pool::StrPool};


#[derive(Clone)]
pub struct Sym {
    pub pool_id: usize,
    pub offset: usize,
    pub ltype: LangType,
}

impl Sym {
    pub fn new(ltype: LangType, offset: usize, pool_id: usize) -> Self {
        Self {
            pool_id,
            offset,
            ltype,
        }
    }

    pub fn fmt(&self, f: &mut fmt::Formatter<'_>, pool: &StrPool) -> fmt::Result {
        match &self.ltype {
            LangType::Var(var) => writeln!(
                f,
                "\
* lexema: '{}'
  + tipo: '{}'
  + despl: {}",
                pool.get(self.pool_id).expect("couldn't find symbol pool id"),
                var.var_type,
                self.offset
            ),
            LangType::Func(func) => {
                writeln!(
                    f,
                    "\
* lexema: '{}'
  + tipo: '{}'
    + tipoRetorno: '{}'
    + numParam: {}",
                    pool.get(self.pool_id).expect("couldn't find symbol pool id"),
                    Type::Func,
                    func.ret_type,
                    func.arg_type.len()
                )?;

                for (i, arg_type) in func.arg_type.iter().enumerate() {
                    writeln!(
                        f,
                        "    + tipoParam{}{}: '{}'",
                        if i < 10 { "0" } else { "" },
                        i,
                        arg_type
                    )?;
                }

                Ok(())
            }
        }
    }
}
