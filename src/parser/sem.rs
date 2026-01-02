use crate::{
    diag::Diag, grammar::Grammar, langtype::{LangType, Type}, span::Span, symtable::SymTable, token::TokenKind
};

pub enum Attr {
    /// Stores return type
    Unit(Option<Type>),
    /// Stores pool id
    Id(usize),
    /// Stores type
    Type(LangType, Option<Span>),
}

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
                LangType::new_var(Type::Void),
                span,
            ),
            TokenKind::Bool | TokenKind::True | TokenKind::False => Attr::Type(
                LangType::new_var(Type::Bool),
                span,
            ),
            TokenKind::Float | TokenKind::FloatLit(_) => Attr::Type(
                LangType::new_var(Type::Float),
                span,
            ),
            TokenKind::Int | TokenKind::IntLit(_) => Attr::Type(
                LangType::new_var(Type::Int),
                span,
            ),
            TokenKind::Str | TokenKind::StrLit(_) => Attr::Type(
                LangType::new_var(Type::Str),
                span,
            ),
            TokenKind::Id(pool_id, _) => Attr::Id(pool_id),
            _ => Attr::Unit(None),
        };

        self.stack.push(Some(attr));
    }
    
    pub fn on_reduce(&mut self, rule: usize) -> Result<(), Diag> {
        let (_, rhs) = Grammar::RULES[rule];

        let args: Option<Vec<Attr>> = self.stack
            .drain(self.stack.len() - rhs.len()..)
            .collect();

        let attr = self.handle_rule(rule, args.as_deref())?;

        self.stack.push(attr);

        Ok(())
    }

    fn handle_rule(&mut self, rule_idx: usize, args: Option<&[Attr]>) -> Result<Option<Attr>, Diag> {
        let Some(args) = args else {
            return Ok(None);
        };

        let action = SemAction::new(self, rule_idx, args);

        action.run().map(|attr| Some(attr))
    }
}

struct SemAction<'a, 'b, 's> {
    analyzer: &'b mut SemAnalyzer<'s>,
    args: &'a [Attr],
    rule: SemRule,
}

impl<'a, 'b, 's> SemAction<'a, 'b, 's> {
    fn new(analyzer: &'b mut SemAnalyzer<'s>, rule_idx: usize, args: &'a [Attr]) -> Self {
        Self {
            analyzer,
            args,
            rule: SemRule::from_idx(rule_idx),
        }
    }

    fn run(&self) -> Result<Attr, Diag> {
        match self.rule {
            SemRule::Axiom                => self.axiom(),
            SemRule::TopBlockStop         => self.top_block_stop(),
            SemRule::TopBlockFunc         => self.top_block_func(),
            SemRule::TopBlockStmnt        => self.top_block_stmnt(),
            SemRule::FuncBlockStop        => self.func_block_stop(),
            SemRule::FuncBlockStmnt       => self.func_block_stmnt(),
            SemRule::FuncParamStop        => self.func_param_stop(),
            SemRule::FuncParamMore        => self.func_param_more(),
            SemRule::FuncParamNone        => self.func_param_none(),
            SemRule::FuncParamSome        => self.func_param_some(),
            SemRule::FuncRetNone          => self.func_ret_none(),
            SemRule::FuncRetSome          => self.func_ret_some(),
            SemRule::FuncParam            => self.func_param(),
            SemRule::FuncName             => self.func_name(),
            SemRule::FuncRettype          => self.func_rettype(),
            SemRule::Func                 => self.func(),
            SemRule::DeclZoneVar          => self.decl_zone_var(),
            SemRule::TypeMap              => self.type_map(),
            SemRule::StmntBlockDoWhile    => self.stmnt_block_do_while(),
            SemRule::StmntBlockDeclAssign => self.stmnt_block_decl_assign(),
            SemRule::StmntBlockDecl       => self.stmnt_block_decl(),
            SemRule::StmntBlockIf         => self.stmnt_block_if(),
            SemRule::Stmnt                => self.stmnt(),
            SemRule::FuncArgStop          => self.func_arg_stop(),
            SemRule::FuncArgMore          => self.func_arg_more(),
            SemRule::FuncArgNone          => self.func_arg_none(),
            SemRule::FuncArgSome          => self.func_arg_some(),
            SemRule::StmntRet             => self.stmnt_ret(),
            SemRule::StmntRead            => self.stmnt_read(),
            SemRule::StmntWrite           => self.stmnt_write(),
            SemRule::StmntCall            => self.stmnt_call(),
            SemRule::StmntAndassign       => self.stmnt_andassign(),
            SemRule::StmntAssign          => self.stmnt_assign(),
            SemRule::ExprId               => self.expr_id(),
            SemRule::ExprLit              => self.expr_lit(),
            SemRule::ExprWrap             => self.expr_wrap(),
            SemRule::ExprCall             => self.expr_call(),
            SemRule::ExprMap              => self.expr_map(),
            SemRule::ExprOperBinNum       => self.expr_oper_bin_num(),
            SemRule::ExprOperBinBool      => self.expr_oper_bin_bool(),
            SemRule::ExprOperUnrBool      => self.expr_oper_unr_bool(),
            SemRule::ExprOperUnrNum       => self.expr_oper_unr_num(),
        }
    }

    fn axiom(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn top_block_stop(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn top_block_func(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn top_block_stmnt(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn func_block_stop(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn func_block_stmnt(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn func_param_stop(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn func_param_more(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn func_param_none(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn func_param_some(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn func_ret_none(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn func_ret_some(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn func_param(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn func_name(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn func_rettype(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn func(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn decl_zone_var(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn type_map(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn stmnt_block_do_while(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn stmnt_block_decl_assign(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn stmnt_block_decl(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn stmnt_block_if(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn stmnt(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn func_arg_stop(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn func_arg_more(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn func_arg_none(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn func_arg_some(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn stmnt_ret(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn stmnt_read(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn stmnt_write(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn stmnt_call(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn stmnt_andassign(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn stmnt_assign(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn expr_id(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn expr_lit(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn expr_wrap(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn expr_call(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn expr_map(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn expr_oper_bin_num(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn expr_oper_bin_bool(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn expr_oper_unr_bool(&self) -> Result<Attr, Diag> {
        todo!()
    }

    fn expr_oper_unr_num(&self) -> Result<Attr, Diag> {
        todo!()
    }
}

#[derive(Debug)]
enum SemRule {
    Axiom,
    TopBlockStop,
    TopBlockFunc,
    TopBlockStmnt,
    FuncBlockStop,
    FuncBlockStmnt,
    FuncParamStop,
    FuncParamMore,
    FuncParamNone,
    FuncParamSome,
    FuncRetNone,
    FuncRetSome,
    FuncParam,
    FuncName,
    FuncRettype,
    Func,
    DeclZoneVar,
    TypeMap,
    StmntBlockDoWhile,
    StmntBlockDeclAssign,
    StmntBlockDecl,
    StmntBlockIf,
    Stmnt,
    FuncArgStop,
    FuncArgMore,
    FuncArgNone,
    FuncArgSome,
    StmntRet,
    StmntRead,
    StmntWrite,
    StmntCall,
    StmntAndassign,
    StmntAssign,
    ExprId,
    ExprLit,
    ExprWrap,
    ExprCall,
    ExprMap,
    ExprOperBinNum,
    ExprOperBinBool,
    ExprOperUnrBool,
    ExprOperUnrNum,
}

impl SemRule {
    pub fn from_idx(idx: usize) -> Self {
        match idx {
            0 => SemRule::Axiom,
            1 => SemRule::TopBlockStop,
            2 => SemRule::TopBlockFunc,
            3  => SemRule::TopBlockStmnt,
            4  => SemRule::FuncBlockStop,
            5  => SemRule::FuncBlockStmnt,
            6  => SemRule::FuncParamStop,
            7  => SemRule::FuncParamMore,
            8  => SemRule::FuncParamNone,
            9  => SemRule::FuncParamSome,
            10 => SemRule::FuncRetNone,
            11 => SemRule::FuncRetSome,
            12 => SemRule::FuncParam,
            13 => SemRule::FuncName,
            14 => SemRule::FuncRettype,
            15 => SemRule::Func,
            18 => SemRule::DeclZoneVar,
            19..=22 => SemRule::TypeMap,
            23 => SemRule::StmntBlockDoWhile,
            24 => SemRule::StmntBlockDeclAssign,
            25 => SemRule::StmntBlockDecl,
            26 => SemRule::StmntBlockIf,
            27 => SemRule::Stmnt,
            28 => SemRule::FuncArgStop,
            29 => SemRule::FuncArgMore,
            30 => SemRule::FuncArgNone,
            31 => SemRule::FuncArgSome,
            32 => SemRule::StmntRet,
            33 => SemRule::StmntRead,
            34 => SemRule::StmntWrite,
            35 => SemRule::StmntCall,
            36 => SemRule::StmntAndassign,
            37 => SemRule::StmntAssign,
            38 => SemRule::ExprId,
            39..=43 => SemRule::ExprLit,
            44 => SemRule::ExprWrap,
            45 => SemRule::ExprCall,
            46 | 48 | 50 | 52 | 54 | 56 => SemRule::ExprMap,
            49 | 51 => SemRule::ExprOperBinNum,
            53 | 55 | 57 => SemRule::ExprOperBinBool,
            47 => SemRule::ExprOperUnrBool,
            58 | 59 => SemRule::ExprOperUnrNum,
            _ => unreachable!(
                "unexpected invalid rule index: got {} but len is: {}",
                idx, Grammar::LEN,
            )
        }
    }
}

//     0  => self.axiom(args),
//
//     1  => self.top_block_stop(args),
//     2  => self.top_block_func(args),
//     3  => self.top_block_stmnt(args),
//
//     4  => self.func_block_stop(args),
//     5  => self.func_block_stmnt(args),
//
//     6  => self.func_param_stop(args),
//     7  => self.func_param_more(args),
//
//     8  => self.func_param_none(args),
//     9  => self.func_param_some(args),
//
//     10 => self.func_ret_none(args),
//     11 => self.func_ret_some(args),
//
//     12 => self.func_param(args),
//     13 => self.func_name(args),
//     14 => self.func_rettype(args),
//
//     15 => self.func(args),
//
//     16 => self.func_ret_none(args),
//     17 => self.func_ret_some(args),
//
//     18 => self.decl_zone_var(args),
//
//     19..=22 => self.type_map(args),
//
//     23 => self.stmnt_block_do_while(args),
//     24 => self.stmnt_block_decl_assign(args),
//     25 => self.stmnt_block_decl(args),
//     26 => self.stmnt_block_if(args),
//     27 => self.stmnt(args),
//
//     28 => self.func_arg_stop(args),
//     29 => self.func_arg_more(args),
//
//     30 => self.func_arg_none(args),
//     31 => self.func_arg_some(args),
//
//     32 => self.stmnt_ret(args),
//     33 => self.stmnt_read(args),
//     34 => self.stmnt_write(args),
//     35 => self.stmnt_call(args),
//     36 => self.stmnt_andassign(args),
//     37 => self.stmnt_assign(args),
//
//     38      => self.expr_id(args),
//     39..=43 => self.expr_lit(args),
//     44      => self.expr_wrap(args),
//     45      => self.expr_call(args),
//
//     46 | 48 | 50 | 52 | 54 | 56 => self.expr_map(args),
//
//     49 | 51      => self.expr_oper_bin_num(args),
//     53 | 55 | 57 => self.expr_oper_bin_bool(args),
//
//     47      => self.expr_oper_unr_bool(args),
//     58 | 59 => self.expr_oper_unr_num(args),
