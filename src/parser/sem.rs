use std::collections::VecDeque;

use crate::{
    diag::{Diag, DiagHelp, DiagKind, DiagSever}, grammar::Grammar, langtype::{LangType, Type, TypeVar}, span::Span, symtable::SymTable, token::TokenKind
};

#[derive(Debug, Clone)]
pub enum Attr {
    /// Stores found return types with reasons and span
    Unit(Option<(Vec<TypeVar>, Option<Span>)>, Option<Span>),
    /// Stores pool id
    Id(usize, Option<Span>),
    /// Stores type and reason
    Type(LangType),
    /// Stores found parameter types with pool ids and reasons
    FuncParams(VecDeque<(usize, Option<Span>, TypeVar)>),
    /// Stores found argument types
    FuncArgs(VecDeque<TypeVar>),
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
                LangType::new_var(Type::Void, span),
            ),
            TokenKind::Bool | TokenKind::True | TokenKind::False => Attr::Type(
                LangType::new_var(Type::Bool, span),
            ),
            TokenKind::Float | TokenKind::FloatLit(_) => Attr::Type(
                LangType::new_var(Type::Float, span),
            ),
            TokenKind::Int | TokenKind::IntLit(_) => Attr::Type(
                LangType::new_var(Type::Int, span),
            ),
            TokenKind::Str | TokenKind::StrLit(_) => Attr::Type(
                LangType::new_var(Type::Str, span),
            ),
            TokenKind::Id(pool_id, _) => Attr::Id(
                pool_id, span,
            ),
            _ => Attr::Unit(None, span),
        };

        self.stack.push(Some(attr));
    }
    
    pub fn on_reduce(&mut self, rule: usize) -> Result<(), Vec<Diag>> {
        let (_, rhs) = Grammar::RULES[rule];

        let args = self.stack.split_off(self.stack.len() - rhs.len());

        match self.handle_rule(rule, args) {
            Ok(attr) => {
                self.stack.push(attr);
                return Ok(());
            },
            Err(diag) => {
                self.stack.push(None);
                return Err(diag);
            },
        }
    }

    fn handle_rule(&mut self, rule_idx: usize, args: Vec<Option<Attr>>) -> Result<Option<Attr>, Vec<Diag>> {
        let mut action = SemAction::new(self, rule_idx);

        action.run(args)
    }
}

struct SemAction<'a, 's> {
    analyzer: &'a mut SemAnalyzer<'s>,
    rule: SemRule,
}

impl<'a, 's> SemAction<'a, 's> {
    fn new(analyzer: &'a mut SemAnalyzer<'s>, rule_idx: usize) -> Self {
        Self {
            analyzer,
            rule: SemRule::from_idx(rule_idx),
        }
    }

    fn symtable(&self) -> &dyn SymTable {
        self.analyzer.symtable
    }

    fn symtable_mut(&mut self) -> &mut dyn SymTable {
        self.analyzer.symtable
    }

    fn func_recovery(&mut self, _args: Vec<Option<Attr>>) {
        if self.symtable().scopes() > 1 {
            self.symtable_mut().pop_scope();
        }
    }

    fn run(&mut self, args: Vec<Option<Attr>>) -> Result<Option<Attr>, Vec<Diag>> {
        let Some(args) = args.clone().into_iter().collect::<Option<Vec<Attr>>>() else {
            match self.rule {
                SemRule::Func => self.func_recovery(args),
                _ => (),
            }
            return Ok(None);
        };

        match self.rule {
            SemRule::Axiom                => self.axiom(args),
            SemRule::Lambda               => self.lambda(args),
            SemRule::BlockMore            => self.block_more(args),
            SemRule::FuncBlockStmnt       => self.func_block_stmnt(args),
            SemRule::FuncParamStop        => self.func_param_stop(args),
            SemRule::FuncParamMore        => self.func_param_more(args),
            SemRule::FuncParamNone        => self.func_param_none(args),
            SemRule::FuncParamSome        => return self.func_param_some(args).map(|attr| Some(attr)),
            SemRule::FuncRetNone          => self.func_ret_none(args),
            SemRule::FuncRetSome          => self.func_ret_some(args),
            SemRule::FuncParam            => self.func_param(args),
            SemRule::FuncName             => self.func_name(args),
            SemRule::FuncRettype          => self.func_rettype(args),
            SemRule::Func                 => self.func(args),
            SemRule::StmntRetNone         => self.stmnt_ret_none(args),
            SemRule::StmntRetSome         => self.stmnt_ret_some(args),
            SemRule::DeclZoneVar          => self.decl_zone_var(args),
            SemRule::TypeMap              => self.type_map(args),
            SemRule::StmntBlockDoWhile    => self.stmnt_block_do_while(args),
            SemRule::StmntBlockDeclAssign => self.stmnt_block_decl_assign(args),
            SemRule::StmntBlockDecl       => self.stmnt_block_decl(args),
            SemRule::StmntBlockIf         => self.stmnt_block_if(args),
            SemRule::Stmnt                => self.stmnt(args),
            SemRule::FuncArgStop          => self.func_arg_stop(args),
            SemRule::FuncArgMore          => self.func_arg_more(args),
            SemRule::FuncArgNone          => self.func_arg_none(args),
            SemRule::FuncArgSome          => self.func_arg_some(args),
            SemRule::StmntRet             => self.stmnt_ret(args),
            SemRule::StmntRead            => self.stmnt_read(args),
            SemRule::StmntWrite           => self.stmnt_write(args),
            SemRule::StmntCall            => return self.stmnt_call(args).map(|attr| Some(attr)),
            SemRule::StmntAndassign       => self.stmnt_andassign(args),
            SemRule::StmntAssign          => self.stmnt_assign(args),
            SemRule::ExprId               => self.expr_id(args),
            SemRule::ExprWrap             => self.expr_wrap(args),
            SemRule::ExprCall             => return self.expr_call(args).map(|attr| Some(attr)),
            SemRule::ExprOperBinNum       => self.expr_oper_bin_num(args),
            SemRule::ExprOperBinCmp       => self.expr_oper_bin_cmp(args),
            SemRule::ExprOperBinBool      => self.expr_oper_bin_bool(args),
            SemRule::ExprOperUnrBool      => self.expr_oper_unr_bool(args),
            SemRule::ExprOperUnrNum       => self.expr_oper_unr_num(args),
        }.map(|attr| Some(attr)).map_err(|diag| vec![diag])
    }

    fn axiom(&mut self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let [Attr::Unit(None, None)] = args[..] else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        self.symtable_mut().pop_scope();

        Ok(Attr::Unit(None, None))
    }

    fn lambda(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let [] = args[..] else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Unit(None, None))
    }

    fn block_more(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 2] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });
        let [Attr::Unit(retinf, _), Attr::Unit(None, None)] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        if let Some((_, Some(span))) = retinf {
            return Err(Diag::make(DiagKind::StrayRet, span, true));
        }

        Ok(Attr::Unit(None, None))
    }

    fn func_block_stmnt(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 2] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });
        let [Attr::Unit(retinf1, _), Attr::Unit(retinf2, _)] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let Some((mut types1, span1)) = retinf1 else {
            return Ok(Attr::Unit(retinf2, None));
        };

        let Some((types2, span2)) = retinf2 else {
            return Ok(Attr::Unit(Some((types1, span1)), None));
        };

        let retspan = if span1.is_some() {
            span1
        } else {
            span2
        };

        types1.extend(types2.into_iter());

        Ok(Attr::Unit(Some((types1, retspan)), None))
    }

    fn func_param_stop(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let [] = args[..] else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::FuncParams(VecDeque::new()))
    }

    fn func_param_more(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 4] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, _),
            Attr::Type(LangType::Var(TypeVar { var_type, reason })),
            Attr::Id(pool_id, id_span),
            Attr::FuncParams(mut params)
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        params.push_front((pool_id, id_span, TypeVar::new(var_type, reason)));

        Ok(Attr::FuncParams(params))
    }

    fn func_param_none(&mut self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Type(LangType::Var(TypeVar { var_type: Type::Void, reason }))] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        self.symtable_mut().add_params([TypeVar::new(Type::Void, reason)].as_slice());

        Ok(Attr::Unit(None, None))
    }

    fn func_param_some(&mut self, args: Vec<Attr>) -> Result<Attr, Vec<Diag>> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Type(LangType::Var(TypeVar { var_type, reason })),
            Attr::Id(pool_id, id_span),
            Attr::FuncParams(mut params)
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        params.push_front((pool_id, id_span, TypeVar::new(var_type, reason)));

        let mut arg_type = Vec::new();
        let mut diags = Vec::new();

        for (pool_id, sp, var_type) in params.into_iter() {
            arg_type.push(var_type.clone());

            let (success, sym) = self.symtable_mut().push_local(pool_id, var_type, sp.clone());

            if !success && let Some(curr_span) = sp && let Some(old_span) = sym.span {
                let mut diag = Diag::make(DiagKind::Redefinition, curr_span.clone(), true);

                diag.add_span(old_span, DiagSever::Note, Some("previously defined here".into()), false);

                let lexeme = self.symtable().lexeme(pool_id)
                    .expect("can't fail as the symbol was inside the table already");

                diag.add_help(DiagHelp::RepId(lexeme, curr_span));

                diags.push(diag);
            }
        }

        self.symtable_mut().add_params(&arg_type);

        if !diags.is_empty() {
            return Err(diags);
        }

        Ok(Attr::Unit(None, None))
    }

    fn func_ret_none(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Type(LangType::Var(TypeVar { var_type: Type::Void, reason }))] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Type(LangType::new_var(Type::Void, reason)))
    }

    fn func_ret_some(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Type(LangType::Var(TypeVar { var_type, reason }))] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Type(LangType::new_var(var_type, reason)))
    }

    fn func_param(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Unit(None, _), func_args, Attr::Unit(None, _)] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(func_args)
    }

    fn func_name(&mut self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Id(pool_id, id_span)] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let (is_new_insertion, sym) = self.symtable_mut().push_func(pool_id, id_span.clone());

        if !is_new_insertion && let Some(curr_span) = &id_span && let Some(old_span) = sym.span {
            let mut diag = Diag::make(DiagKind::Redefinition, curr_span.clone(), true);

            diag.add_span(old_span, DiagSever::Note, Some("previously defined here".into()), false);

            let lexeme = self.symtable().lexeme(pool_id)
                .expect("can't fail as the symbol was inside the table already");

            diag.add_help(DiagHelp::RepId(lexeme, curr_span.clone()));

            return Err(diag);
        }

        Ok(Attr::Id(pool_id, id_span))
    }

    fn func_rettype(&mut self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Type(LangType::Var(var_type))] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        self.symtable_mut().add_ret_type(var_type.clone());

        Ok(Attr::Type(LangType::Var(var_type)))
    }

    fn func(&mut self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 7] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, _),
            Attr::Type(LangType::Var(ret_type)),
            Attr::Id(_, id_span),
            Attr::Unit(None, _),
            Attr::Unit(None, _),
            Attr::Unit(ret_types, _),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        self.symtable_mut().pop_scope();

        if let Some(ret_types) = ret_types {
            for TypeVar {var_type: rt, reason: rt_span} in ret_types.0 {
                if rt != ret_type.var_type
                    && let Some(rt_span) = rt_span
                    && let Some(rt_reason) = &ret_type.reason {

                    let diag = if ret_type.var_type != Type::Void {
                        let mut diag = Diag::make
                            (DiagKind::MismatchedRetType(rt, ret_type.var_type),
                            rt_span,
                            true
                        );

                        diag.add_span(
                            rt_reason.clone(),
                            DiagSever::Note,
                            Some(format!("expected `{}` due to it's return type", ret_type.var_type)),
                            false
                        );

                        diag
                    } else {
                        let mut diag = Diag::make
                            (DiagKind::UnexpectedRetType(rt),
                            rt_span,
                            true
                        );

                        diag.add_span(
                            rt_reason.clone(),
                            DiagSever::Note,
                            Some("unexpected due to it's return type".into()),
                            false
                        );

                        diag.add_help(DiagHelp::RepRetType(rt, rt_reason.clone()));

                        diag
                    };

                    return Err(diag);
                }
            }
        } else if ret_type.var_type != Type::Void
            && let Some(id_span) = id_span
            && let Some(rt_reason) = ret_type.reason {

            let mut diag = Diag::make(DiagKind::ExpectedRetType, id_span, false);

            diag.add_span(
                rt_reason.clone(),
                DiagSever::Note,
                Some("expected due to it's return type".into()),
                false
            );

            diag.add_help(DiagHelp::RepRetType(Type::Void, rt_reason));

            return Err(diag);
        }

        Ok(Attr::Unit(None, None))
    }

    fn stmnt_ret_none(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let [] = args[..] else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Type(LangType::new_var(Type::Void, None)))
    }

    fn stmnt_ret_some(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Type(LangType::Var(expr_type))] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Type(LangType::Var(expr_type)))
    }

    fn decl_zone_var(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let [] = args[..] else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Unit(None, None))
    }

    fn type_map(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Type(LangType::Var(var_type))] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Type(LangType::Var(var_type)))
    }

    fn stmnt_block_do_while(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 9] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, _),
            Attr::Unit(None, _),
            Attr::Unit(ret_types, _),
            Attr::Unit(None, _),
            Attr::Unit(None, _),
            Attr::Unit(None, _),
            Attr::Type(LangType::Var(expr_type)),
            Attr::Unit(None, _),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        if expr_type.var_type != Type::Bool && let Some(span) = expr_type.reason {
            let diag = Diag::make(
                DiagKind::MismatchedTypes(expr_type.var_type, vec![Type::Bool]),
                span,
                true
            );

            return Err(diag);
        }

        Ok(Attr::Unit(ret_types, None))
    }

    fn stmnt_block_decl_assign(&mut self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 7] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, _),
            Attr::Unit(None, _),
            Attr::Type(LangType::Var(id_type)),
            Attr::Id(pool_id, id_span),
            Attr::Unit(None, _),
            Attr::Type(LangType::Var(expr_type)),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let (is_new_insertion, sym) = self.symtable_mut().push_local(pool_id, id_type.clone(), id_span.clone());

        if !is_new_insertion && let Some(curr_span) = id_span && let Some(old_span) = sym.span {
            let mut diag = Diag::make(DiagKind::Redefinition, curr_span.clone(), true);

            diag.add_span(old_span, DiagSever::Note, Some("previously defined here".into()), false);

            let lexeme = self.symtable().lexeme(pool_id)
                .expect("can't fail as the symbol was inside the table already");

            diag.add_help(DiagHelp::RepId(lexeme, curr_span));

            return Err(diag);
        }

        if id_type.var_type != expr_type.var_type
            && let Some(expr_span) = expr_type.reason
            && let Some(type_span) = id_type.reason {

            let mut diag = Diag::make(
                DiagKind::MismatchedTypes(expr_type.var_type, vec![id_type.var_type]),
                expr_span,
                true,
            );

            diag.add_span(type_span, DiagSever::Note, Some("expected because of this".into()), false);

            return Err(diag);
        }

        Ok(Attr::Unit(None, None))
    }

    fn stmnt_block_decl(&mut self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 5] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, _),
            Attr::Unit(None, _),
            Attr::Type(LangType::Var(id_type)),
            Attr::Id(pool_id, id_span),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let (success, sym) = self.symtable_mut().push_local(pool_id, id_type, id_span.clone());

        if !success && let Some(curr_span) = id_span && let Some(old_span) = sym.span {
            let mut diag = Diag::make(DiagKind::Redefinition, curr_span.clone(), true);

            diag.add_span(old_span, DiagSever::Note, Some("previously defined here".into()), false);

            let lexeme = self.symtable().lexeme(pool_id)
                .expect("can't fail as the symbol was inside the table already");

            diag.add_help(DiagHelp::RepId(lexeme, curr_span));

            return Err(diag);
        }

        Ok(Attr::Unit(None, None))
    }

    fn stmnt_block_if(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 5] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, _),
            Attr::Unit(None, _),
            Attr::Type(LangType::Var(expr_type)),
            Attr::Unit(None, _),
            Attr::Unit(ret_types, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        if expr_type.var_type != Type::Bool && let Some(span) = expr_type.reason {
            let diag = Diag::make(
                DiagKind::MismatchedTypes(expr_type.var_type, vec![Type::Bool]),
                span,
                true
            );

            return Err(diag);
        }

        Ok(Attr::Unit(ret_types, None))
    }

    fn stmnt(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Unit(ret_types, _)] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Unit(ret_types, None))
    }

    fn func_arg_stop(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let [] = args[..] else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::FuncArgs(VecDeque::new()))
    }

    fn func_arg_more(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, _),
            Attr::Type(LangType::Var(expr_type)),
            Attr::FuncArgs(mut args),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        args.push_front(expr_type);

        Ok(Attr::FuncArgs(args))
    }

    fn func_arg_none(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let [] = args[..] else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::FuncArgs(VecDeque::new()))
    }

    fn func_arg_some(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 2] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Type(LangType::Var(expr_type)),
            Attr::FuncArgs(mut args),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        args.push_front(expr_type);

        Ok(Attr::FuncArgs(args))
    }

    fn stmnt_ret(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, span_ret),
            Attr::Type(LangType::Var(mut ret_type)),
            Attr::Unit(None, span_semi),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let span = if let Some(span_ret) = span_ret && let Some(span_semi) = span_semi {
            let span = Span::new(span_ret.start, span_semi.end);

            if ret_type.var_type == Type::Void {
                ret_type.reason = Some(span.clone());
            }

            Some(span)
        } else {
            None
        };

        Ok(Attr::Unit(Some((vec![ret_type], span.clone())), span))
    }

    fn stmnt_read(&mut self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, read_span),
            Attr::Id(pool_id, id_span),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        if let Some(sym) = self.symtable().search(pool_id) {
            let Some(span) = &sym.span else {
                return Ok(Attr::Unit(None, None));
            };

            let LangType::Var(sym_type) = &sym.lang_type else {
                let diag = Diag::make(
                    DiagKind::MismatchedTypes(
                        Type::Func,
                        vec![Type::Str, Type::Int, Type::Float],
                    ),
                    span.clone(),
                    true
                );

                return Err(diag);
            };

            if ![Type::Str, Type::Int, Type::Float].contains(&sym_type.var_type) && let Some(read_span) = read_span {
                let mut diag = Diag::make(
                    DiagKind::MismatchedTypes(
                        sym_type.var_type,
                        vec![Type::Str, Type::Int, Type::Float],
                    ),
                    span.clone(),
                    true
                );

                diag.add_span(read_span, DiagSever::Note, Some("expected due to this I/O operation".into()), false);

                return Err(diag);
            }
        } else if id_span.is_some() {
            let _ = self.symtable_mut().push_global(pool_id, TypeVar::new(Type::Int, id_span.clone()), id_span);
        }


        Ok(Attr::Unit(None, None))
    }

    fn stmnt_write(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, write_span),
            Attr::Type(LangType::Var(expr_type)),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        if ![Type::Str, Type::Int, Type::Float].contains(&expr_type.var_type)
            && let Some(span) = expr_type.reason
            && let Some(write_span) = write_span
        {
            let mut diag = Diag::make(
                DiagKind::MismatchedTypes(
                    expr_type.var_type,
                    vec![Type::Str, Type::Int, Type::Float],
                ),
                span.clone(),
                true
            );

            diag.add_span(write_span, DiagSever::Note, Some("expected due to this I/O operation".into()), false);

            return Err(diag);
        }
        
        Ok(Attr::Unit(None, None))
    }

    fn stmnt_call(&self, args: Vec<Attr>) -> Result<Attr, Vec<Diag>> {
        let args: [Attr; 5] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Id(pool_id, id_span),
            Attr::Unit(None, call_start),
            Attr::FuncArgs(mut args),
            Attr::Unit(None, call_end),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let arg_span = if let Some(Some(arg_start)) = args.front().map(|arg| &arg.reason)
            && let Some(Some(arg_end)) = args.back().map(|arg| &arg.reason) {

            Some(Span::new(arg_start.start, arg_end.end))
        } else {
            None
        };

        let call_span = if let Some(call_start) = call_start && let Some(call_end) = call_end {
            Some(Span::new(call_start.start, call_end.end))
        } else {
            None
        };

        let Some(sym) = self.symtable().search(pool_id) else {
            if let Some(span) = id_span {
                let lexeme = self.symtable().lexeme(pool_id)
                    .expect("can't fail as the symbol was inside the table already");

                let diag = Diag::make(
                    DiagKind::UndefinedFunc(lexeme),
                    span,
                    true
                );

                return Err(vec![diag]);
            }

            return Ok(Attr::Unit(None, None));
        };

        let LangType::Func(func_type) = &sym.lang_type else {
            if let Some(span) = id_span && let Some(call_span) = call_span {

                let mut diag = Diag::make(
                    DiagKind::MismatchedTypes(sym.lang_type.main_type(), vec![Type::Func]),
                    span,
                    true
                );

                diag.add_span(
                    call_span,
                    DiagSever::Note,
                    Some("call expression requires a function".into()),
                    false
                );

                return Err(vec![diag]);
            }

            return Ok(Attr::Unit(None, None));
        };

        if let Some(arg_first) = args.front_mut() && arg_first.var_type == Type::Void {
            arg_first.reason = call_span.clone();
        }

        let param_iter = func_type
            .param_type
            .iter()
            .filter(|v| v.var_type != Type::Void)
            .collect::<Vec<&TypeVar>>();

        if param_iter.len() != args.len() {
            let count_span = if args.len() == 0 {
                call_span
            } else {
                arg_span
            };

            if let Some(count_span) = &count_span && let Some(func_span) = &sym.span {
                let mut diag = Diag::make(
                    DiagKind::InvalidCall(args.len(), param_iter.len()),
                    count_span.clone(),
                    true
                );

                diag.add_span(
                    func_span.clone(),
                    DiagSever::Note,
                    Some("expected due to it's parameter list".into()),
                    false
                );

                return Err(vec![diag]);
            }

            return Ok(Attr::Unit(None, None));
        }

        let mut diags = Vec::new();

        for (arg_type, param_type) in args.iter().zip(param_iter.iter()) {
            if arg_type.var_type != param_type.var_type
                && let Some(arg_reason) = &arg_type.reason
                && let Some(param_reason) = &param_type.reason {

                let mut diag = Diag::make(
                    DiagKind::MismatchedTypes(arg_type.var_type, vec![param_type.var_type]),
                    arg_reason.clone(),
                    true
                );

                diag.add_span(
                    param_reason.clone(),
                    DiagSever::Note,
                    Some(format!("expected `{}` due to this parameter type", param_type.var_type)),
                    false,
                );

                if param_type.var_type == Type::Void && let Some(arg_span) = &arg_span {
                    diag.add_help(DiagHelp::DelArgs(arg_span.clone()));
                }

                diags.push(diag);
            }
        }

        if !diags.is_empty() {
            return Err(diags);
        }

        Ok(Attr::Unit(None, None))
    }

    fn stmnt_andassign(&mut self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 4] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Id(pool_id, id_span),
            Attr::Unit(None, andassign_span),
            Attr::Type(LangType::Var(expr_type)),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        if expr_type.var_type != Type::Bool && let Some(span) = expr_type.reason && let Some(andassign_span) = andassign_span {
            let mut diag = Diag::make(
                DiagKind::MismatchedTypes(expr_type.var_type, vec![Type::Bool]),
                span,
                true
            );

            diag.add_span(
                andassign_span,
                DiagSever::Note,
                Some("expected `boolean` due to this logical operator".into()),
                false
            );

            return Err(diag);
        }

        let Some(sym) = self.symtable().search(pool_id) else {
            if let Some(span) = id_span && let Some(andassign_span) = andassign_span {
                let _ = self.symtable_mut().push_global(pool_id, TypeVar::new(Type::Int, Some(span.clone())), Some(span.clone()));

                let mut diag = Diag::make(
                    DiagKind::MismatchedTypes(Type::Int, vec![Type::Bool]),
                    span,
                    true
                );

                diag.add_span(
                    andassign_span,
                    DiagSever::Note,
                    Some("expected `boolean` due to this logical operator".into()),
                    false
                );

                return Err(diag);
            }

            return Ok(Attr::Unit(None, None));
        };

        let LangType::Var(var_type) = &sym.lang_type else {
            if let Some(span) = id_span {

                let diag = Diag::make(
                    DiagKind::MismatchedTypes(Type::Func, vec![Type::Bool]),
                    span.clone(),
                    true
                );

                return Err(diag);
            }

            return Ok(Attr::Unit(None, None));
        };

        if var_type.var_type != Type::Bool && let Some(span) = &var_type.reason && let Some(andassign_span) = andassign_span {
            let mut diag = Diag::make(
                DiagKind::MismatchedTypes(var_type.var_type, vec![Type::Bool]),
                span.clone(),
                true
            );

            diag.add_span(
                andassign_span,
                DiagSever::Note,
                Some("expected due to this operator".into()),
                false
            );

            return Err(diag);
        }

        Ok(Attr::Unit(None, None))
    }

    fn stmnt_assign(&mut self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 4] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Id(pool_id, id_span),
            Attr::Unit(None, _),
            Attr::Type(LangType::Var(expr_type)),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let Some(sym) = self.symtable().search(pool_id) else {
            let _ = self.symtable_mut().push_global(pool_id, TypeVar::new(Type::Int, id_span.clone()), id_span.clone());

            if expr_type.var_type != Type::Int && let Some(expr_span) = expr_type.reason && let Some(id_span) = id_span {
                let mut diag = Diag::make(
                    DiagKind::MismatchedTypes(expr_type.var_type, vec![Type::Int]),
                    expr_span,
                    true
                );

                diag.add_span(id_span, DiagSever::Note, Some("expected `int` due to this implicit declaration".into()), false);

                return Err(diag);
            }

            return Ok(Attr::Unit(None, None));
        };

        let valid_types = vec![Type::Int, Type::Float, Type::Str, Type::Bool];

        if !valid_types.contains(&expr_type.var_type) {
            if let Some(span) = expr_type.reason {
                let diag = Diag::make(
                    DiagKind::MismatchedTypes(expr_type.var_type, valid_types),
                    span.clone(),
                    true
                );

                return Err(diag);
            }

            return Ok(Attr::Unit(None, None))
        }

        let LangType::Var(var_type) = &sym.lang_type else {
            if let Some(curr_span) = id_span && let Some(old_span) = &sym.span {
                let mut diag = Diag::make(
                    DiagKind::Redefinition,
                    curr_span.clone(),
                    true
                );

                diag.add_span(old_span.clone(), DiagSever::Note, Some("previously defined here".into()), false);

                let lexeme = self.symtable().lexeme(pool_id)
                    .expect("can't fail as the symbol was inside the table already");

                diag.add_help(DiagHelp::RepId(lexeme, curr_span));

                return Err(diag);
            }

            return Ok(Attr::Unit(None, None));
        };

        if var_type.var_type != expr_type.var_type
            && let Some(reason) = &var_type.reason
            && let Some(span) = &expr_type.reason
        {
            let mut diag = Diag::make(
                DiagKind::MismatchedTypes(expr_type.var_type, vec![var_type.var_type]),
                span.clone(),
                true
            );

            if sym.implicit {
                diag.add_span(reason.clone(), DiagSever::Note, Some("expected `int` due to this implicit declaration".into()), false);
            } else {
                diag.add_span(reason.clone(), DiagSever::Note, Some("expected because of this".into()), false);
            }

            return Err(diag);
        }

        Ok(Attr::Unit(None, None))
    }

    fn expr_id(&mut self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Id(pool_id, id_span)] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let Some(sym) = self.symtable().search(pool_id) else {
            if let Some(span) = id_span {
                let _ = self.symtable_mut().push_global(pool_id, TypeVar::new(Type::Int, Some(span.clone())), Some(span.clone()));
            }

            return Ok(Attr::Type(LangType::new_var(Type::Void, None)));
        };

        let LangType::Var(var_type) = &sym.lang_type else {
            if let Some(span) = &id_span {
                let mut diag = Diag::make(
                    DiagKind::MismatchedTypes(Type::Func, vec![Type::Int, Type::Float, Type::Str, Type::Bool]),
                    span.clone(),
                    true,
                );

                diag.add_help(DiagHelp::InsCall(span.clone()));

                return Err(diag);
            }

            return Ok(Attr::Type(LangType::new_var(Type::Void, None)));
        };

        Ok(Attr::Type(LangType::new_var(var_type.var_type, id_span)))
    }

    fn expr_wrap(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, start_span),
            Attr::Type(LangType::Var(expr_type)),
            Attr::Unit(None, end_span),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let span = if let Some(start_span) = start_span && let Some(end_span) = end_span {
            Some(Span::new(start_span.start, end_span.end))
        } else {
            None
        };

        Ok(Attr::Type(LangType::new_var(expr_type.var_type, span)))
    }

    fn expr_call(&self, args: Vec<Attr>) -> Result<Attr, Vec<Diag>> {
        let args: [Attr; 4] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Id(pool_id, id_span),
            Attr::Unit(None, call_start),
            Attr::FuncArgs(mut args),
            Attr::Unit(None, call_end),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let full_span = if let Some(id_span) = &id_span && let Some(call_end) = &call_end {
            Some(Span::new(id_span.start, call_end.end))
        } else {
            None
        };

        let arg_span = if let Some(Some(arg_start)) = args.front().map(|arg| &arg.reason)
            && let Some(Some(arg_end)) = args.back().map(|arg| &arg.reason) {

            Some(Span::new(arg_start.start, arg_end.end))
        } else {
            None
        };

        let call_span = if let Some(call_start) = &call_start && let Some(call_end) = &call_end {
            Some(Span::new(call_start.start, call_end.end))
        } else {
            None
        };

        let Some(sym) = self.symtable().search(pool_id) else {
            if let Some(span) = id_span {
                let lexeme = self.symtable().lexeme(pool_id)
                    .expect("can't fail as the symbol was inside the table already");

                let diag = Diag::make(
                    DiagKind::UndefinedFunc(lexeme),
                    span,
                    true
                );

                return Err(vec![diag]);
            }

            return Ok(Attr::Type(LangType::new_var(Type::Void, None)));
        };

        let LangType::Func(func_type) = &sym.lang_type else {
            if let Some(span) = id_span && let Some(call_span) = call_span {

                let mut diag = Diag::make(
                    DiagKind::MismatchedTypes(sym.lang_type.main_type(), vec![Type::Func]),
                    span,
                    true
                );

                diag.add_span(
                    call_span.clone(),
                    DiagSever::Note,
                    Some("call expression requires a function".into()),
                    false
                );

                diag.add_help(DiagHelp::DelCall(call_span));

                return Err(vec![diag]);
            }

            return Ok(Attr::Type(LangType::new_var(Type::Void, None)));
        };

        if let Some(arg_first) = args.front_mut() && arg_first.var_type == Type::Void {
            arg_first.reason = call_span.clone();
        }

        let param_iter = func_type
            .param_type
            .iter()
            .filter(|v| v.var_type != Type::Void)
            .collect::<Vec<&TypeVar>>();

        if param_iter.len() != args.len() {
            let count_span = if args.len() == 0 {
                call_span
            } else {
                arg_span
            };

            if let Some(count_span) = &count_span && let Some(func_span) = &sym.span {
                let mut diag = Diag::make(
                    DiagKind::InvalidCall(args.len(), param_iter.len()),
                    count_span.clone(),
                    true
                );

                diag.add_span(
                    func_span.clone(),
                    DiagSever::Note,
                    Some("expected due to this parameter list".into()),
                    false
                );

                return Err(vec![diag]);
            }

            return Ok(Attr::Type(LangType::new_var(Type::Void, None)));
        }

        let mut diags = Vec::new();
        let mut success = true;

        for (arg_type, param_type) in args.iter().zip(param_iter.iter()) {
            if arg_type.var_type != param_type.var_type {
                if let Some(arg_reason) = &arg_type.reason
                    && let Some(param_reason) = &param_type.reason {

                    let mut diag = Diag::make(
                        DiagKind::MismatchedTypes(arg_type.var_type, vec![param_type.var_type]),
                        arg_reason.clone(),
                        true
                    );

                    diag.add_span(
                        param_reason.clone(),
                        DiagSever::Note,
                        Some(format!("expected `{}` due to this parameter type", param_type.var_type)),
                        false,
                    );

                    if param_type.var_type == Type::Void && let Some(arg_span) = &arg_span {
                        diag.add_help(DiagHelp::DelArgs(arg_span.clone()));
                    }

                    diags.push(diag);
                }

                success = false;
            }
        }

        if !diags.is_empty() {
            return Err(diags);
        }


        if !success {
            return Ok(Attr::Type(LangType::new_var(Type::Void, None)));
        }

        Ok(Attr::Type(LangType::new_var(func_type.ret_type.var_type, full_span)))
    }

    fn expr_oper_bin_num(&mut self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Type(LangType::Var(expr_type1)),
            Attr::Unit(None, oper_span),
            Attr::Type(LangType::Var(expr_type2)),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        if let Some(oper_span) = oper_span {
            let valid_types = vec![Type::Int, Type::Float];

            if !valid_types.contains(&expr_type1.var_type) && let Some(span) = expr_type1.reason {
                let mut diag = Diag::make(
                    DiagKind::MismatchedTypes(expr_type1.var_type, valid_types),
                    span,
                    true
                );

                diag.add_span(
                    oper_span,
                    DiagSever::Note,
                    Some("expected due to this arithmetic operator".into()),
                    false
                );

                return Err(diag);
            }

            if !valid_types.contains(&expr_type2.var_type) && let Some(span) = expr_type2.reason{
                let mut diag = Diag::make(
                    DiagKind::MismatchedTypes(expr_type2.var_type, valid_types),
                    span,
                    true
                );

                diag.add_span(
                    oper_span,
                    DiagSever::Note,
                    Some("expected due to this arithmetic operator".into()),
                    false
                );

                return Err(diag);
            }
        }

        if expr_type1.var_type != expr_type2.var_type
            && let Some(span1) = &expr_type1.reason
            && let Some(span2) = expr_type2.reason {

            let mut diag = Diag::make(
                DiagKind::MismatchedTypes(expr_type2.var_type, vec![expr_type1.var_type]),
                span2,
                true
            );

            diag.add_span(
                span1.clone(),
                DiagSever::Note,
                Some(format!("because this has type `{}`", expr_type1.var_type)),
                false
            );

            return Err(diag);
        }

        let span = if let Some(expr1_span) = expr_type1.reason && let Some(expr2_span) = expr_type2.reason {
            Some(Span::new(expr1_span.start, expr2_span.end))
        } else {
            None
        };

        Ok(Attr::Type(LangType::new_var(expr_type1.var_type, span)))
    }

    fn expr_oper_bin_cmp(&mut self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Type(LangType::Var(expr_type1)),
            Attr::Unit(None, oper_span),
            Attr::Type(LangType::Var(expr_type2)),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        if let Some(oper_span) = oper_span {
            let valid_types = vec![Type::Int, Type::Float];

            if !valid_types.contains(&expr_type1.var_type) && let Some(span) = expr_type1.reason {
                let mut diag = Diag::make(
                    DiagKind::MismatchedTypes(expr_type1.var_type, valid_types),
                    span,
                    true
                );

                diag.add_span(
                    oper_span,
                    DiagSever::Note,
                    Some("expected due to this relational operator".into()),
                    false
                );

                return Err(diag);
            }

            if !valid_types.contains(&expr_type2.var_type) && let Some(span) = expr_type2.reason{
                let mut diag = Diag::make(
                    DiagKind::MismatchedTypes(expr_type2.var_type, valid_types),
                    span,
                    true
                );

                diag.add_span(
                    oper_span,
                    DiagSever::Note,
                    Some("expected due to this relational operator".into()),
                    false
                );

                return Err(diag);
            }
        }

        if expr_type1.var_type != expr_type2.var_type
            && let Some(span1) = &expr_type1.reason
            && let Some(span2) = expr_type2.reason {

            let mut diag = Diag::make(
                DiagKind::MismatchedTypes(expr_type2.var_type, vec![expr_type1.var_type]),
                span2,
                true
            );

            diag.add_span(
                span1.clone(),
                DiagSever::Note,
                Some(format!("because this has type `{}`", expr_type1.var_type)),
                false
            );

            return Err(diag);
        }

        let span = if let Some(expr1_span) = expr_type1.reason && let Some(expr2_span) = expr_type2.reason {
            Some(Span::new(expr1_span.start, expr2_span.end))
        } else {
            None
        };

        Ok(Attr::Type(LangType::new_var(expr_type1.var_type, span)))
    }

    fn expr_oper_bin_bool(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Type(LangType::Var(expr_type1)),
            Attr::Unit(None, oper_span),
            Attr::Type(LangType::Var(expr_type2)),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };


        if let Some(oper_span) = oper_span {
            let valid_types = vec![Type::Bool];

            if !valid_types.contains(&expr_type1.var_type) && let Some(span) = expr_type1.reason {
                let mut diag = Diag::make(
                    DiagKind::MismatchedTypes(expr_type1.var_type, valid_types),
                    span,
                    true
                );

                diag.add_span(
                    oper_span,
                    DiagSever::Note,
                    Some("expected `boolean` due to this logical operator".into()),
                    false
                );

                return Err(diag);
            }

            if !valid_types.contains(&expr_type2.var_type) && let Some(span) = expr_type2.reason{
                let mut diag = Diag::make(
                    DiagKind::MismatchedTypes(expr_type2.var_type, valid_types),
                    span,
                    true
                );

                diag.add_span(
                    oper_span,
                    DiagSever::Note,
                    Some("expected `boolean` due to this logical operator".into()),
                    false
                );

                return Err(diag);
            }
        }

        let span = if let Some(expr1_span) = expr_type1.reason && let Some(expr2_span) = expr_type2.reason {
            Some(Span::new(expr1_span.start, expr2_span.end))
        } else {
            None
        };

        Ok(Attr::Type(LangType::new_var(Type::Bool, span)))
    }

    fn expr_oper_unr_bool(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 2] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, oper_span),
            Attr::Type(LangType::Var(expr_type)),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };


        if expr_type.var_type != Type::Bool
            && let Some(oper_span) = &oper_span
            && let Some(span) = expr_type.reason {

            let mut diag = Diag::make(
                DiagKind::MismatchedTypes(expr_type.var_type, vec![Type::Bool]),
                span,
                true
            );

            diag.add_span(
                oper_span.clone(),
                DiagSever::Note,
                Some("expected `boolean` due to this logical operator".into()),
                false
            );

            return Err(diag);
        }

        let span = if let Some(oper_span) = oper_span && let Some(expr_span) = expr_type.reason {
            Some(Span::new(oper_span.start, expr_span.end))
        } else {
            None
        };

        Ok(Attr::Type(LangType::new_var(expr_type.var_type, span)))
    }

    fn expr_oper_unr_num(&self, args: Vec<Attr>) -> Result<Attr, Diag> {
        let args: [Attr; 2] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, oper_span),
            Attr::Type(LangType::Var(expr_type)),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let valid_types = vec![Type::Int, Type::Float];

        if !valid_types.contains(&expr_type.var_type)
            && let Some(oper_span) = &oper_span
            && let Some(span) = expr_type.reason {

            let mut diag = Diag::make(
                DiagKind::MismatchedTypes(expr_type.var_type, valid_types),
                span,
                true
            );

            diag.add_span(
                oper_span.clone(),
                DiagSever::Note,
                Some("expected due to this arithmetic operator".into()),
                false
            );

            return Err(diag);
        }

        let span = if let Some(oper_span) = oper_span && let Some(expr_span) = expr_type.reason {
            Some(Span::new(oper_span.start, expr_span.end))
        } else {
            None
        };

        Ok(Attr::Type(LangType::new_var(expr_type.var_type, span)))
    }
}

#[derive(Debug)]
enum SemRule {
    Axiom,
    Lambda,
    BlockMore,
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
    StmntRetNone,
    StmntRetSome,
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
    ExprWrap,
    ExprCall,
    ExprOperBinNum,
    ExprOperBinCmp,
    ExprOperBinBool,
    ExprOperUnrBool,
    ExprOperUnrNum,
}

impl SemRule {
    pub fn from_idx(idx: usize) -> Self {
        match idx {
            0 => SemRule::Axiom,
            1 | 4 => SemRule::Lambda,
            2 | 3 => SemRule::BlockMore,
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
            16 => SemRule::StmntRetNone,
            17 => SemRule::StmntRetSome,
            18 => SemRule::DeclZoneVar,
            19..=22 | 46 | 48 | 50 | 52 | 54 | 56 => SemRule::TypeMap,
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
            39..=43 => SemRule::TypeMap,
            44 => SemRule::ExprWrap,
            45 => SemRule::ExprCall,
            49 | 51 => SemRule::ExprOperBinNum,
            53 | 55 => SemRule::ExprOperBinCmp,
            57 => SemRule::ExprOperBinBool,
            47 => SemRule::ExprOperUnrBool,
            58 | 59 => SemRule::ExprOperUnrNum,
            _ => unreachable!(
                "unexpected invalid rule index: got {} but len is: {}",
                idx, Grammar::LEN,
            )
        }
    }
}
