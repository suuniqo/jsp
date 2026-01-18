use std::collections::{HashSet, VecDeque};

use crate::{
    diag::{Diag, DiagKind, DiagRef, DiagHelp},
    types::{TypeId, Type, TypeVar},
    span::Span,
    symtable::SymTable,
};

use super::{
    analyzer::SemAnalyzer,
    rule::SemRule,
    attr::Attr,
};


pub(super) struct SemAction<'a, 's> {
    analyzer: &'a mut SemAnalyzer<'s>,
    rule: SemRule,
}

impl<'a, 's> SemAction<'a, 's> {
    const EXPECTED_NUM: [Type; 2] = [Type::Int, Type::Float];
    const EXPECTED_IO:  [Type; 3] = [Type::Str, Type::Int, Type::Float];
    const EXPECTED_VAR: [Type; 4] = [Type::Str, Type::Int, Type::Float, Type::Bool];

    pub(super) fn new(analyzer: &'a mut SemAnalyzer<'s>, rule_idx: usize) -> Self {
        Self {
            analyzer,
            rule: SemRule::from_idx(rule_idx),
        }
    }

    fn symtable(&self) -> &dyn SymTable {
        self.analyzer.symtable()
    }

    fn symtable_mut(&mut self) -> &mut dyn SymTable {
        self.analyzer.symtable_mut()
    }

    fn recovery_func(&mut self, _args: Vec<Option<Attr>>) {
        if self.symtable().scopes() > 1 {
            self.symtable_mut().pop_scope();
        }
    }

    fn recovery_stmnt_block_decl_assign(&mut self, args: Vec<Option<Attr>>) {
        let args: [Option<Attr>; 7] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            _,
            _,
            Some(Attr::Type(TypeId::Var(id_type))),
            Some(Attr::Id(pool_id, id_span)),
            _,
            _,
            _,
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let _ = self.symtable_mut().push_local(pool_id, id_type.clone(), id_span.clone());
    }

    fn recovery_stmnt_assign(&mut self, args: Vec<Option<Attr>>) {
        let args: [Option<Attr>; 4] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Some(Attr::Id(pool_id, id_span)),
            _,
            _,
            _,
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        if self.symtable().search(pool_id).is_none() {
            let _ = self.symtable_mut().push_global(pool_id, TypeVar::new(Type::Int, id_span.clone()), id_span.clone());
        };
    }

    pub(super) fn run(&mut self, args: Vec<Option<Attr>>) -> Result<Option<Attr>, Vec<DiagRef>> {
        let Some(args) = args.clone().into_iter().collect::<Option<Vec<Attr>>>() else {
            match self.rule {
                SemRule::Func                                  => self.recovery_func(args),
                SemRule::StmntBlockDeclAssign                  => self.recovery_stmnt_block_decl_assign(args),
                SemRule::StmntAssign | SemRule::StmntAndassign => self.recovery_stmnt_assign(args),
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
            SemRule::FuncParamSome        => return self.func_param_some(args).map(Some),
            SemRule::FuncRetNone          => self.func_ret_none(args),
            SemRule::FuncRetSome          => self.func_ret_some(args),
            SemRule::FuncParam            => self.func_param(args),
            SemRule::FuncName             => self.func_name(args),
            SemRule::FuncRettype          => self.func_rettype(args),
            SemRule::Func                 => self.func(args),
            SemRule::FuncDecl             => return self.func_decl(args).map(Some),
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
            SemRule::StmntCall            => return self.stmnt_call(args).map(Some),
            SemRule::StmntAndassign       => self.stmnt_andassign(args),
            SemRule::StmntAssign          => self.stmnt_assign(args),
            SemRule::ExprId               => return self.expr_id(args).map_err(|diag| vec![diag]),
            SemRule::ExprMap              => self.expr_map(args),
            SemRule::ExprWrap             => self.expr_wrap(args),
            SemRule::ExprCall             => return self.expr_call(args),
            SemRule::ExprOperBinNum       => self.expr_oper_bin_num(args),
            SemRule::ExprOperBinCmp       => self.expr_oper_bin_cmp(args),
            SemRule::ExprOperBinBool      => self.expr_oper_bin_bool(args),
            SemRule::ExprOperUnrBool      => self.expr_oper_unr_bool(args),
            SemRule::ExprOperUnrNum       => self.expr_oper_unr_num(args),
        }.map(Some).map_err(|diag| vec![diag])
    }

    fn mismatched_types(&self, span: Span, found: Type, expected: Vec<Type>, pool_id: Option<usize>, extra: Option<Span>) -> DiagRef {
        let mut diag = Diag::make(
            DiagKind::MismatchedTypes(found, expected),
            span.clone(),
            true
        );

        if let Some(pool_id) = pool_id && let Some(sym) = self.symtable().search(pool_id) && let Some(sym_span) = &sym.span {
            let def_span = {
                let reason = match &sym.lang_type {
                    TypeId::Var(type_var) => &type_var.reason,
                    TypeId::Func(type_func) => &type_func.ret_type.reason,
                };

                if let Some(reason) = reason {
                    Span::new(reason.start, sym_span.end)
                } else {
                    sym_span.clone()
                }
            };

            if !span.intersect(sym_span) && extra.is_none_or(|extra| !extra.intersect(sym_span)) {
                if sym.implicit {
                    diag.add_note(def_span, &format!("found `{}` due to this implicit declaration", found));
                } else {
                    diag.add_note(def_span, &format!("found `{}` due to this declaration", found));
                }
            }
        }

        diag
    }

    fn redefinition(&self, curr_span: Span, old_span: Span, pool_id: usize) -> DiagRef {
        let mut diag = Diag::make(DiagKind::Redefinition, curr_span.clone(), true);

        diag.add_note(old_span, "previously defined here");

        let lexeme = self.symtable().lexeme(pool_id)
            .expect("can't fail as the symbol was inside the table already");

        diag.with_help(DiagHelp::RepId(lexeme, curr_span))
    }

    fn undefined_func(&self, span: Span, pool_id: usize) -> DiagRef {
        let lexeme = self.symtable().lexeme(pool_id)
            .expect("can't fail as the symbol was inside the table already");

        Diag::make(DiagKind::UndefinedFunc(lexeme), span, true)
    }

    fn axiom(&mut self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let [Attr::Unit(None, None)] = args[..] else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        self.symtable_mut().pop_scope();

        Ok(Attr::Unit(None, None))
    }

    fn lambda(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let [] = args[..] else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Unit(None, None))
    }

    fn block_more(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
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

    fn func_block_stmnt(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
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

        types1.extend(types2);

        Ok(Attr::Unit(Some((types1, retspan)), None))
    }

    fn func_param_stop(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let [] = args[..] else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::FuncParams(VecDeque::new()))
    }

    fn func_param_more(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 4] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, _),
            Attr::Type(TypeId::Var(TypeVar { var_type, reason })),
            Attr::Id(pool_id, id_span),
            Attr::FuncParams(mut params)
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        params.push_front((pool_id, id_span, TypeVar::new(var_type, reason)));

        Ok(Attr::FuncParams(params))
    }

    fn func_param_none(&mut self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Type(TypeId::Var(TypeVar { var_type: Type::Void, reason }))] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Type(TypeId::new_var(Type::Void, reason)))
    }

    fn func_param_some(&mut self, args: Vec<Attr>) -> Result<Attr, Vec<DiagRef>> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Type(TypeId::Var(TypeVar { var_type, reason })),
            Attr::Id(pool_id, id_span),
            Attr::FuncParams(mut params)
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        params.push_front((pool_id, id_span, TypeVar::new(var_type, reason)));

        Ok(Attr::FuncParams(params))
    }

    fn func_ret_none(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Type(TypeId::Var(TypeVar { var_type: Type::Void, reason }))] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Type(TypeId::new_var(Type::Void, reason)))
    }

    fn func_ret_some(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Type(TypeId::Var(TypeVar { var_type, reason }))] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Type(TypeId::new_var(var_type, reason)))
    }

    fn func_param(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Unit(None, _), func_args, Attr::Unit(None, _)] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(func_args)
    }

    fn func_name(&mut self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Id(pool_id, id_span)] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Id(pool_id, id_span))
    }

    fn func_rettype(&mut self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Type(TypeId::Var(var_type))] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Type(TypeId::Var(var_type)))
    }

    fn func_decl(&mut self, args: Vec<Attr>) -> Result<Attr, Vec<DiagRef>> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Type(TypeId::Var(ret_type)),
            Attr::Id(pool_id, id_span),
            params,
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let (param_type, params) = if let Attr::Type(TypeId::Var(void_type)) = params {
            (vec![void_type], None)
        } else if let Attr::FuncParams(params) = params {
            let param_types = params.iter().map(|(.., var_type)| var_type.clone()).collect();

            (param_types, Some(params))
        } else {
            unreachable!("invalid param type in func declaration action");
        };

        let (is_new_insertion, sym) = self.symtable_mut().push_func(
            pool_id,
            &param_type,
            ret_type.clone(),
            id_span.clone()
        );

        let mut diags = Vec::new();

        if !is_new_insertion && let Some(curr_span) = &id_span && let Some(old_span) = sym.span {
            diags.push(self.redefinition(curr_span.clone(), old_span, pool_id));
        }

        if let Some(params) = params {
            let mut arg_type = Vec::new();

            for (pool_id, sp, var_type) in params.into_iter() {
                arg_type.push(var_type.clone());

                let (success, sym) = self.symtable_mut().push_local(pool_id, var_type, sp.clone());

                if !success && let Some(curr_span) = sp && let Some(old_span) = sym.span {
                    diags.push(self.redefinition(curr_span, old_span, pool_id));
                }
            }
        }

        if !diags.is_empty() {
            return Err(diags);
        }

        Ok(Attr::Type(TypeId::Var(ret_type)))
    }

    fn func(&mut self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 5] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, _),
            Attr::Type(TypeId::Var(ret_type)),
            Attr::Unit(None, _),
            Attr::Unit(ret_types, _),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        self.symtable_mut().pop_scope();

        if let Some(ret_types) = ret_types {
            let mut maybe_diag: Option<DiagRef> = None;
            let mut mismatched = HashSet::new();

            if let Some(rt_reason) = &ret_type.reason {
                for TypeVar {var_type: rt, reason: rt_span} in ret_types.0 {
                    if rt != ret_type.var_type
                        && let Some(rt_span) = rt_span {

                        if let Some(diag) = &mut maybe_diag {
                            let msg = if matches!(diag.kind, DiagKind::MismatchedRetType(..)) {
                                format!("expected `{}`, found `{}`", ret_type.var_type, rt)
                            } else {
                                format!("unexpected `{}`", rt)
                            };

                            mismatched.insert(rt);

                            diag.add_error(rt_span, &msg);
                        } else if ret_type.var_type != Type::Void {
                            let mut diag = Diag::make
                                (DiagKind::MismatchedRetType(rt, ret_type.var_type),
                                rt_span,
                                true
                            );

                            mismatched.insert(rt);

                            diag.add_note(
                                rt_reason.clone(),
                                &format!("expected `{}` due to it's return type", ret_type.var_type)
                            );

                            maybe_diag = Some(diag);
                        } else {
                            let mut diag = Diag::make
                                (DiagKind::UnexpectedRetType(rt),
                                rt_span,
                                true
                            );

                            diag.add_note(rt_reason.clone(), "unexpected due to it's return type");

                            mismatched.insert(rt);

                            maybe_diag = Some(diag);
                        };
                    }
                }

                if let Some(mut diag) = maybe_diag {
                    if mismatched.len() == 1 && let Some(suggestion) = mismatched.into_iter().next() {
                        diag.add_help(DiagHelp::RepRetType(suggestion, rt_reason.clone()));
                    }

                    return Err(diag)
                }
            }
        }

        Ok(Attr::Unit(None, None))
    }

    fn stmnt_ret_none(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let [] = args[..] else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Expr(TypeId::new_var(Type::Void, None), None))
    }

    fn stmnt_ret_some(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Expr(TypeId::Var(expr_type), pool_id)] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Expr(TypeId::Var(expr_type), pool_id))
    }

    fn decl_zone_var(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let [] = args[..] else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Unit(None, None))
    }

    fn type_map(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Type(TypeId::Var(var_type))] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Type(TypeId::Var(var_type)))
    }

    fn stmnt_block_do_while(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
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
            Attr::Expr(TypeId::Var(expr_type), pool_id),
            Attr::Unit(None, _),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        if expr_type.var_type != Type::Bool && let Some(span) = expr_type.reason {
            return Err(self.mismatched_types(span, expr_type.var_type, vec![Type::Bool], pool_id, None));
        }

        Ok(Attr::Unit(ret_types, None))
    }

    fn stmnt_block_decl_assign(&mut self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 7] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, _),
            Attr::Unit(None, _),
            Attr::Type(TypeId::Var(id_type)),
            Attr::Id(pool_id, id_span),
            Attr::Unit(None, _),
            Attr::Expr(TypeId::Var(expr_type), expr_pool_id),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let (is_new_insertion, sym) = self.symtable_mut().push_local(pool_id, id_type.clone(), id_span.clone());

        if !is_new_insertion
            && let Some(curr_span) = id_span
            && let Some(old_span) = sym.span
        {
            return Err(self.redefinition(curr_span, old_span, pool_id));
        }

        if id_type.var_type != expr_type.var_type
            && let Some(expr_span) = expr_type.reason
            && let Some(reason) = id_type.reason
        {
            let mut diag = self.mismatched_types(
                expr_span,
                expr_type.var_type,
                vec![id_type.var_type],
                expr_pool_id,
                Some(reason.clone())
            );

            diag.add_note(reason, &format!("expected `{}` because of this", id_type.var_type));

            return Err(diag);
        }

        Ok(Attr::Unit(None, None))
    }

    fn stmnt_block_decl(&mut self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 5] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, _),
            Attr::Unit(None, _),
            Attr::Type(TypeId::Var(id_type)),
            Attr::Id(pool_id, id_span),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let (success, sym) = self.symtable_mut().push_local(pool_id, id_type, id_span.clone());

        if !success && let Some(curr_span) = id_span && let Some(old_span) = sym.span {
            return Err(self.redefinition(curr_span, old_span, pool_id));
        }

        Ok(Attr::Unit(None, None))
    }

    fn stmnt_block_if(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 5] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, _),
            Attr::Unit(None, _),
            Attr::Expr(TypeId::Var(expr_type), pool_id),
            Attr::Unit(None, _),
            Attr::Unit(ret_types, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        if expr_type.var_type != Type::Bool && let Some(span) = expr_type.reason {
            return Err(self.mismatched_types(span, expr_type.var_type, vec![Type::Bool], pool_id, None));
        }

        Ok(Attr::Unit(ret_types, None))
    }

    fn stmnt(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Unit(ret_types, _)] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Unit(ret_types, None))
    }

    fn func_arg_stop(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let [] = args[..] else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::FuncArgs(VecDeque::new()))
    }

    fn func_arg_more(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, _),
            Attr::Expr(TypeId::Var(expr_type), pool_id),
            Attr::FuncArgs(mut args),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        args.push_front((expr_type, pool_id));

        Ok(Attr::FuncArgs(args))
    }

    fn func_arg_none(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let [] = args[..] else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::FuncArgs(VecDeque::new()))
    }

    fn func_arg_some(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 2] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Expr(TypeId::Var(expr_type), pool_id),
            Attr::FuncArgs(mut args),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        args.push_front((expr_type, pool_id));

        Ok(Attr::FuncArgs(args))
    }

    fn stmnt_ret(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, span_ret),
            Attr::Expr(TypeId::Var(mut ret_type), _),
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

    fn stmnt_read(&mut self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
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
            let Some(span) = &id_span else {
                return Ok(Attr::Unit(None, None));
            };

            let TypeId::Var(sym_type) = &sym.lang_type else {
                return Err(self.mismatched_types(span.clone(), Type::Func, Self::EXPECTED_IO.into(), Some(pool_id), None));
            };

            if !Self::EXPECTED_IO.contains(&sym_type.var_type) && let Some(read_span) = read_span {
                let mut diag = self.mismatched_types(
                    span.clone(),
                    sym_type.var_type,
                    Self::EXPECTED_IO.into(),
                    Some(pool_id),
                    Some(read_span.clone())
                );

                diag.add_note(read_span, "expected due to this I/O operation");

                return Err(diag);
            }
        } else if id_span.is_some() {
            let _ = self.symtable_mut().push_global(pool_id, TypeVar::new(Type::Int, id_span.clone()), id_span);
        }

        Ok(Attr::Unit(None, None))
    }

    fn stmnt_write(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, write_span),
            Attr::Expr(TypeId::Var(expr_type), pool_id),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        if !Self::EXPECTED_IO.contains(&expr_type.var_type)
            && let Some(span) = expr_type.reason
            && let Some(write_span) = write_span
        {
            let mut diag = self.mismatched_types(span.clone(),
                expr_type.var_type,
                Self::EXPECTED_IO.into(),
                pool_id,
                Some(write_span.clone())
            );

            diag.add_note(write_span, "expected due to this I/O operation");

            return Err(diag);
        }
        
        Ok(Attr::Unit(None, None))
    }

    fn eval_call(
        &self,
        call_start: Option<Span>,
        call_end: Option<Span>,
        id_span: Option<Span>,
        pool_id: usize,
        mut args: VecDeque<(TypeVar, Option<usize>)>,
        expr: bool,
        ) -> Result<Option<Type>, Vec<DiagRef>>
    {
        let arg_span = if let Some(Some(arg_start)) = args.front().map(|arg| &arg.0.reason)
            && let Some(Some(arg_end)) = args.back().map(|arg| &arg.0.reason) {

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
                return Err(vec![self.undefined_func(span, pool_id)]);
            }

            return Ok(None);
        };

        let TypeId::Func(func_type) = &sym.lang_type else {
            if let Some(span) = id_span && let Some(call_span) = call_span {
                let mut diag = self.mismatched_types(
                    span,
                    sym.lang_type.main_type(),
                    vec![Type::Func],
                    Some(pool_id),
                    Some(call_span.clone()),
                );

                diag.add_note(call_span.clone(), "variables can't be called");

                if expr {
                    diag.add_help(DiagHelp::DelCall(call_span));
                }

                return Err(vec![diag]);
            }

            return Ok(None);
        };

        if let Some(arg_first) = args.front_mut() && arg_first.0.var_type == Type::Void {
            arg_first.0.reason = call_span.clone();
        }

        let param_iter = func_type
            .param_type
            .iter()
            .filter(|v| v.var_type != Type::Void)
            .collect::<Vec<&TypeVar>>();

        if param_iter.len() != args.len() {
            let count_span = if args.is_empty() {
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

                diag.add_note(func_span.clone(), "expected due to it's parameter list");

                return Err(vec![diag]);
            }

            return Ok(Some(func_type.ret_type.var_type));
        }

        let mut diags = Vec::new();

        for ((arg_type, arg_pool_id), param_type) in args.into_iter().zip(param_iter.into_iter()) {
            if arg_type.var_type != param_type.var_type
                && let Some(arg_reason) = arg_type.reason
                && let Some(param_reason) = &param_type.reason
            {
                let mut diag = self.mismatched_types(
                    arg_reason,
                    arg_type.var_type,
                    vec![param_type.var_type],
                    arg_pool_id,
                    Some(param_reason.clone()),
                );

                diag.add_note(param_reason.clone(), "expected `{}` due to this parameter type");

                if param_type.var_type == Type::Void && let Some(arg_span) = &arg_span {
                    diag.add_help(DiagHelp::DelArgs(arg_span.clone()));
                }

                diags.push(diag);
            }
        }

        if !diags.is_empty() {
            return Err(diags);
        }

        Ok(Some(func_type.ret_type.var_type))
    }

    fn stmnt_call(&self, args: Vec<Attr>) -> Result<Attr, Vec<DiagRef>> {
        let args: [Attr; 5] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Id(pool_id, id_span),
            Attr::Unit(None, call_start),
            Attr::FuncArgs(args),
            Attr::Unit(None, call_end),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        self.eval_call(call_start, call_end, id_span, pool_id, args, false).map(|_| Attr::Unit(None, None))
    }

    fn stmnt_andassign(&mut self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 4] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Id(pool_id, id_span),
            Attr::Unit(None, andassign_span),
            Attr::Expr(TypeId::Var(expr_type), expr_pool_id),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        if expr_type.var_type != Type::Bool && let Some(span) = expr_type.reason && let Some(andassign_span) = andassign_span {
            let mut diag = self.mismatched_types(
                span,
                expr_type.var_type,
                vec![Type::Bool],
                expr_pool_id,
                Some(andassign_span.clone()),
            );

            diag.add_note(andassign_span, "expected `boolean` due to this logical operator");

            return Err(diag);
        }

        let Some(sym) = self.symtable().search(pool_id) else {
            if let Some(span) = id_span && let Some(andassign_span) = andassign_span {
                let _ = self.symtable_mut().push_global(pool_id, TypeVar::new(Type::Int, Some(span.clone())), Some(span.clone()));

                let mut diag = self.mismatched_types(
                    span,
                    Type::Int,
                    vec![Type::Bool],
                    Some(pool_id),
                    Some(andassign_span.clone()),
                );

                diag.add_note(andassign_span, "expected `boolean` due to this logical operator");

                return Err(diag);
            }

            return Ok(Attr::Unit(None, None));
        };

        let TypeId::Var(var_type) = &sym.lang_type else {
            if let Some(span) = id_span {
                return Err(self.mismatched_types(span, Type::Func, vec![Type::Bool], Some(pool_id), None));
            }

            return Ok(Attr::Unit(None, None));
        };

        if var_type.var_type != Type::Bool && let Some(span) = &var_type.reason && let Some(andassign_span) = andassign_span {
            let mut diag = self.mismatched_types(
                span.clone(),
                var_type.var_type,
                vec![Type::Bool],
                Some(pool_id),
                Some(andassign_span.clone()),
            );

            diag.add_note(andassign_span, "expected `boolean` due to this logical operator");

            return Err(diag);
        }

        Ok(Attr::Unit(None, None))
    }

    fn stmnt_assign(&mut self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 4] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Id(pool_id, id_span),
            Attr::Unit(None, _),
            Attr::Expr(TypeId::Var(expr_type), expr_pool_id),
            Attr::Unit(None, _),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let Some(sym) = self.symtable().search(pool_id) else {
            let _ = self.symtable_mut().push_global(pool_id, TypeVar::new(Type::Int, id_span.clone()), id_span.clone());

            if expr_type.var_type != Type::Int && let Some(expr_span) = expr_type.reason && let Some(id_span) = id_span {
                let mut diag = self.mismatched_types(
                    expr_span,
                    expr_type.var_type,
                    vec![Type::Int],
                    expr_pool_id,
                    Some(id_span.clone()),
                );

                diag.add_note(id_span, "expected `int` due to this implicit declaration");

                return Err(diag);
            }

            return Ok(Attr::Unit(None, None));
        };

        if !Self::EXPECTED_VAR.contains(&expr_type.var_type) {
            if let Some(span) = expr_type.reason {
                return Err(self.mismatched_types(span.clone(), expr_type.var_type, Self::EXPECTED_VAR.into(), expr_pool_id, None));
            }

            return Ok(Attr::Unit(None, None))
        }

        let TypeId::Var(var_type) = &sym.lang_type else {
            if let Some(curr_span) = id_span && let Some(old_span) = &sym.span {
                return Err(self.redefinition(curr_span, old_span.clone(), pool_id));
            }

            return Ok(Attr::Unit(None, None));
        };

        if var_type.var_type != expr_type.var_type
            && let Some(reason) = &var_type.reason
            && let Some(span) = &expr_type.reason
        {
            let mut diag = self.mismatched_types(
                span.clone(),
                expr_type.var_type,
                vec![var_type.var_type],
                expr_pool_id,
                Some(reason.clone())
            );

            let msg = if sym.implicit {
                "expected `int` due to this implicit declaration".into()
            } else {
                format!("expected `{}` due to it's declaration", var_type.var_type)
            };

            diag.add_note(reason.clone(), &msg);

            return Err(diag);
        }

        Ok(Attr::Unit(None, None))
    }

    fn expr_id(&mut self, args: Vec<Attr>) -> Result<Option<Attr>, DiagRef> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Id(pool_id, id_span)] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let Some(sym) = self.symtable().search(pool_id) else {
            if let Some(span) = id_span {
                let _ = self.symtable_mut().push_global(pool_id, TypeVar::new(Type::Int, Some(span.clone())), Some(span.clone()));
                return Ok(Some(Attr::Expr(TypeId::new_var(Type::Int, Some(span)), Some(pool_id))));
            }

            return Ok(Some(Attr::Expr(TypeId::new_var(Type::Int, None), Some(pool_id))));
        };

        let TypeId::Var(var_type) = &sym.lang_type else {
            if let Some(span) = &id_span {
                let mut diag = self.mismatched_types(span.clone(), Type::Func, Self::EXPECTED_VAR.into(), Some(pool_id), None);

                diag.add_help(DiagHelp::InsCall(span.clone()));

                return Err(diag);
            }

            return Ok(None);
        };

        Ok(Some(Attr::Expr(TypeId::new_var(var_type.var_type, id_span), Some(pool_id))))
    }

    fn expr_map(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 1] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [Attr::Expr(TypeId::Var(var_type), pool_id)] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        Ok(Attr::Expr(TypeId::Var(var_type), pool_id))
    }


    fn expr_wrap(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, start_span),
            Attr::Expr(TypeId::Var(expr_type), pool_id),
            Attr::Unit(None, end_span),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let span = if let Some(start_span) = start_span && let Some(end_span) = end_span {
            Some(Span::new(start_span.start, end_span.end))
        } else {
            None
        };

        Ok(Attr::Expr(TypeId::new_var(expr_type.var_type, span), pool_id))
    }

    fn expr_call(&self, args: Vec<Attr>) -> Result<Option<Attr>, Vec<DiagRef>> {
        let args: [Attr; 4] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Id(pool_id, id_span),
            Attr::Unit(None, call_start),
            Attr::FuncArgs(args),
            Attr::Unit(None, call_end),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        let full_span = if let Some(id_span) = &id_span && let Some(call_end) = &call_end {
            Some(Span::new(id_span.start, call_end.end))
        } else {
            None
        };

        self.eval_call(call_start, call_end, id_span, pool_id, args, true).map(|func_info| {
            func_info.map(|ret_type| Attr::Expr(TypeId::new_var(ret_type, full_span), Some(pool_id)))
        })
    }

    fn expr_oper_bin(&self, args: Vec<Attr>, valid: Vec<Type>, msg: &str, out_type: Option<Type>) -> Result<Attr, DiagRef> {
        let args: [Attr; 3] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Expr(TypeId::Var(expr_type1), pool_id1),
            Attr::Unit(None, oper_span),
            Attr::Expr(TypeId::Var(expr_type2), pool_id2),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        if let Some(oper_span) = oper_span {
            if !valid.contains(&expr_type1.var_type) && let Some(span) = expr_type1.reason {
                let mut diag = self.mismatched_types(span, expr_type1.var_type, valid, pool_id1, Some(oper_span.clone()));

                diag.add_note(oper_span, msg);

                return Err(diag);
            }

            if !valid.contains(&expr_type2.var_type) && let Some(span) = expr_type2.reason{
                let mut diag = self.mismatched_types(span, expr_type2.var_type, valid, pool_id2, Some(oper_span.clone()));

                diag.add_note(oper_span, msg);

                return Err(diag);
            }
        }

        if expr_type1.var_type != expr_type2.var_type
            && let Some(span1) = &expr_type1.reason
            && let Some(span2) = expr_type2.reason
        {
            let mut diag = self.mismatched_types(span2, expr_type2.var_type, vec![expr_type1.var_type], pool_id2, Some(span1.clone()));

            diag.add_note(span1.clone(), &format!("because this has type `{}`", expr_type1.var_type));

            return Err(diag);
        }

        let span = if let Some(expr1_span) = expr_type1.reason && let Some(expr2_span) = expr_type2.reason {
            Some(Span::new(expr1_span.start, expr2_span.end))
        } else {
            None
        };

        Ok(Attr::Expr(TypeId::new_var(out_type.unwrap_or(expr_type1.var_type), span), None))
    }

    fn expr_oper_bin_num(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        self.expr_oper_bin(args, Self::EXPECTED_NUM.into(), "expected due to this arithmetic operator", None)
    }

    fn expr_oper_bin_cmp(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        self.expr_oper_bin(args, Self::EXPECTED_NUM.into(), "expected due to this relational operator", Some(Type::Bool))
    }

    fn expr_oper_bin_bool(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        self.expr_oper_bin(args, vec![Type::Bool],"expected `boolean` due to this logical operator", Some(Type::Bool))
    }

    fn expr_oper_unr(&self, args: Vec<Attr>, valid: Vec<Type>, msg: &str) -> Result<Attr, DiagRef> {
        let args: [Attr; 2] = args.try_into().unwrap_or_else(|args| {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        });

        let [
            Attr::Unit(None, oper_span),
            Attr::Expr(TypeId::Var(expr_type), pool_id),
        ] = args else {
            unreachable!("invalid args in {:?} rule: {:?}", self.rule, args);
        };

        if !valid.contains(&expr_type.var_type)
            && let Some(oper_span) = &oper_span
            && let Some(span) = expr_type.reason
        {
            let mut diag = self.mismatched_types(span, expr_type.var_type, valid, pool_id, Some(oper_span.clone()));

            diag.add_note(oper_span.clone(), msg);

            return Err(diag);
        }

        let span = if let Some(oper_span) = oper_span && let Some(expr_span) = expr_type.reason {
            Some(Span::new(oper_span.start, expr_span.end))
        } else {
            None
        };

        Ok(Attr::Expr(TypeId::new_var(expr_type.var_type, span), None))

    }

    fn expr_oper_unr_bool(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        self.expr_oper_unr(args, vec![Type::Bool], "expected `boolean` due to this logical operator")
    }

    fn expr_oper_unr_num(&self, args: Vec<Attr>) -> Result<Attr, DiagRef> {
        self.expr_oper_unr(args, Self::EXPECTED_NUM.into(), "expected due to this arithmetic operator")
    }
}
