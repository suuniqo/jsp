use std::{cell::RefCell, rc::Rc};

use crate::{diag::{DiagRef, DiagLevel, DiagSever}, pool::PoolLookup, target::Target};

mod format;

use format::ReporterFmt;


pub struct Reporter<'t, Pool: PoolLookup> {
    errs: usize,
    warns: usize,
    quiet: bool,
    target: &'t Target,
    pool: Rc<RefCell<Pool>>,
    level: Option<DiagLevel>,
}

impl<'t, Pool: PoolLookup> Reporter<'t, Pool> {
    pub fn new(target: &'t Target, pool: Rc<RefCell<Pool>>, quiet: bool) -> Self {
        Self {
            errs: 0,
            warns: 0,
            quiet,
            target,
            pool,
            level: None,
        }
    }

    pub fn emit(&mut self, diag: DiagRef) {
        if diag.kind.sever() == DiagSever::Error {
            self.errs += 1;
        } else {
            self.warns += 1;
        }

        if let Some(next) = diag.kind.level() {
            self.level = Some(self.level.map_or(next, |curr| curr.min(next)));
        }

        if !self.quiet {
            ReporterFmt::dump_diag(self.target, &*self.pool.borrow(), &diag);
        }
    }

    pub fn finish(&self) -> Option<DiagLevel> {
        if self.quiet {
            return self.level;
        }

        ReporterFmt::dump_warns(self.target, self.warns);

        if self.level.is_some() {
            ReporterFmt::dump_failure(self.target, self.errs);
        } else {
            ReporterFmt::dump_success(self.target);
        }

        self.level
    }
}
