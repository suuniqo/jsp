use std::{cell::RefCell, rc::Rc};

use crate::{diag::{Diag, DiagLevel, DiagSever}, pool::StrPool, target::Target};

mod format;

use format::ReporterFmt;


pub struct Reporter<'t> {
    errs: usize,
    warns: usize,
    quiet: bool,
    target: &'t Target,
    pool: Rc<RefCell<StrPool>>,
    level: DiagLevel,
}

impl<'t> Reporter<'t> {
    pub fn new(target: &'t Target, pool: Rc<RefCell<StrPool>>, quiet: bool) -> Self {
        Self {
            errs: 0,
            warns: 0,
            quiet,
            target,
            pool,
            level: DiagLevel::None,
        }
    }

    pub fn emit(&mut self, diag: Diag) {
        if diag.kind.sever() == DiagSever::Error {
            self.errs += 1;
        } else {
            self.warns += 1;
        }

        self.level = self.level.min(diag.kind.level());

        if !self.quiet {
            ReporterFmt::dump_diag(self.target, &self.pool.borrow(), &diag);
        }
    }

    pub fn finish(&self) -> DiagLevel {
        ReporterFmt::dump_warns(self.target, self.warns);

        if self.level != DiagLevel::None {
            ReporterFmt::dump_failure(self.target, self.errs);
        } else {
            ReporterFmt::dump_success(self.target);
        }

        self.level
    }
}
