use crate::diag::DiagSever;
use crate::target::Target;
use crate::diag::Diag;

use super::format::ReporterFmt;


pub struct Reporter<'t> {
    errs: usize,
    warns: usize,
    quiet: bool,
    target: &'t Target,
}

impl<'t> Reporter<'t> {
    pub fn new(target: &'t Target, quiet: bool) -> Self {
        Self {
            errs: 0,
            warns: 0,
            quiet,
            target,
        }
    }

    pub fn emit(&mut self, diag: Diag) {
        if diag.kind.sever() == DiagSever::Error {
            self.errs += 1;
        } else {
            self.warns += 1;
        }

        if !self.quiet {
            ReporterFmt::dump_diag(self.target, &diag);
        }
    }

    pub fn found_err(&self) -> bool {
        self.errs > 0
    }

    pub fn finnish(&self) {
        ReporterFmt::dump_warns(self.target, self.warns);

        if self.found_err() {
            ReporterFmt::dump_failure(self.target, self.errs);
        } else {
            ReporterFmt::dump_success(self.target);
        }
    }
}
