use crate::diag::DiagSever;
use crate::target::Target;
use crate::diag::Diag;

use super::format::ReporterFmt;


pub struct Reporter<'t> {
    errs: usize,
    warns: usize,
    trg: &'t Target,
    diags: Vec<Diag>,
}

impl<'t> Reporter<'t> {
    pub fn new(target: &'t Target) -> Self {
        Self {
            errs: 0,
            warns: 0,
            trg: target,
            diags: Vec::new(),
        }
    }

    pub fn push(&mut self, diag: Diag) {
        if diag.kind.sever() == DiagSever::Error {
            self.errs += 1;
        } else {
            self.warns += 1;
        }

        self.diags.push(diag);
    }

    pub fn found_err(&self) -> bool {
        self.errs > 0
    }

    pub fn dump(&self) {
        for diag in self.diags.iter() {
            ReporterFmt::dump_diag(self.trg, diag);
        }

        ReporterFmt::dump_warns(self.trg, self.warns);

        if self.found_err() {
            ReporterFmt::dump_failure(self.trg, self.errs);
        } else {
            ReporterFmt::dump_success(self.trg);
        }
    }
}
