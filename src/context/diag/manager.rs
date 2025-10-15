use crate::target::Target;

use super::diag::Diag;
use super::color::DiagColor;


pub struct DiagManager<'t> {
    target: &'t Target,
    diags: Vec<Diag>,
    max_row_len: usize,
}

impl<'t> DiagManager<'t> {
    pub fn new(target: &'t Target) -> Self {
        Self {
            target,
            diags: Vec::new(),
            max_row_len: 1,
        }
    }

    pub fn push(&mut self, diag: Diag) {
        let row_len = Self::row_len(diag.row);

        if row_len > self.max_row_len {
            self.max_row_len = row_len;
        }

        self.diags.push(diag);
    }
    
    fn row_len(row: usize) -> usize {
        (row as f64).log10() as usize + 1
    }
}

impl Drop for DiagManager<'_> {
    fn drop(&mut self) {
        let padding_len = self.max_row_len - (self.max_row_len % 4) + 4;

        for diag in self.diags.iter() {
            let padding_row = " ".repeat(padding_len);
            let padding_ctx = " ".repeat(diag.col - 1);

            let len = diag.kind.payload_len();
            let underline = "~".repeat(if len > 0 { len - 1 } else { 0 });

            let context = self.target.nth_line(diag.row - 1)
                .unwrap_or_else(|| unreachable!("diagnostic <{}> with invalid line {}", diag.kind, diag.row));

            eprintln!("{}{}:{}:{}: {}{}: {}{}",
                DiagColor::HIGHLIGHT, self.target.path(), diag.row, diag.col,
                diag.kind.sever().color(), diag.kind.sever(), DiagColor::RESET, diag.kind
            );

            eprintln!("{}{} | {}{}{}{}{}",
                &padding_row[DiagManager::row_len(diag.row)..], diag.row, &context[..padding_ctx.len()],
                diag.kind.sever().color(), &context[padding_ctx.len()..padding_ctx.len()+underline.len()+1], DiagColor::RESET,
                &context[padding_ctx.len()+underline.len()+1..]
            );

            eprintln!("{} | {}{}^{}{}",
                padding_row, padding_ctx,
                diag.kind.sever().color(), underline, DiagColor::RESET
            );
        }
    }
}
