use crate::span::Span;
use crate::target::Target;

use crate::{diag::Diag, color::Color};


pub struct Reporter<'t> {
    trg: &'t Target,
    diags: Vec<Diag>,
    max_span: Span,
}

impl<'t> Reporter<'t> {
    pub fn new(target: &'t Target) -> Self {
        Self {
            trg: target,
            diags: Vec::new(),
            max_span: Span::new(0, 0),
        }
    }

    pub fn push(&mut self, diag: Diag) {
        let span = &diag.span;

        if span.end > self.max_span.end {
            self.max_span = span.clone();
        }

        self.diags.push(diag);
    }
    
    fn padding_len(&self) -> usize {
        let (row, _) = self.trg.coord_from_span(&self.max_span)
            .expect("invalid span when fetching padding len");

        let digits = Self::digits(row + 1);

        digits - (digits % 4) + 4
    }

    fn digits(row: usize) -> usize {
        (row as f64).log10() as usize + 1
    }
}

impl Drop for Reporter<'_> {
    fn drop(&mut self) {
        let padding_len = self.padding_len();
        let padding_row = " ".repeat(padding_len);

        for diag in self.diags.iter() {
            let (row, col) = self.trg.coord_from_span(&diag.span)
                .expect("invalid span when fetching diag coords");

            let (line, offset) = self.trg.nth_line(row)
                .expect("invalid row when fetching line");

            let lspan = Span::new(diag.span.start - offset, diag.span.end - offset);

            eprintln!("{}{}:{}:{}: {}{}: {}{}",
                Color::HIGHLIGHT, self.trg.path(), row + 1, col + 1,
                diag.kind.sever().color(), diag.kind.sever(), Color::RESET, diag.kind
            );

            let prefix = &line[..lspan.start];
            let context = &line[lspan.start..lspan.end];
            let suffix = &line[lspan.end..];

            let prefix_len = prefix.chars().count();
            let context_len = context.chars().count();

            eprintln!("{}{} |", Color::HIGHLIGHT, padding_row);

            eprintln!("{}{}{} | {}{}{}{}{}{}",
                Color::HIGHLIGHT, &padding_row[Self::digits(row + 1)..], row + 1, Color::RESET, prefix,
                diag.kind.sever().color(), context, Color::RESET, suffix
            );

            let prefix_padding = " ".repeat(prefix_len);
            let underline = "^".repeat(context_len);

            eprintln!("{}{} | {}{}{}{}",
                Color::HIGHLIGHT, padding_row, prefix_padding,
                diag.kind.sever().color(), underline, Color::RESET
            );
        }
    }
}
