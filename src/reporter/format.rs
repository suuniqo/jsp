use std::collections::BTreeMap;

use crate::{color::Color, diag::{Diag, DiagKind, DiagSever, DiagSpan, HelpAction}, span::Span, target::Target};

pub struct ReporterFmt;

impl ReporterFmt {
    pub fn dump_failure(trg: &Target, errs: usize) {
        eprintln!("{}{}: {}error: {}couldn't process file due to {} previous error{}",
            Color::HIGHLIGHT, trg.path(),
            DiagSever::Error.color(), Color::RESET,
            errs, if errs > 1 { "s" } else { "" },
        );
    }

    pub fn dump_warns(trg: &Target, warns: usize) {
        if warns > 0 {
            eprintln!("{}{}: {}warning: {}file generated {} warning{}",
                Color::HIGHLIGHT, trg.path(),
                DiagSever::Warning.color(), Color::RESET,
                warns, if warns == 1 { "" } else { "s" },
            );
            eprintln!()
        }
    }

    pub fn dump_success(trg: &Target) {
        eprintln!("{}{}: {}info: {}file successfully processed",
            Color::HIGHLIGHT, trg.path(),
            DiagSever::Note.color(), Color::RESET,
        );
    }

    pub fn dump_diag(trg: &Target, diag: &Diag) {
        let mut span_map = diag.spans
            .iter()
            .map(|diag_span| {
                let (row, _) = trg.coord_from_span(&diag_span.span)
                    .expect("invalid span when fetching diag coords");

                (row, diag_span)
            })
            .fold(BTreeMap::new(), |mut map, (row, diag_span)| {
                let spans: &mut Vec<&DiagSpan> = map.entry(row).or_default();

                let pos = spans
                    .binary_search_by(|probe| probe.span.cmp(&diag_span.span))
                    .unwrap_or_else(|e| e);

                spans.insert(pos, diag_span);

                map
            });

        Self::normalize_map(&diag.kind, &mut span_map);

        let span = diag.main_span();

        let (row, col) = trg.coord_from_span(&span)
            .expect("invalid span when fetching diag coords");

        eprintln!("{}{}:{}:{}: {}{}: {}{}",
            Color::HIGHLIGHT, trg.path(), row + 1, col + 1,
            diag.kind.sever().color(), diag.kind.sever(), Color::RESET, diag.kind
        );

        let max_padding = " ".repeat(ReporterFmt::max_padding(trg));

        let mut last_row = None;

        for (row, spans) in span_map {
            if last_row.is_some_and(|last_row| last_row + 1 != row) {
                eprintln!("{} {}:", max_padding, Color::HIGHLIGHT)
            }

            let (line, offset) = trg.nth_line(row)
                .expect("invalid row when fetching line");

            let spans_len = spans.len();

            let comments = spans
                .iter()
                .enumerate()
                .filter(|(i, span)| span.msg.is_some() && i + 1 != spans_len)
                .count();

            let underline_cols = 1 + comments + (comments != 0) as usize;

            let mut formatted = String::new();
            let mut underline = vec![String::new(); underline_cols];

            let mut pos = 0;
            let mut underline_col = underline_cols - 1;

            let row_padding = " ".repeat(ReporterFmt::row_padding(row, trg));

            for (i, diag_span) in spans.into_iter().enumerate() {
                let span = &diag_span.span;

                let rel_span = Span::new(span.start - offset, span.end - offset);

                if pos < span.start {
                    formatted.push_str(&line[pos..rel_span.start]);

                    for j in 0..=underline_col {
                        underline[j].push_str(&" ".repeat(rel_span.start - pos));
                    }
                }

                formatted.push_str(&diag_span.highlight(line, offset));
                underline[0].push_str(&diag_span.underline());

                pos = rel_span.end;

                let context_len = ReporterFmt::human_redable(
                    &line[rel_span.start..rel_span.end]
                ).len();

                if let Some(msg) = &diag_span.msg {
                    for j in 1..underline_col {
                        underline[j].push_str(&format!(
                            "{}|{}",
                            diag_span.sever.color(),
                            " ".repeat(context_len - 1)
                        ));
                    }

                    underline[underline_col].push_str(&format!(
                        "{}{}{}",
                        if underline_col == 0 { " " } else { "" },
                        diag_span.sever.color(),
                        msg,
                    ));

                    underline_col = if underline_col > 2 {
                        underline_col - 1
                    } else {
                        0
                    };
                } else {
                    for j in 0..=underline_col {
                        underline[j].push_str(&" ".repeat(context_len));
                    }
                }

                if i + 1 == spans_len {
                    formatted.push_str(&line[rel_span.end..]);
                }
            }

            last_row = Some(row);

            eprintln!("{}{} |{}", Color::HIGHLIGHT, max_padding, Color::RESET);

            eprintln!(
                "{}{}{} | {}{}",
                row_padding,
                Color::HIGHLIGHT,
                row + 1,
                Color::RESET,
                formatted,
            );

            for line in underline {
                eprintln!("{}{} | {}", max_padding, Color::HIGHLIGHT, line);
            }
        }

        if let Some(help) = &diag.help {
            let action = help.action();
            let span = action.span();

            let (row, _) = trg.coord_from_span(&span)
                .expect("invalid span when fetching diag coords");

            let (line, offset) = trg.nth_line(row)
                .expect("invalid row when fetching line");

            let lspan = Span::new(span.start - offset, span.end - offset);

            let prefix = &line[..lspan.start];
            let context = ReporterFmt::human_redable(&line[lspan.start..lspan.end]);
            let suffix = &line[lspan.end..];

            let prefix_padding = " ".repeat(prefix.chars().count());
            let context_len = context.chars().count();

            let row_padding = " ".repeat(ReporterFmt::row_padding(row, trg));

            eprintln!("{}{} |{}", Color::HIGHLIGHT, max_padding, Color::RESET);

            eprintln!("{}{}--> {}help: {}{}",
                max_padding, Color::HIGHLIGHT,
                Color::BLUE, Color::RESET, help,
            );

            eprintln!("{}{} |{}", Color::HIGHLIGHT, max_padding, Color::RESET);

            match action {
                HelpAction::Insert(_, before, insert) => {
                    let underline = insert
                        .chars()
                        .map(|c| if c.is_whitespace() { c } else { '+' })
                        .collect::<String>();

                    if before {
                        eprintln!("{}{}{} | {}{}{}{}{}{}{}",
                            Color::HIGHLIGHT, row_padding, row + 1, Color::RESET, prefix,
                            Color::BLUE, insert, Color::RESET, context, suffix
                        );

                        eprintln!("{}{} | {}{}{}{}",
                            Color::HIGHLIGHT, max_padding, prefix_padding,
                            Color::BLUE, underline, Color::RESET,
                        );
                    } else {
                        let prefix_padding = format!("{}{}", prefix_padding, " ".repeat(context_len));

                        eprintln!("{}{}{} | {}{}{}{}{}{}{}",
                            Color::HIGHLIGHT, row_padding, row + 1, Color::RESET, prefix,
                            context, Color::BLUE, insert, Color::RESET, suffix
                        );

                        eprintln!("{}{} | {}{}{}{}",
                            Color::HIGHLIGHT, max_padding, prefix_padding,
                            Color::BLUE, underline, Color::RESET
                        );
                    }
                },
                HelpAction::Delete(_) => {
                    eprintln!("{}{}{} {}- {}{}{}{}{}{}",
                        Color::HIGHLIGHT, row_padding, row + 1, Color::RED,
                        Color::RESET, prefix, Color::RED, context, Color::RESET, suffix
                    );
                    eprintln!("{}{}{} {}+ {}{}{}",
                        Color::HIGHLIGHT, row_padding, row + 1, Color::BLUE,
                        Color::RESET, prefix, suffix.trim_start()
                    );
                    eprintln!("{}{} |{}", Color::HIGHLIGHT, max_padding, Color::RESET);
                },
                HelpAction::Replace(_, replace) => {
                    eprintln!("{}{}{} {}- {}{}{}{}{}{}",
                        Color::HIGHLIGHT, row_padding, row + 1, Color::RED,
                        Color::RESET, prefix, Color::RED, context, Color::RESET, suffix
                    );
                    eprintln!("{}{}{} {}+ {}{}{}{}{}{}",
                        Color::HIGHLIGHT, row_padding, row + 1, Color::BLUE,
                        Color::RESET, prefix, Color::BLUE, replace, Color::RESET, suffix
                    );
                    eprintln!("{}{} |{}", Color::HIGHLIGHT, max_padding, Color::RESET);
                },
            }
        }

        eprintln!();
    }

    fn normalize_map(kind: &DiagKind, span_map: &mut BTreeMap<usize, Vec<&DiagSpan>>) {
        if !matches!(kind, DiagKind::UnexpectedTok(..)) {
            return;
        }

        for vec in span_map.values_mut() {
            if vec.iter().filter(|diag_span| diag_span.sever != DiagSever::Note).count() !=  0 {
                vec.retain(|diag_span| diag_span.sever != DiagSever::Note);
            }
        }
    }

    fn max_padding(trg: &Target) -> usize {
        Self::digits(trg.lines()) + 1
    }

    fn row_padding(row: usize, trg: &Target) -> usize {
        Self::digits(trg.lines()) - Self::digits(row + 1) + 1
    }

    fn human_redable(slice: &str) -> String {
        let mut readable = String::new();

        for c in slice.chars() {
            if c.is_control() {
                readable.push_str(&c.escape_default().to_string());
            } else {
                readable.push(c);
            }
        }

        readable
    }

    fn digits(num: usize) -> usize {
        (num as f64).log10() as usize + 1
    }
}

impl DiagSpan {
    fn underline(&self) -> String {
        let sym = match self.sever {
            DiagSever::Note => "-",
            DiagSever::Error | DiagSever::Warning => "^",
        };

        format!(
            "{}{}{}",
            self.sever.color(),
            sym.repeat(self.span.len()),
            Color::RESET
        )
    }

    fn highlight(&self, line: &str, offset: usize) -> String {
        let highlight = if self.highlight {
            self.sever.color()
        } else {
            ""
        };

        format!(
            "{}{}{}",
            highlight,
            ReporterFmt::human_redable(&line[self.span.start-offset..self.span.end-offset]),
            Color::RESET
        )
    }
}
