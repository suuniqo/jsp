use std::{collections::BTreeMap, fmt};

use crate::{style::Style, diag::{Diag, DiagHelp, DiagKind, DiagSever, DiagSpan, HelpAction}, grammar::MetaSym, span::Span, target::Target};

pub struct ReporterFmt;

impl ReporterFmt {
    pub fn dump_failure(trg: &Target, errs: usize) {
        eprintln!("{}{}: {}error: {}couldn't process file due to {} previous error{}",
            Style::High, trg.path(),
            DiagSever::Error.color(), Style::Reset,
            errs, if errs > 1 { "s" } else { "" },
        );
    }

    pub fn dump_warns(trg: &Target, warns: usize) {
        if warns > 0 {
            eprintln!("{}{}: {}warning: {}file generated {} warning{}",
                Style::High, trg.path(),
                DiagSever::Warning.color(), Style::Reset,
                warns, if warns == 1 { "" } else { "s" },
            );
            eprintln!()
        }
    }

    pub fn dump_success(trg: &Target) {
        eprintln!("{}{}: {}info: {}file successfully processed",
            Style::High, trg.path(),
            DiagSever::Note.color(), Style::Reset,
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
            Style::High, trg.path(), row + 1, col + 1,
            diag.kind.sever().color(), diag.kind.sever(), Style::Reset, diag.kind
        );

        let max_padding = " ".repeat(ReporterFmt::max_padding(trg));

        let mut last_row = None;

        for (row, spans) in span_map {
            if last_row.is_some_and(|last_row| last_row + 1 != row) {
                eprintln!("{} {}:", max_padding, Style::High)
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
                    let frag = &line[pos..rel_span.start];

                    formatted.push_str(frag);

                    for j in 0..=underline_col {
                        underline[j].push_str(&" ".repeat(frag.chars().count()));
                    }
                }

                formatted.push_str(&diag_span.highlight(line, offset));
                underline[0].push_str(&diag_span.underline(trg));

                pos = rel_span.end;

                let context_len = ReporterFmt::human_redable(
                    &line[rel_span.start..rel_span.end]
                ).chars().count();

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

            eprintln!("{}{} |", Style::High, max_padding);

            eprintln!(
                "{}{}{} | {}{}",
                row_padding,
                Style::High,
                row + 1,
                Style::Reset,
                formatted,
            );

            for line in underline {
                eprintln!("{}{} | {}", max_padding, Style::High, line);
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

            eprintln!("{}{} |{}", Style::High, max_padding, Style::Reset);

            eprintln!("{}{}--> {}help: {}{}",
                max_padding, Style::High,
                Style::Blue, Style::Reset, Fmt(help),
            );

            eprintln!("{}{} |", Style::High, max_padding);

            match action {
                HelpAction::Insert(_, before, insert) => {
                    let underline = insert
                        .chars()
                        .map(|c| if c.is_whitespace() { c } else { '+' })
                        .collect::<String>();

                    if before {
                        eprintln!("{}{}{} | {}{}{}{}{}{}{}",
                            Style::High, row_padding, row + 1, Style::Reset, prefix,
                            Style::Blue, insert, Style::Reset, context, suffix
                        );

                        eprintln!("{}{} | {}{}{}{}",
                            Style::High, max_padding, prefix_padding,
                            Style::Blue, underline, Style::Reset,
                        );
                    } else {
                        let prefix_padding = format!("{}{}", prefix_padding, " ".repeat(context_len));

                        eprintln!("{}{}{} | {}{}{}{}{}{}{}",
                            Style::High, row_padding, row + 1, Style::Reset, prefix,
                            context, Style::Blue, insert, Style::Reset, suffix
                        );

                        eprintln!("{}{} | {}{}{}{}",
                            Style::High, max_padding, prefix_padding,
                            Style::Blue, underline, Style::Reset
                        );
                    }
                },
                HelpAction::Delete(_) => {
                    eprintln!("{}{}{} {}- {}{}{}{}{}{}",
                        Style::High, row_padding, row + 1, Style::Red,
                        Style::Reset, prefix, Style::Red, context, Style::Reset, suffix
                    );
                    eprintln!("{}{}{} {}+ {}{}{}",
                        Style::High, row_padding, row + 1, Style::Blue,
                        Style::Reset, prefix, suffix.trim_start()
                    );
                    eprintln!("{}{} |{}", Style::High, max_padding, Style::Reset);
                },
                HelpAction::Replace(_, replace) => {
                    eprintln!("{}{}{} {}- {}{}{}{}{}{}",
                        Style::High, row_padding, row + 1, Style::Red,
                        Style::Reset, prefix, Style::Red, context, Style::Reset, suffix
                    );
                    eprintln!("{}{}{} {}+ {}{}{}{}{}{}",
                        Style::High, row_padding, row + 1, Style::Blue,
                        Style::Reset, prefix, Style::Blue, replace, Style::Reset, suffix
                    );
                    eprintln!("{}{} |{}", Style::High, max_padding, Style::Reset);
                },
            }
        }

        eprintln!();
    }

    fn normalize_map(kind: &DiagKind, span_map: &mut BTreeMap<usize, Vec<&DiagSpan>>) {
        if !matches!(kind, DiagKind::UnexpectedTok(..) | DiagKind::MissingSemi) {
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
    fn underline(&self, trg: &Target) -> String {
        let sym = match self.sever {
            DiagSever::Note => "-",
            DiagSever::Error | DiagSever::Warning => "^",
        };

        let len = if let Some(frag) = trg.slice_from_span(&self.span) {
            frag.chars().count()
        } else {
            self.span.len()
        }.max(1);

        format!(
            "{}{}{}",
            self.sever.color(),
            sym.repeat(len),
            Style::Reset
        )
    }

    fn highlight(&self, line: &str, offset: usize) -> String {
        let highlight = if self.highlight {
            self.sever.color()
        } else {
            Style::None
        };

        format!(
            "{}{}{}",
            highlight,
            ReporterFmt::human_redable(&line[self.span.start-offset..self.span.end-offset]),
            Style::Reset
        )
    }
}

struct Fmt<'a, T>(&'a T);

impl<'a> fmt::Display for Fmt<'a, MetaSym> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            MetaSym::Stmnt => write!(f, "{}a {}statement{}",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::FuncBlock => write!(f, "{}a {}function{}",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::Expr => write!(f, "{}an {}expression{}",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::Type => write!(f, "{}a {}type{}",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::Id => write!(f, "{}an {}identifier{}",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::Assign => write!(f, "{}an {}assignment{}",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::Comma => write!(f, "{}`{},{}`",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::Semi => write!(f, "{}`{};{}`",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::LParen => write!(f, "{}`{}({}`",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::RParen => write!(f, "{}`{}){}`",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::LBrack => write!(f, "{}`{}{{{}`",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::RBrack => write!(f, "{}`{}}}{}`",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::OperBinary => write!(f, "{}a {}binary operator{}",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::OperUnary => write!(f, "{}an {}unary operator{}",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::FuncArgs => write!(f, "{}an {}argument list{}",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::FuncParams => write!(f, "{}a {}parameter list{}",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::FuncType => write!(f, "{}a {}return type{}",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::FuncId => write!(f, "{}a {}function name{}",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::FuncParam => write!(f, "{}a {}parameter{}",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::Func => write!(f, "{}`{}function{}`",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::While => write!(f, "{}`{}while{}`",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::If => write!(f, "{}`{}if{}`",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::Do => write!(f, "{}`{}do{}`",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::Let => write!(f, "{}`{}let{}`",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::Read => write!(f, "{}`{}read{}`",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::Write => write!(f, "{}`{}write{}`",
                Style::Reset, Style::High, Style::Reset
            ),
            MetaSym::Ret => write!(f, "{}`{}return{}`",
                Style::Reset, Style::High, Style::Reset
            ),
        }
    }
}

impl<'a> fmt::Display for Fmt<'a, DiagHelp> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            DiagHelp::InsDecimal(..) => write!(f, "{}add a decimal part", Style::Reset),
            DiagHelp::InsToken(found, before, insertion) => {
                if insertion.is_empty() {
                    unreachable!("insertion can't be empty");
                } else {
                    write!(f, "insert ")?;

                    for (i, sym) in insertion.iter().enumerate() {
                        write!(f, "{}", Fmt(sym))?;

                        if i + 2 < insertion.len() {
                            write!(f, ", ")?;
                        } else if i + 2 == insertion.len() {
                            write!(f, " and ")?;
                        }
                    }

                    write!(f, " {} `{}{}{}`",
                        if *before { "before" } else { "after" },
                        Style::High, found.kind.lexeme(), Style::Reset,
                    )
                }
            },
            DiagHelp::DelToken(found) => write!(
                f,
                "{}remove the unnecessary `{}{}{}`",
                Style::Reset, Style::High, found.kind.lexeme(), Style::Reset
            ),
            DiagHelp::RepToken(found, rep) => write!(
                f,
                "{}replace `{}{}{}` by {}",
                Style::Reset, Style::High, found.kind.lexeme(), Style::Reset, Fmt(rep),
            ),
            DiagHelp::RepKw(_) => write!(f, "{}change the name to use it as an identifier", Style::Reset),
            DiagHelp::DelTrailingComma(_) => write!(f, "{}remove the trailing comma", Style::Reset),
            DiagHelp::InsVarType(_) => write!(f, "{}add the missing type", Style::Reset),
            DiagHelp::InsRetType(_) => write!(f, "{}add a {}return type", Style::Reset, Style::High),
            DiagHelp::InsParamList(..) => write!(f, "{}add a {}parameter list", Style::Reset, Style::High),
            DiagHelp::InsParam(_) => write!(f, "{}perhaps you meant to have no parameters", Style::Reset),
        }
    }
}

impl fmt::Display for DiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagKind::StrayChar(c) => write!(f, "{}illegal character `{}{}{}` in program",
                Style::Reset, Style::High,
                if c.is_control() {
                    c.escape_default().to_string()
                } else {
                    c.to_string()
                },
                Style::Reset
            ),
            DiagKind::UntermComm => write!(f, "{}unterminated block comment", Style::Reset),
            DiagKind::UntermStr(_) => write!(f, "{}missing terminating character `{}\"{}` on string literal",
                Style::Reset, Style::High, Style::Reset
            ),
            DiagKind::OverflowStr(_) => write!(f, "{}string literal is too long", Style::Reset),
            DiagKind::InvEscSeq(c) => write!(f, "{}unknown escape sequence `{}\\{c}{}`",
                Style::Reset, Style::High, Style::Reset
            ),
            DiagKind::OverflowInt => write!(f, "{}integer literal out of range for 16-byte type", Style::Reset),
            DiagKind::OverflowFloat => write!(f, "{}float literal out of range for 32-byte type", Style::Reset),
            DiagKind::InvFmtFloat(_) => write!(f, "{}missing decimal part after `{}.{}` in float literal",
                Style::Reset, Style::High, Style::Reset
            ),
            DiagKind::MalformedStr(c, _) => write!(f, "{}malformed string literal, contains control character `{}{}{}`",
                Style::Reset, Style::High, c.escape_default(), Style::Reset
            ),
            DiagKind::UnexpectedTok(found, expected) => {
                if expected.is_empty() {
                    return write!(f, "unexpected `{}{}{}`", Style::High, found.lexeme(), Style::Reset);
                }

                write!(f, "expected ")?;

                for (i, sym) in expected.iter().enumerate() {
                    write!(f, "{}", Fmt(sym))?;

                    if i + 2 < expected.len() {
                        write!(f, ", ")?;
                    } else if i + 2 == expected.len() {
                        write!(f, " or ")?;
                    }
                }

                write!(f, ", found `{}{}{}`", Style::High, found.lexeme(), Style::Reset)
            },
            DiagKind::MismatchedDelim(kind) => write!(f, "{}mismatched closing delimiter `{}{}{}`",
                Style::Reset, Style::High, kind.lexeme(), Style::Reset
            ),
            DiagKind::UnclosedDelim => write!(f, "{}unclosed delimiter", Style::Reset),
            DiagKind::KeywordAsId(kw) => write!(f, "{}keyword `{}{}{}` used as an identifier",
                Style::Reset, Style::High, kw.lexeme(), Style::Reset),
            DiagKind::MissingSemi => write!(f, "{}missing `{};{}` at the end of a statement",
                Style::Reset, Style::High, Style::Reset
            ),
            DiagKind::TrailingCommaParam => write!(f, "{}trailing comma in a function parameter list", Style::Reset),
            DiagKind::TrailingCommaArg => write!(f, "{}trailing comma in a function call", Style::Reset),
            DiagKind::MissingVarType => write!(f, "{}missing {}type{} in a variable declaration",
                Style::Reset, Style::High, Style::Reset
            ),
            DiagKind::MissingRetType => write!(f, "{}missing {}return type{} in a function declaration",
                Style::Reset, Style::High, Style::Reset
            ),
            DiagKind::MissingParamList => write!(f, "{}missing {}parameter list{} in a function declaration",
                Style::Reset, Style::High, Style::Reset
            ),
            DiagKind::EmptyParamList => write!(f, "{}empty {}parameter list{} in a function declaration",
                Style::Reset, Style::High, Style::Reset
            ),
        }
    }
}
