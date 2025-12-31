use std::{collections::BTreeMap, fmt};

use crate::{color::Color, diag::{Diag, DiagHelp, DiagKind, DiagSever, DiagSpan, HelpAction}, grammar::MetaSym, span::Span, target::Target};

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

            eprintln!("{}{} |", Color::HIGHLIGHT, max_padding);

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
                Color::BLUE, Color::RESET, Fmt(help),
            );

            eprintln!("{}{} |", Color::HIGHLIGHT, max_padding);

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

struct Fmt<'a, T>(&'a T);

impl<'a> fmt::Display for Fmt<'a, MetaSym> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            MetaSym::Stmnt => write!(f, "{}a {}statement{}",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::FuncBlock => write!(f, "{}a {}function{}",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::Expr => write!(f, "{}an {}expression{}",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::Type => write!(f, "{}a {}type{}",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::Id => write!(f, "{}an {}identifier{}",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::Assign => write!(f, "{}an {}assignment{}",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::Comma => write!(f, "{}`{},{}`",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::Semi => write!(f, "{}`{};{}`",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::LParen => write!(f, "{}`{}({}`",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::RParen => write!(f, "{}`{}){}`",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::LBrack => write!(f, "{}`{}{{{}`",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::RBrack => write!(f, "{}`{}}}{}`",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::OperBinary => write!(f, "{}a {}binary operator{}",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::OperUnary => write!(f, "{}an {}unary operator{}",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::FuncArgs => write!(f, "{}an {}argument list{}",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::FuncParams => write!(f, "{}a {}parameter list{}",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::FuncType => write!(f, "{}a {}return type{}",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::FuncId => write!(f, "{}a {}function name{}",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::FuncParam => write!(f, "{}a {}parameter{}",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::Func => write!(f, "{}`{}function{}`",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::While => write!(f, "{}`{}while{}`",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::If => write!(f, "{}`{}if{}`",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::Do => write!(f, "{}`{}do{}`",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::Let => write!(f, "{}`{}let{}`",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::Read => write!(f, "{}`{}read{}`",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::Write => write!(f, "{}`{}write{}`",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            MetaSym::Ret => write!(f, "{}`{}return{}`",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
        }
    }
}

impl<'a> fmt::Display for Fmt<'a, DiagHelp> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            DiagHelp::InsDecimal(..) => write!(f, "{}add a decimal part", Color::RESET),
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
                        Color::HIGHLIGHT, found.kind.lexeme(), Color::RESET,
                    )
                }
            },
            DiagHelp::DelToken(found) => write!(
                f,
                "{}remove the unnecessary `{}{}{}`",
                Color::RESET, Color::HIGHLIGHT, found.kind.lexeme(), Color::RESET
            ),
            DiagHelp::RepToken(found, rep) => write!(
                f,
                "{}replace `{}{}{}` by {}",
                Color::RESET, Color::HIGHLIGHT, found.kind.lexeme(), Color::RESET, Fmt(rep),
            ),
            DiagHelp::RepKw(_) => write!(f, "{}change the name to use it as an identifier", Color::RESET),
            DiagHelp::DelTrailingComma(_) => write!(f, "{}remove the trailing comma", Color::RESET),
            DiagHelp::InsVarType(_) => write!(f, "{}add the missing type", Color::RESET),
            DiagHelp::InsRetType(_) => write!(f, "{}add a {}return type", Color::RESET, Color::HIGHLIGHT),
            DiagHelp::InsParamList(..) => write!(f, "{}add a {}parameter list", Color::RESET, Color::HIGHLIGHT),
            DiagHelp::InsParam(_) => write!(f, "{}perhaps you meant to have no parameters", Color::RESET),
        }
    }
}

impl fmt::Display for DiagKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagKind::StrayChar(c) => write!(f, "{}illegal character `{}{}{}` in program",
                Color::RESET, Color::HIGHLIGHT,
                if c.is_control() {
                    c.escape_default().to_string()
                } else {
                    c.to_string()
                },
                Color::RESET
            ),
            DiagKind::UntermComm => write!(f, "{}unterminated block comment", Color::RESET),
            DiagKind::UntermStr(_) => write!(f, "{}missing terminating character `{}\"{}` on string literal",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            DiagKind::OverflowStr(_) => write!(f, "{}string literal is too long", Color::RESET),
            DiagKind::InvEscSeq(c) => write!(f, "{}unknown escape sequence `{}\\{c}{}`",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            DiagKind::OverflowInt => write!(f, "{}integer literal out of range for 16-byte type", Color::RESET),
            DiagKind::OverflowFloat => write!(f, "{}float literal out of range for 32-byte type", Color::RESET),
            DiagKind::InvFmtFloat(_) => write!(f, "{}missing decimal part after `{}.{}` in float literal",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            DiagKind::MalformedStr(c, _) => write!(f, "{}malformed string literal, contains control character `{}{}{}`",
                Color::RESET, Color::HIGHLIGHT, c.escape_default(), Color::RESET
            ),
            DiagKind::UnexpectedTok(found, expected) => {
                if expected.is_empty() {
                    return write!(f, "unexpected `{}{}{}`", Color::HIGHLIGHT, found.lexeme(), Color::RESET);
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

                write!(f, ", found `{}{}{}`", Color::HIGHLIGHT, found.lexeme(), Color::RESET)
            },
            DiagKind::MismatchedDelim(kind) => write!(f, "{}mismatched closing delimiter `{}{}{}`",
                Color::RESET, Color::HIGHLIGHT, kind.lexeme(), Color::RESET
            ),
            DiagKind::UnclosedDelim => write!(f, "{}unclosed delimiter", Color::RESET),
            DiagKind::KeywordAsId(kw) => write!(f, "{}keyword `{}{}{}` used as an identifier",
                Color::RESET, Color::HIGHLIGHT, kw.lexeme(), Color::RESET),
            DiagKind::MissingSemi => write!(f, "{}missing `{};{}` at the end of a statement",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            DiagKind::TrailingCommaParam => write!(f, "{}trailing comma in a function parameter list", Color::RESET),
            DiagKind::TrailingCommaArg => write!(f, "{}trailing comma in a function call", Color::RESET),
            DiagKind::MissingVarType => write!(f, "{}missing {}type{} in a variable declaration",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            DiagKind::MissingRetType => write!(f, "{}missing {}return type{} in a function declaration",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            DiagKind::MissingParamList => write!(f, "{}missing {}parameter list{} in a function declaration",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
            DiagKind::EmptyParamList => write!(f, "{}empty {}parameter list{} in a function declaration",
                Color::RESET, Color::HIGHLIGHT, Color::RESET
            ),
        }
    }
}
