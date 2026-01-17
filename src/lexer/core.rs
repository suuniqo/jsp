use std::cell::RefCell;
use std::rc::Rc;

use crate::reporter::Reporter;
use crate::diag::{DiagRef, DiagHelp, DiagKind, Diag};
use crate::pool::{PoolInterner, PoolLookup};
use crate::target::Target;
use crate::token::{Token, TokenKind};
use crate::span::Span;

use super::{Lexer, window::Window};


#[derive(Clone)]
pub struct LexerCore<'t, Pool: PoolInterner + PoolLookup> {
    win: Window<'t>,
    target: &'t Target,
    reporter: Rc<RefCell<Reporter<'t, Pool>>>,
    pool: Rc<RefCell<Pool>>,
}

impl<'t, Pool> LexerCore<'t, Pool>
where
    Pool: PoolLookup + PoolInterner
{
    pub fn new(
        reporter: Rc<RefCell<Reporter<'t, Pool>>>,
        pool: Rc<RefCell<Pool>>,
        target: &'t Target
    ) -> Self {
        let win = Window::new(target.src());

        Self {
            win,
            target,
            reporter,
            pool,
        }
    }

    fn skip_comment(&mut self) -> Result<(), DiagRef> {
        // eat initial '/' and '*'
        self.win.consume();
        self.win.consume();

        let span = self.win.span();

        loop {
            self.win.consume_while(|c| c != '*');

            if self.win.peek_two() == ('*', '/') {
                break;
            } else if self.win.finished() {
                return Err(Diag::make(DiagKind::UntermComm, span, true));
            }

            self.win.consume();
        }

        // eat ending '*' and '/'
        self.win.consume();
        self.win.consume();

        Ok(())
    }

    fn parse_int(slice: &str) -> Result<TokenKind, DiagKind> {
        let mut val = 0i32;

        for c in slice.chars() {
            val = val * 10 + (c as u8 - b'0') as i32;

            if val > i16::MAX as i32 {
                return Err(DiagKind::OverflowInt);
            }
        }

        Ok(TokenKind::IntLit(val as i16))
    }

    fn parse_float(slice: &str) -> Result<TokenKind, DiagKind> {
        let mut val = 0f64;
        let mut chars = slice.chars();

        // compute integer part
        for c in chars.by_ref().take_while(|&c| c != '.') {
            val = val * 10.0 + (c as u8 - b'0') as f64;

            if val > f32::MAX as f64 {
                return Err(DiagKind::OverflowFloat);
            }
        }

        let mut div = 10.0;

        // consume the '.'
        chars.next();

        // compute decimal part
        for c in chars {
            val += (c as u8 - b'0') as f64 / div;
            div *= 10.0;

            if val > f32::MAX as f64 {
                return Err(DiagKind::OverflowFloat);
            }

            if div > 1e10 {
                break;
            }
        }

        let val = val as f32;

        if !val.is_finite() {
            Err(DiagKind::OverflowFloat)
        } else {
            Ok(TokenKind::FloatLit(val))
        }
    }

    fn read_num(&mut self) -> Result<TokenKind, DiagKind> {
        let mut has_dot = false;

        while !self.win.finished() {
            let next = self.win.peek_one();

            if !has_dot && next == '.' {
                has_dot = true;
                self.win.consume();

                if !self.win.peek_one().is_ascii_digit() {
                    let mut span = self.win.span();

                    // remove '.'
                    span.end -= 1;

                    let slice = self.target.slice_from_span(&span)
                        .expect("invalid span when parsing number");

                    return Err(DiagKind::InvFmtFloat(slice.to_string().parse::<f32>().unwrap_or(0.0)));
                }
            } else if !next.is_ascii_digit() {
                break;
            }

            self.win.consume();
        }

        let slice = self.target.slice_from_span(&self.win.span())
            .expect("invalid span when parsing number");

        if !has_dot {
            Self::parse_int(slice)
        } else {
            Self::parse_float(slice)
        }
    }

    fn make_unterm_str_diag(&self, string: String) -> DiagRef {
        let diag = Diag::make(DiagKind::UntermStr(string), self.win.span(), false);

        diag.with_help(DiagHelp::InsQuote(self.win.span()))
    }

    fn read_str(&mut self) -> Result<Token, DiagRef> {
        let mut string = String::new();

        loop {
            match self.win.peek_one() {
                '\n' => return Err(self.make_unterm_str_diag(string)),
                '\\' => {
                    let start = self.win.span().end;

                    // consume the '\'
                    self.win.consume();

                    let next = self.win.peek_one();

                    if self.win.finished() || next == '\n' {
                        return Err(self.make_unterm_str_diag(string));
                    }

                    if next.is_control() {
                        // target the control character
                        self.win.collapse();
                        self.win.consume();

                        let span = self.win.span();

                        // to recover from the error the line is skipped
                        self.win.consume_while(|next| next != '\n');

                        return Err(Diag::make(DiagKind::MalformedStr(next, string), span, true));
                    }

                    if let Some(esc_seq) = Self::esc_seq(next) {
                        // consume the escape character
                        self.win.consume();
                        string.push(esc_seq);
                    } else {
                        // consume escape character
                        self.win.consume();

                        self.reporter.borrow_mut().emit(Diag::make(
                            DiagKind::InvEscSeq(next),
                            Span::new(start, self.win.span().end),
                            true,
                        ));

                        string.push('\\');
                        string.push(next);
                    }
                },
                '"' => {
                    self.win.consume();
                    break;
                },
                other => {
                    if self.win.finished() {
                        return Err(self.make_unterm_str_diag(string));
                    }

                    if other.is_control() {
                        // target the control character
                        self.win.collapse();
                        self.win.consume();

                        let span = self.win.span();

                        // to recover from the error the line is skipped
                        self.win.consume_while(|next| next != '\n');

                        return Err(Diag::make(DiagKind::MalformedStr(other, string), span, true));
                    }

                    // consume the character
                    self.win.consume();
                    string.push(other);
                },
            }
        }

        let span = self.win.span();

        if string.len() > TokenKind::MAX_STR_LEN {
            Err(Diag::make(DiagKind::OverflowStr(string[..TokenKind::MAX_STR_LEN].to_string()), span, true))
        } else {
            Ok(Token::new(TokenKind::StrLit(string), span))
        }
    }

    fn read_id(&mut self) -> TokenKind {
        self.win.consume_while(|next| next.is_ascii_alphanumeric() || next == '_');

        let lexeme = self.target.slice_from_span(&self.win.span())
            .expect("invalid span when parsing lexeme");
        
        if let Some(keyword) = TokenKind::as_keyword(lexeme) {
            keyword
        } else {
            let id = self.pool.borrow_mut().intern(lexeme);

            TokenKind::Id(id)
        }
    }

    fn next_token(&mut self) -> Result<Token, DiagRef> {
        loop {
            self.win.consume_while(|c| c.is_ascii_whitespace());
            self.win.collapse();

            if self.win.peek_two() == ('/', '*') {
                self.skip_comment()?;
            } else {
                break
            }
        }

        self.win.collapse();

        let Some(curr) = self.win.consume() else {
            return Ok(Token::eof());
        };

        let kind = match curr {
            ',' => Ok(TokenKind::Comma),
            ';' => Ok(TokenKind::Semi),
            '(' => Ok(TokenKind::LParen),
            ')' => Ok(TokenKind::RParen),
            '{' => Ok(TokenKind::LBrack),
            '}' => Ok(TokenKind::RBrack),
            '+' => Ok(TokenKind::Sum),
            '-' => Ok(TokenKind::Sub),
            '*' => Ok(TokenKind::Mul),
            '!' => Ok(TokenKind::Not),
            '<' => Ok(TokenKind::Lt),
            '=' => {
                if self.win.peek_one() == '=' {
                    self.win.consume();
                    Ok(TokenKind::Eq)
                } else {
                    Ok(TokenKind::Assign)
                }
            },
            '&' => {
                match self.win.peek_one() {
                    '&' => {
                        self.win.consume();
                        Ok(TokenKind::And)
                    },
                    '=' => {
                        self.win.consume();
                        Ok(TokenKind::AndAssign)
                    }
                    _ => Err(DiagKind::StrayChar('&'))
                }
            },
            '0'..='9' => self.read_num(),
            '"' => return self.read_str(),
            'a'..='z' | 'A'..='Z' | '_' => Ok(self.read_id()),
            other => Err(DiagKind::StrayChar(other)),
        };

        let span = self.win.span();

        match kind {
            Ok(kind) => Ok(Token::new(kind, span)),
            Err(kind) => Err(Diag::make(kind, span, true)),
        }
    }

    const fn esc_seq(byte: char) -> Option<char> {
        match byte {
            't' => Some('\t'),
            'n' => Some('\n'),
            _ => None,
        }
    }
}

impl<Pool> Iterator for LexerCore<'_, Pool>
where
    Pool: PoolLookup + PoolInterner
{
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.next_token() {
                Ok(token) => return if token.kind == TokenKind::Eof {
                    None
                } else {
                    Some(token)
                },
                Err(mut diag) => {
                    let dummy = match diag.kind.clone() {
                        DiagKind::UntermStr(str) => Some(TokenKind::StrLit(str)),
                        DiagKind::MalformedStr(_, str) => Some(TokenKind::StrLit(str)),
                        DiagKind::OverflowStr(str) => Some(TokenKind::StrLit(str)),
                        DiagKind::OverflowInt => Some(TokenKind::IntLit(i16::MAX)),
                        DiagKind::OverflowFloat => Some(TokenKind::FloatLit(f32::MAX)),
                        DiagKind::InvFmtFloat(num) => Some(TokenKind::FloatLit(num)),
                        _ => None,
                    };


                    if matches!(diag.kind, DiagKind::InvFmtFloat(_)) {
                        diag.add_help(DiagHelp::InsDecimal(diag.main_span()));
                    }

                    let span = diag.main_span();
                    self.reporter.borrow_mut().emit(diag);

                    if let Some(dummy) = dummy {
                        return Some(Token::new(dummy, span));
                    }
                },
            }
        }
    }
}

impl<'t, Pool> Lexer for LexerCore<'t, Pool>
where
    Pool: PoolLookup + PoolInterner
{}
