use crate::context::{symtable::SymTable, Context};
use crate::diag::{Diag, DiagKind};
use crate::target::Target;
use crate::token::{Token, TokenKind};
use crate::window::Window;
use crate::span::Span;

use super::Lexer;


pub struct LexerCore<'t, 'c, T: SymTable> {
    win: Window<'t>,
    trg: &'t Target,
    ctx: &'c mut Context<'t, T>,
}

impl<'t, 'c, T: SymTable> LexerCore<'t, 'c, T> {
    pub fn new(ctx: &'c mut Context<'t, T>, trg: &'t Target) -> Self {
        let win = Window::new(trg.src());

        Self {
            win,
            trg,
            ctx,
        }
    }

    fn skip_comment(&mut self) -> Result<(), Diag> {
        // eat initial '/' and '*'
        self.win.consume();
        self.win.consume();

        let span = self.win.span();

        loop {
            self.win.consume_while(|c| c != '*');

            if self.win.peek_two() == ('*', '/') {
                break;
            } else if self.win.finished() {
                return Err(Diag::new(DiagKind::UntermComm, span));
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

        return Ok(TokenKind::IntLit(val as i16));
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
                    return Err(DiagKind::InvFmtFloat);
                }
            } else if !next.is_ascii_digit() {
                break;
            }

            self.win.consume();
        }

        let slice = self.trg.slice_from_span(&self.win.span());

        if !has_dot {
            Self::parse_int(slice)
        } else {
            Self::parse_float(slice)
        }
    }

    fn read_str(&mut self) -> Result<Token, Diag> {
        let mut string = String::new();

        loop {
            match self.win.peek_one() {
                '\n' => return Err(Diag::new(DiagKind::UntermStr, self.win.span())),
                '\\' => {
                    let start = self.win.span().end;

                    // consume the '\'
                    self.win.consume();

                    let next = self.win.peek_one();

                    if self.win.finished() || next == '\n' {
                        return Err(Diag::new(DiagKind::UntermStr, self.win.span()));
                    }

                    if next.is_control() {
                        // target the control character
                        self.win.collapse();
                        self.win.consume();

                        let span = self.win.span();

                        // to recover from the error the line is skipped
                        self.win.consume_while(|next| next != '\n');

                        return Err(Diag::new(DiagKind::MalformedStr(next), span));
                    }

                    if let Some(esc_seq) = Self::esc_seq(next) {
                        // consume the escape character
                        self.win.consume();
                        string.push(esc_seq);
                    } else {
                        // consume escape character
                        self.win.consume();

                        self.ctx.reporter.push(Diag::new(
                            DiagKind::InvEscSeq(next),
                            Span::new(start, self.win.span().end)
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
                        return Err(Diag::new(DiagKind::UntermStr, self.win.span()));
                    }

                    if other.is_control() {
                        // target the control character
                        self.win.collapse();
                        self.win.consume();

                        let span = self.win.span();

                        // to recover from the error the line is skipped
                        self.win.consume_while(|next| next != '\n');

                        return Err(Diag::new(DiagKind::MalformedStr(other), span));
                    }

                    // consume the character
                    self.win.consume();
                    string.push(other);
                },
            }
        }

        let span = self.win.span();

        if string.len() > TokenKind::MAX_STR_LEN {
            Err(Diag::new(DiagKind::OverflowStr(string.len()), span))
        } else {
            Ok(Token::new(TokenKind::StrLit(string), span))
        }
    }

    fn read_id(&mut self) -> TokenKind {
        self.win.consume_while(|next| next.is_ascii_alphanumeric() || next == '_');

        let lexeme = self.trg.slice_from_span(&self.win.span());
        
        if let Some(keyword) = TokenKind::as_keyword(lexeme) {
            keyword
        } else {
            TokenKind::Id(self.ctx.symtable.intern(lexeme))
        }
    }

    fn next_token(&mut self) -> Result<Token, Diag> {
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
            return Ok(Token::new(TokenKind::Eof, self.win.span()));
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
            '/' => Ok(TokenKind::Div),
            '%' => Ok(TokenKind::Mod),
            '=' => {
                if self.win.peek_one() == '=' {
                    self.win.consume();
                    Ok(TokenKind::Eq)
                } else {
                    Ok(TokenKind::Assign)
                }
            },
            '!' => {
                if self.win.peek_one() == '=' {
                    self.win.consume();
                    Ok(TokenKind::Ne)
                } else {
                    Ok(TokenKind::Not)
                }
            },
            '<' => {
                if self.win.peek_one() == '=' {
                    self.win.consume();
                    Ok(TokenKind::Le)
                } else {
                    Ok(TokenKind::Lt)
                }
            },
            '>' => {
                if self.win.peek_one() == '=' {
                    self.win.consume();
                    Ok(TokenKind::Ge)
                } else {
                    Ok(TokenKind::Gt)
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
            '|' => {
                if self.win.peek_one() == '|' {
                    self.win.consume();
                    Ok(TokenKind::Or)
                } else {
                    Err(DiagKind::StrayChar('|'))
                }
            }
            '0'..='9' => self.read_num(),
            '"' => return self.read_str(),
            'a'..='z' | 'A'..='Z' | '_' => Ok(self.read_id()),
            other => Err(DiagKind::StrayChar(other)),
        };

        let span = self.win.span();

        match kind {
            Ok(kind) => Ok(Token::new(kind, span)),
            Err(kind) => Err(Diag::new(kind, span)),
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

impl<'t, 'c, T: SymTable> Iterator for LexerCore<'t, 'c, T> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.next_token() {
                Ok(token) => return if token.kind == TokenKind::Eof {
                    None
                } else {
                    Some(token)
                },
                Err(diag) => self.ctx.reporter.push(diag),
            }
        }
    }
}

impl<T: SymTable> Lexer for LexerCore<'_, '_, T> {}
