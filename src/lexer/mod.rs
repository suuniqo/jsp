use crate::context::Context;
use crate::target::Target;
use crate::token::{Token, TokenKind};

use crate::context::diagnostics::DiagKind;

pub mod consumer;
pub mod iter;

pub struct Lexer<'t, 'c> {
    bytes: &'t [u8],
    curr: Option<u8>,
    rpos: usize,
    row: usize,
    col: usize,
    ctx: &'c mut Context<'t>,
}

impl<'t, 'c> Lexer<'t, 'c> {
    pub fn new(ctx: &'c mut Context<'t>, target: &'t Target) -> Self {
        Self {
            bytes: target.bytes(),
            curr: Some(0),
            rpos: 0,
            row: 1,
            col: 0,
            ctx,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.read();
        self.skip_whitespace();

        let Some(curr) = self.curr else {
            return None;
        };

        let kind = match curr {
            b',' => TokenKind::Comma,
            b';' => TokenKind::Semi,
            b'(' => TokenKind::LParen,
            b')' => TokenKind::RParen,
            b'{' => TokenKind::LBrack,
            b'}' => TokenKind::RBrack,
            b'+' => TokenKind::Sum,
            b'-' => TokenKind::Sub,
            b'*' => TokenKind::Mul,
            b'%' => TokenKind::Mod,
            b'=' => {
                if self.peek().is_some_and(|next| next == b'=') {
                    self.read();
                    TokenKind::Eq
                } else {
                    TokenKind::Assign
                }
            },
            b'!' => {
                if self.peek().is_some_and(|next| next == b'=') {
                    self.read();
                    TokenKind::Ne
                } else {
                    TokenKind::Not
                }
            },
            b'<' => {
                if self.peek().is_some_and(|next| next == b'=') {
                    self.read();
                    TokenKind::Le
                } else {
                    TokenKind::Lt
                }
            },
            b'>' => {
                if self.peek().is_some_and(|next| next == b'=') {
                    self.read();
                    TokenKind::Ge
                } else {
                    TokenKind::Gt
                }
            },
            b'&' => {
                let Some(next) = self.peek() else {
                    self.ctx.diags.push(DiagKind::StrayChar('&'), self.row, self.col);
                    return self.next_token();
                };

                if next == b'&' {
                    self.read();
                    TokenKind::And
                } else if next == b'=' {
                    self.read();
                    TokenKind::AndAssign
                } else {
                    self.ctx.diags.push(DiagKind::StrayChar('&'), self.row, self.col);
                    return self.next_token();
                }
            },
            b'|' => {
                if self.peek().is_some_and(|next| next == b'|') {
                    self.read();
                    TokenKind::Or
                } else {
                    self.ctx.diags.push(DiagKind::StrayChar('|'), self.row, self.col);
                    return self.next_token();
                }
            }
            b'/' => {
                if self.peek().is_some_and(|next| next == b'*') {
                    self.skip_comment();
                    return self.next_token();
                } else {
                    TokenKind::Div
                }
            }
            b'0'..=b'9' => {
                if let Some(kind) = self.read_num() {
                    kind
                } else {
                    return self.next_token();
                }
            },
            b'"' => {
                if let Some(kind) = self.read_str() {
                    kind
                } else {
                    return self.next_token();
                }
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => self.read_id(),
            other => {
                self.ctx.diags.push(DiagKind::StrayChar(other as char), self.row, self.col);
                return self.next_token();
            }
        };

        Some(Token::new(kind, self.row, self.col))
    }

    fn peek(&mut self) -> Option<u8> {
        if self.rpos >= self.bytes.len() {
            None
        } else {
            Some(self.bytes[self.rpos])
        }
    }

    fn read(&mut self) {
        let Some(curr) = self.curr else {
            return;
        };

        if curr == b'\n' {
            self.row += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }

        self.curr = self.peek();
        self.rpos += 1;
    }


    fn skip_whitespace(&mut self) {
        while self.curr.is_some_and(|curr| curr.is_ascii_whitespace()) {
            self.read();
        }
    }

    fn skip_comment(&mut self) {
        let coords = (self.row, self.col);

        self.read();

        loop {
            self.read();

            let Some(curr) = self.curr else {
                self.ctx.diags.push(DiagKind::UnterminatedComment, coords.0, coords.1);
                break;
            };

            if curr == b'*' {
                self.read();

                if curr == b'/' {
                    break;
                }
            }
        }
    }

    fn read_num(&mut self) -> Option<TokenKind> {
        let col = self.col;
        let start = self.rpos - 1;

        let mut dot = None;

        loop {
            let next = self.peek();

            if dot.is_none() && next.is_some_and(|next| next == b'.') {
                dot = Some(self.rpos);

                self.read();

                if self.peek().is_none_or(|next| !next.is_ascii_digit()) {
                    self.ctx.diags.push(DiagKind::FloatInvFmt(self.rpos - start), self.row, col);
                    return None;
                }
            } else if next.is_none_or(|next| !next.is_ascii_digit()) {
                break;
            }

            self.read();
        }

        let Some(dot_pos) = dot else {
            let mut val = 0i32;

            for &byte in &self.bytes[start..self.rpos] {
                val = val * 10 + (byte - b'0') as i32;

                if val > i16::MAX as i32 {
                    self.ctx.diags.push(DiagKind::IntOverflow(self.rpos - start), self.row, col);
                    return None;
                }
            }

            return Some(TokenKind::IntConst(val as i16));
        };

        let mut val = 0f64;

        for &byte in &self.bytes[start..dot_pos] {
            val = val * 10.0 + (byte - b'0') as f64;

            if val > f32::MAX as f64 {
                self.ctx.diags.push(DiagKind::FloatOverflow(self.rpos - start), self.row, col);
                
                return None;
            }
        }

        let mut div = 10.0;

        for &byte in &self.bytes[dot_pos+1..self.rpos] {
            val += (byte - b'0') as f64 / div;
            div *= 10.0;

            if val > f32::MAX as f64 {
                self.ctx.diags.push(DiagKind::FloatOverflow(self.rpos - start), self.row, col);
                
                return None;
            }

            if div > 1e10 {
                break;
            }
        }

        let val = val as f32;

        if !(val.is_finite()) {
            self.ctx.diags.push(DiagKind::FloatOverflow(self.rpos - start), self.row, col);
            
            return None;
        }

        Some(TokenKind::FloatConst(val))
    }

    fn read_str(&mut self) -> Option<TokenKind> {
        let col = self.col;

        let mut str = String::new();
        let mut added_len = 1;

        loop {
            let Some(next) = self.peek() else {
                self.ctx.diags.push(DiagKind::UnterminatedStr(str.len() + added_len), self.row, col);
                return None;
            };

            if !next.is_ascii_graphic() && next != b' ' {
                self.ctx.diags.push(DiagKind::UnterminatedStr(str.len() + added_len), self.row, col);

                while !self.peek().is_none_or(|next| next == b'\n') {
                    self.read();
                }

                return None;
            }

            if next == b'"' {
                self.read();
                break;
            }

            if next == b'\\' {
                self.read();

                let Some(next) = self.peek() else {
                    self.ctx.diags.push(DiagKind::UnterminatedStr(str.len() + added_len), self.row, col);
                    return None;
                };

                if let Some(esc_seq) = Lexer::esc_seq(next) {
                    str.push(esc_seq as char);
                    added_len += 1;
                } else {
                    self.ctx.diags.push(DiagKind::InvEscSeq(next as char), self.row, self.col);
                    str.push('\\');
                    str.push(next as char);
                }
            } else {
                str.push(next as char);
            }

            self.read();
        }

        added_len += 1;

        if str.len() > TokenKind::MAX_STR_LEN {
            self.ctx.diags.push(DiagKind::StrOverflow((str.len(), added_len)), self.row, col);

            return None;
        }

        Some(TokenKind::StrConst(str))
    }

    fn read_id(&mut self) -> TokenKind {
        let start = self.rpos - 1;

        while self.peek().is_some_and(|next| next.is_ascii_alphanumeric() || next == b'_') {
            self.read();
        }
        
        TokenKind::Id(self.ctx.strpool.intern(&self.bytes[start..self.rpos]))
    }

    fn esc_seq(byte: u8) -> Option<u8> {
        match byte {
            b'0' => Some(b'\0'),
            b't' => Some(b'\t'),
            b'n' => Some(b'\n'),
            b'r' => Some(b'\r'),
            b'\\' => Some(b'\\'),
            b'\'' => Some(b'\''),
            b'\"' => Some(b'\"'),
            _ => None,
        }
    }
}
