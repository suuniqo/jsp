use crate::context::{diag::{Diag, DiagKind}, symtable::SymTable, Context};
use crate::target::Target;
use crate::token::{Token, TokenKind};

use super::Lexer;


pub struct LexerCore<'t, 'c, T: SymTable> {
    bytes: &'t [u8],
    curr: Option<u8>,
    rpos: usize,
    row: usize,
    col: usize,
    ctx: &'c mut Context<'t, T>,
}

impl<'t, 'c, T: SymTable> LexerCore<'t, 'c, T> {
    pub fn new(ctx: &'c mut Context<'t, T>, target: &'t Target) -> Self {
        Self {
            bytes: target.bytes(),
            curr: None,
            rpos: 0,
            row: 1,
            col: 1,
            ctx,
        }
    }

    #[inline]
    fn peek(&mut self) -> Option<u8> {
        self.bytes.get(self.rpos).copied()
    }

    fn read(&mut self) {
        if let Some(curr) = self.curr {
            if curr == b'\n' {
                self.row += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        }

        self.curr = self.peek();
        self.rpos = usize::min(self.rpos + 1, self.bytes.len());
    }


    fn skip_whitespace(&mut self) {
        while self.curr.is_some_and(|curr| curr.is_ascii_whitespace()) {
            self.read();
        }
    }

    fn skip_comment(&mut self) -> Result<(), Diag> {
        let coords = (self.row, self.col);

        self.read();

        loop {
            self.read();

            let Some(curr) = self.curr else {
                return Err(Diag::new(DiagKind::UnterminatedComment, coords.0, coords.1));
            };

            if curr == b'*' && self.peek().is_some_and(|next| next == b'/') {
                self.read();
                return Ok(())
            }
        }
    }

    fn read_num(&mut self) -> Result<TokenKind, DiagKind> {
        let start = self.rpos - 1;

        let mut dot = None;

        loop {
            let next = self.peek();

            if dot.is_none() && next.is_some_and(|next| next == b'.') {
                dot = Some(self.rpos);

                self.read();

                if self.peek().is_none_or(|next| !next.is_ascii_digit()) {
                    return Err(DiagKind::FloatInvFmt(self.rpos - start));
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
                    return Err(DiagKind::IntOverflow(self.rpos - start));
                }
            }

            return Ok(TokenKind::IntLit(val as i16));
        };

        let mut val = 0f64;

        for &byte in &self.bytes[start..dot_pos] {
            val = val * 10.0 + (byte - b'0') as f64;

            if val > f32::MAX as f64 {
                return Err(DiagKind::FloatOverflow(self.rpos - start));
            }
        }

        let mut div = 10.0;

        for &byte in &self.bytes[dot_pos+1..self.rpos] {
            val += (byte - b'0') as f64 / div;
            div *= 10.0;

            if val > f32::MAX as f64 {
                return Err(DiagKind::FloatOverflow(self.rpos - start));
            }

            if div > 1e10 {
                break;
            }
        }

        let val = val as f32;

        if !(val.is_finite()) {
            Err(DiagKind::FloatOverflow(self.rpos - start))
        } else {
            Ok(TokenKind::FloatLit(val))
        }

    }

    fn read_str(&mut self) -> Result<TokenKind, DiagKind> {
        let mut string = String::new();
        let mut added_len = 1;

        loop {
            let Some(next) = self.peek() else {
                return Err(DiagKind::UnterminatedStr(string.len() + added_len));
            };

            if !next.is_ascii_graphic() && next != b' ' {
                while !self.peek().is_none_or(|next| next == b'\n') {
                    self.read();
                }

                return Err(DiagKind::UnterminatedStr(string.len() + added_len));
            }

            if next == b'"' {
                self.read();
                break;
            }

            if next == b'\\' {
                self.read();

                let Some(next) = self.peek() else {
                    return Err(DiagKind::UnterminatedStr(string.len() + added_len));
                };

                if let Some(esc_seq) = Self::esc_seq(next) {
                    string.push(esc_seq as char);
                    added_len += 1;
                } else {
                    self.ctx.diags.push(Diag::new(DiagKind::InvEscSeq(next as char), self.row, self.col));

                    string.push('\\');
                    string.push(next as char);
                }
            } else {
                string.push(next as char);
            }

            self.read();
        }

        added_len += 1;

        if string.len() > TokenKind::MAX_STR_LEN {
            return Err(DiagKind::StrOverflow((string.len(), added_len)));
        }

        Ok(TokenKind::StrLit(string))
    }

    fn read_id(&mut self) -> TokenKind {
        let start = self.rpos - 1;

        while self.peek().is_some_and(|next| next.is_ascii_alphanumeric() || next == b'_') {
            self.read();
        }

        let pos = self.ctx.symtable.intern(&self.bytes[start..self.rpos]);
        
        if let Some(keyword) = self.ctx.symtable.as_keyword(pos) {
            keyword
        } else {
            TokenKind::Id(pos)
        }
    }

    fn next_token(&mut self) -> Result<Token, Diag> {
        loop {
            self.read();
            self.skip_whitespace();

            if self.curr.is_some_and(|curr| curr == b'/') && self.peek().is_some_and(|next| next == b'*') {
                self.skip_comment()?;
            } else {
                break;
            }
        }

        let Some(curr) = self.curr else {
            return Ok(Token::new(TokenKind::Eof, self.col, self.row));
        };

        let col = self.col;

        let kind = match curr {
            b',' => Ok(TokenKind::Comma),
            b';' => Ok(TokenKind::Semi),
            b'(' => Ok(TokenKind::LParen),
            b')' => Ok(TokenKind::RParen),
            b'{' => Ok(TokenKind::LBrack),
            b'}' => Ok(TokenKind::RBrack),
            b'+' => Ok(TokenKind::Sum),
            b'-' => Ok(TokenKind::Sub),
            b'*' => Ok(TokenKind::Mul),
            b'/' => Ok(TokenKind::Div),
            b'%' => Ok(TokenKind::Mod),
            b'=' => {
                if self.peek().is_some_and(|next| next == b'=') {
                    self.read();
                    Ok(TokenKind::Eq)
                } else {
                    Ok(TokenKind::Assign)
                }
            },
            b'!' => {
                if self.peek().is_some_and(|next| next == b'=') {
                    self.read();
                    Ok(TokenKind::Ne)
                } else {
                    Ok(TokenKind::Not)
                }
            },
            b'<' => {
                if self.peek().is_some_and(|next| next == b'=') {
                    self.read();
                    Ok(TokenKind::Le)
                } else {
                    Ok(TokenKind::Lt)
                }
            },
            b'>' => {
                if self.peek().is_some_and(|next| next == b'=') {
                    self.read();
                    Ok(TokenKind::Ge)
                } else {
                    Ok(TokenKind::Gt)
                }
            },
            b'&' => {
                let Some(next) = self.peek() else {
                    return Err(Diag::new(DiagKind::StrayChar('&'), self.row, col));
                };

                if next == b'&' {
                    self.read();
                    Ok(TokenKind::And)
                } else if next == b'=' {
                    self.read();
                    Ok(TokenKind::AndAssign)
                } else {
                    Err(DiagKind::StrayChar('&'))
                }
            },
            b'|' => {
                if self.peek().is_some_and(|next| next == b'|') {
                    self.read();
                    Ok(TokenKind::Or)
                } else {
                    Err(DiagKind::StrayChar('|'))
                }
            }
            b'0'..=b'9' => self.read_num(),
            b'"' => self.read_str(),
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => Ok(self.read_id()),
            other => Err(DiagKind::StrayChar(other as char)),
        };

        kind.map(|kind| Token::new(kind, self.row, col))
            .map_err(|kind| Diag::new(kind, self.row, col))
    }

    const fn esc_seq(byte: u8) -> Option<u8> {
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
                Err(diag) => self.ctx.diags.push(diag),
            }
        }
    }
}

impl<T: SymTable> Lexer for LexerCore<'_, '_, T> {}
