use std::fmt;


#[derive(Clone, PartialEq)]
pub enum TokenKind {
    If,
    Do,
    While,
    Int,
    Float,
    Str,
    Bool,
    Void,
    Let,
    Func,
    Ret,
    Read,
    Write,
    True,
    False,

    FloatLit(f32),
    IntLit(i16),
    StrLit(String),
    Id(usize),

    Assign,
    AndAssign,

    Comma,
    Semi,
    LParen,
    RParen,
    LBrack,
    RBrack,

    Sum,
    Mul,
    Sub,
    Div,
    Mod,

    And,
    Or,
    Not,
    Lt,
    Le,
    Gt,
    Ge,
    Ne,
    Eq,

    Eof,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (code, value) = match self {
            TokenKind::Bool => ("Bool", None),
            TokenKind::Do => ("Do", None),
            TokenKind::Float => ("Float", None),
            TokenKind::Func => ("Func", None),
            TokenKind::If => ("If", None),
            TokenKind::Int => ("Int", None),
            TokenKind::Let => ("Let", None),
            TokenKind::Read => ("Read", None),
            TokenKind::Ret => ("Ret", None),
            TokenKind::Str => ("Str", None),
            TokenKind::Void => ("Void", None),
            TokenKind::While => ("While", None),
            TokenKind::Write => ("Write", None),
            TokenKind::True => ("True", None),
            TokenKind::False => ("False", None),

            TokenKind::FloatLit(value) => ("FloatLit", Some(value.to_string())),
            TokenKind::IntLit(value) => ("IntLit", Some(value.to_string())),
            TokenKind::StrLit(value) => ("StrLit", Some(format!("\"{value}\""))),
            TokenKind::Id(value) => ("Id", Some(value.to_string())),

            TokenKind::Assign => ("Assign", None),
            TokenKind::AndAssign => ("AndAssign", None),
            TokenKind::Comma => ("Comma", None),
            TokenKind::Semi => ("Semi", None),
            TokenKind::LParen => ("LParen", None),
            TokenKind::RParen => ("RParen", None),
            TokenKind::LBrack => ("LBrack", None),
            TokenKind::RBrack => ("RBrack", None),

            TokenKind::Sum => ("Sum", None),
            TokenKind::Mul => ("Mul", None),
            TokenKind::Sub => ("Sub", None),
            TokenKind::Div => ("Div", None),
            TokenKind::Mod => ("Mod", None),

            TokenKind::And => ("And", None),
            TokenKind::Or => ("Or", None),
            TokenKind::Not => ("Not", None),
            TokenKind::Lt => ("Lt", None),
            TokenKind::Le => ("Le", None),
            TokenKind::Gt => ("Gt", None),
            TokenKind::Ge => ("Ge", None),
            TokenKind::Ne => ("Ne", None),
            TokenKind::Eq => ("Eq", None),

            TokenKind::Eof => ("Eof", None),
        };

        write!(f, "<{}, {}>", code, value.unwrap_or("".to_string()))
    }
}

impl TokenKind {
    pub const MAX_STR_LEN: usize = 64;

    const KEYWORDS_LEN: usize = 15;
    const KEYWORDS: [TokenKind; TokenKind::KEYWORDS_LEN] = [
        TokenKind::If,
        TokenKind::Do,
        TokenKind::While,
        TokenKind::Int,
        TokenKind::Float,
        TokenKind::Str,
        TokenKind::Bool,
        TokenKind::Void,
        TokenKind::Let,
        TokenKind::Func,
        TokenKind::Ret,
        TokenKind::Read,
        TokenKind::Write,
        TokenKind::True,
        TokenKind::False,
    ];

    pub fn lexeme(&self) -> Option<&'static [u8]> {
        match self {
            TokenKind::Bool => Some(b"boolean"),
            TokenKind::Do => Some(b"do"),
            TokenKind::Float => Some(b"float"),
            TokenKind::Func => Some(b"function"),
            TokenKind::If => Some(b"if"),
            TokenKind::Int => Some(b"int"),
            TokenKind::Let => Some(b"let"),
            TokenKind::Read => Some(b"read"),
            TokenKind::Ret => Some(b"return"),
            TokenKind::Str => Some(b"string"),
            TokenKind::Void => Some(b"void"),
            TokenKind::While => Some(b"while"),
            TokenKind::Write => Some(b"write"),
            TokenKind::True => Some(b"true"),
            TokenKind::False => Some(b"false"),

            TokenKind::FloatLit(_) => None,
            TokenKind::IntLit(_) => None,
            TokenKind::StrLit(_) => None,
            TokenKind::Id(_) => None,

            TokenKind::Assign => Some(b"="),
            TokenKind::AndAssign => Some(b"&="),
            TokenKind::Comma => Some(b","),
            TokenKind::Semi => Some(b";"),
            TokenKind::LParen => Some(b"("),
            TokenKind::RParen => Some(b")"),
            TokenKind::LBrack => Some(b"{"),
            TokenKind::RBrack => Some(b"}"),

            TokenKind::Sum => Some(b"+"),
            TokenKind::Mul => Some(b"*"),
            TokenKind::Sub => Some(b"-"),
            TokenKind::Div => Some(b"/"),
            TokenKind::Mod => Some(b"%"),

            TokenKind::And => Some(b"&&"),
            TokenKind::Or => Some(b"||"),
            TokenKind::Not => Some(b"!"),
            TokenKind::Lt => Some(b"<"),
            TokenKind::Le => Some(b"<="),
            TokenKind::Gt => Some(b">"),
            TokenKind::Ge => Some(b">="),
            TokenKind::Ne => Some(b"!="),
            TokenKind::Eq => Some(b"=="),

            TokenKind::Eof => None,
        }
    }

    pub fn as_keyword(lexeme: &str) -> Option<TokenKind> {
        Some(Self::KEYWORDS[match lexeme {
            "if" => 0,
            "do" => 1,
            "while" => 2,
            "int" => 3,
            "float" => 4,
            "string" => 5,
            "boolean" => 6,
            "void" => 7,
            "let" => 8,
            "function" => 9,
            "return" => 10,
            "read" => 11,
            "write" => 12,
            "true" => 13,
            "false" => 14,
            _ => return None,
        }].clone())
    }
}
