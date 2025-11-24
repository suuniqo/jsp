use std::fmt;


#[derive(Debug, Clone, PartialEq)]
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

    Sub,
    Sum,
    Mul,

    And,
    Not,

    Lt,
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
            TokenKind::Sub => ("Sub", None),
            TokenKind::Mul => ("Mul", None),

            TokenKind::And => ("And", None),
            TokenKind::Not => ("Not", None),
            TokenKind::Lt => ("Lt", None),
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

    pub fn lexeme(&self) -> &'static str {
        match self {
            TokenKind::Bool => "boolean",
            TokenKind::Do => "do",
            TokenKind::Float => "float",
            TokenKind::Func => "function",
            TokenKind::If => "if",
            TokenKind::Int => "int",
            TokenKind::Let => "let",
            TokenKind::Read => "read",
            TokenKind::Ret => "return",
            TokenKind::Str => "string",
            TokenKind::Void => "void",
            TokenKind::While => "while",
            TokenKind::Write => "write",
            TokenKind::True => "true",
            TokenKind::False => "false",

            TokenKind::FloatLit(_) => "float literal",
            TokenKind::IntLit(_) => "int literal",
            TokenKind::StrLit(_) => "string literal",
            TokenKind::Id(_) => "identifier",

            TokenKind::Assign => "=",
            TokenKind::AndAssign => "&=",
            TokenKind::Comma => ",",
            TokenKind::Semi => ";",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBrack => "{",
            TokenKind::RBrack => "}",

            TokenKind::Sum => "+",
            TokenKind::Sub => "-",
            TokenKind::Mul => "*",
            TokenKind::And => "&&",
            TokenKind::Not => "!",
            TokenKind::Lt => "<",
            TokenKind::Eq => "==",

            TokenKind::Eof => "EOF",
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
