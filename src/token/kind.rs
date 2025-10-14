use std::fmt;


pub enum TokenKind {
    Bool,
    Do,
    Float,
    Func,
    If,
    Int,
    Let,
    Read,
    Ret,
    Str,
    Void,
    While,
    Write,
    True,
    False,

    FloatConst(f32),
    IntConst(i16),
    StrConst(String),
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

            TokenKind::FloatConst(value) => ("FloatCons", Some(value.to_string())),
            TokenKind::IntConst(value) => ("IntCons", Some(value.to_string())),
            TokenKind::StrConst(value) => ("StrCons", Some(format!("\"{value}\""))),
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
        };

        write!(f, "<{}, {}>", code, value.unwrap_or("-".to_string()))
    }
}

impl TokenKind {
    pub const MAX_STR_LEN: usize = 64;
    pub const KEYWORDS_LEN: usize = 12;

    pub const KEYWORDS: [&[u8]; TokenKind::KEYWORDS_LEN] = [
        b"let",
        b"int",
        b"float",
        b"boolean",
        b"string",
        b"void",
        b"write",
        b"read",
        b"function",
        b"return",
        b"do",
        b"while"
    ];

    pub fn lexeme(&self) -> Option<&'static str> {
        match self {
            TokenKind::Bool => Some("boolean"),
            TokenKind::Do => Some("do"),
            TokenKind::Float => Some("float"),
            TokenKind::Func => Some("function"),
            TokenKind::If => Some("if"),
            TokenKind::Int => Some("int"),
            TokenKind::Let => Some("let"),
            TokenKind::Read => Some("read"),
            TokenKind::Ret => Some("return"),
            TokenKind::Str => Some("string"),
            TokenKind::Void => Some("void"),
            TokenKind::While => Some("while"),
            TokenKind::Write => Some("write"),
            TokenKind::True => Some("true"),
            TokenKind::False => Some("false"),

            TokenKind::FloatConst(_) => None,
            TokenKind::IntConst(_) => None,
            TokenKind::StrConst(_) => None,
            TokenKind::Id(_) => None,

            TokenKind::Assign => Some("="),
            TokenKind::AndAssign => Some("&="),
            TokenKind::Comma => Some(","),
            TokenKind::Semi => Some(";"),
            TokenKind::LParen => Some("("),
            TokenKind::RParen => Some(")"),
            TokenKind::LBrack => Some("{"),
            TokenKind::RBrack => Some("}"),

            TokenKind::Sum => Some("+"),
            TokenKind::Mul => Some("*"),
            TokenKind::Sub => Some("-"),
            TokenKind::Div => Some("/"),
            TokenKind::Mod => Some("%"),

            TokenKind::And => Some("&&"),
            TokenKind::Or => Some("||"),
            TokenKind::Not => Some("!"),
            TokenKind::Lt => Some("<"),
            TokenKind::Le => Some("<="),
            TokenKind::Gt => Some(">"),
            TokenKind::Ge => Some(">="),
            TokenKind::Ne => Some("!="),
            TokenKind::Eq => Some("=="),
        }
    }
}
