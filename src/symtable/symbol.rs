use std::{rc::Rc, fmt};


#[derive(Eq, Hash, PartialEq, Clone)]
pub struct Symbol {
    lexeme: Rc<str>
}

impl Symbol {
    pub fn from_lexeme(lexeme: &str) -> Self {
        Self {
            lexeme: Rc::from(lexeme)
        }
    }

    pub fn lexeme(&self) -> Rc<str> {
        Rc::clone(&self.lexeme)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "* '{}'", self.lexeme)
    }
}
