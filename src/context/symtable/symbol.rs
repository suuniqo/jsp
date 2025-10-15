use std::{rc::Rc, fmt};


#[derive(Eq, Hash, PartialEq, Clone)]
pub struct Symbol {
    lexeme: Rc<[u8]>
}

impl Symbol {
    pub fn from_bytes(bytes: &[u8]) -> Self {
        Self {
            lexeme: Rc::from(bytes)
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "* '{}'", String::from_utf8_lossy(&self.lexeme))
    }
}
