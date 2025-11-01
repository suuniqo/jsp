use crate::diag::DiagSever;


pub struct Color;

impl Color {
    pub const RESET: &'static str = "\x1b[0m";
    pub const HIGHLIGHT: &'static str = "\x1b[1;38;5;189m";
    pub const ORANGE: &'static str = "\x1b[1;38;5;217m";
    pub const RED: &'static str =  "\x1b[1;38;5;211m";
}

impl DiagSever {
    pub fn color(&self) -> &'static str {
        match self {
            DiagSever::Warning => Color::ORANGE,
            DiagSever::Error => Color::RED,
        }
    }
}

