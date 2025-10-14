pub struct DiagColor;

impl DiagColor {
    pub const RESET: &'static str = "\x1b[0m";
    pub const HIGHLIGHT: &'static str = "\x1b[1;38;5;189m";
    pub const BLUE: &'static str = "\x1b[1;38;5;81m";
    pub const ORANGE: &'static str = "\x1b[1;38;5;217m";
    pub const RED: &'static str =  "\x1b[1;38;5;211m";
}
