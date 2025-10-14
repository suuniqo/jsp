use super::DiagKind;


pub struct Diag {
    pub kind: DiagKind,
    pub row: usize,
    pub col: usize,
}

impl Diag {
    pub fn new(kind: DiagKind, row: usize, col: usize) -> Self {
        Self {
            kind,
            row,
            col,
        }
    }
}
