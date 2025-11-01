use crate::target::Target;


#[derive(Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
        }
    }
}

impl Target {
    pub fn coord_from_span(&self, span: &Span) -> Option<(usize, usize)> {
        let offsets = self.line_offsets();

        let row = offsets
            .binary_search_by(|offset| offset.cmp(&span.start))
            .unwrap_or_else(|e| e - 1);

        let (line, offset) = self.nth_line(row)?;

        let col = line
            .char_indices()
            .position(|(idx, _)| idx == span.start - offset)?;

        Some((row, col))
    }

    /// Returns the slice defined by the span
    /// Spans are assumed to be always correct, so no validation needed
    pub fn slice_from_span(&self, span: &Span) -> &str {
        &self.src()[span.start..span.end]
    }
}
