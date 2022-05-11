use std::fmt::Display;

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct SourceRange {
    pub line_from: usize,
    pub from: usize,
    pub line_to: usize,
    pub to: usize,
}

impl SourceRange {
    pub fn new(line_from: usize, from: usize, line_to: usize, to: usize) -> Self {
        Self { line_from, line_to, from, to }
    }
}

impl Display for SourceRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "at {}:{}", self.line_from, self.from)
    }
}