use std::fmt::Display;

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct Pos {
    pub line: usize,
    pub from: usize,
    pub to: usize,
}

impl Pos {
    pub fn new(line: usize, from: usize, to: usize) -> Self {
        Self { line, from, to }
    }
}

impl Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "at {}:{}", self.line, self.from)
    }
}