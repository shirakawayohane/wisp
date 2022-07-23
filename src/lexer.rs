use std::fmt::{Display};

use anyhow::Result;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Token<'a> {
    Symbol(&'a str),
    NumberLiteral(&'a str),
    Plus,
    Minus,
    Asterisk,
    Slash,
    Eq,
    Gt,
    Ge,
    Lt,
    Le,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Colon,
    True,
    False,
    And,
    Or,
    Not
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Symbol(s) => write!(f, "{}", s),
            Token::NumberLiteral(s) => write!(f, "{}", s),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Colon => write!(f, ":"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Eq => write!(f, "="),
            Token::Gt => write!(f, ">"),
            Token::Ge => write!(f, ">="),
            Token::Lt => write!(f, "<"),
            Token::Le => write!(f, "<="),
            Token::And => write!(f, "and"),
            Token::Or => write!(f, "or"),
            Token::Not => write!(f, "not")
        }
    }
}

const SPECIAL_CHARS: &'static [char] = &['(', ')', ':', ',', '[', ']'];

pub fn tokenize(source: &str) -> Result<Vec<Token>> {
    let mut ret = Vec::new();
    let mut src = source;
    loop {
        if let Some(c) = src.chars().next() {
            let mut eaten = 1;
            let token = match c {
                '\n' => {
                    src = &src[1..];
                    continue;
                }
                ' ' | '\t' | ',' => {
                    src = &src[1..];
                    continue;
                }
                '(' => Token::LParen,
                ')' => Token::RParen,
                '[' => Token::LBracket,
                ']' => Token::RBracket,
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Asterisk,
                '/' => Token::Slash,
                ':' => Token::Colon,
                '=' => Token::Eq,
                '>' => {
                    if src.starts_with(">=") {
                        eaten = 2;
                        Token::Ge
                    } else {
                        Token::Gt
                    }
                },
                '<' => {
                    if src.starts_with("<=") {
                        eaten = 2;
                        Token::Le
                    } else {
                        Token::Lt
                    }
                }
                _ => {
                    if c.is_digit(10) {
                        eaten = src.find(|c: char| c != '.' && !c.is_digit(10)).unwrap();
                        let value_str = &src[0..eaten];
                        Token::NumberLiteral(value_str)
                    } else {
                        eaten = src
                            .find(|c: char| {
                                c.is_whitespace() || SPECIAL_CHARS.contains(&c)
                            })
                            .unwrap();
                        let name = &src[0..eaten];
                        match name {
                            "true" => Token::True,
                            "false" => Token::False,
                            "and" => Token::And,
                            "or" => Token::Or,
                            "not" => Token::Not,
                            _ => Token::Symbol(name),
                        }
                    }
                }
            };
            ret.push(token);
            src = &src[eaten..];
        } else {
            break;
        }
    }
    return Ok(ret);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_calc() {
        let tokens = tokenize(
            "(defn calc : i32
                       (a :f32 b: i32) 
                         (* 3.14 (/ (+ a (- b 1)) 2))",
        )
            .unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::LParen,
                Token::Symbol("defn"),
                Token::Symbol("calc"),
                Token::Colon,
                Token::Symbol("i32"),
                Token::LParen,
                Token::Symbol("a"),
                Token::Colon,
                Token::Symbol("f32"),
                Token::Symbol("b"),
                Token::Colon,
                Token::Symbol("i32"),
                Token::RParen,
                Token::LParen,
                Token::Asterisk,
                Token::NumberLiteral("3.14"),
                Token::LParen,
                Token::Slash,
                Token::LParen,
                Token::Plus,
                Token::Symbol("a"),
                Token::LParen,
                Token::Minus,
                Token::Symbol("b"),
                Token::NumberLiteral("1"),
                Token::RParen,
                Token::RParen,
                Token::NumberLiteral("2"),
                Token::RParen,
                Token::RParen
            ]
        );
        let tokens = tokenize("
            (or (and false (a < b))
                (and true (a >= b)))").unwrap();
        assert_eq!(tokens,
        vec![
            Token::LParen,
            Token::Or,
            Token::LParen,
            Token::And,
            Token::False,
            Token::LParen,
            Token::Symbol("a"),
            Token::Lt,
            Token::Symbol("b"),
            Token::RParen,
            Token::RParen,
            Token::LParen,
            Token::And,
            Token::True,
            Token::LParen,
            Token::Symbol("a"),
            Token::Ge,
            Token::Symbol("b"),
            Token::RParen,
            Token::RParen,
            Token::RParen,
        ])
    }

    #[test]
    fn test_sub() {
        let tokens = tokenize("(- a 1)").unwrap();
        assert_eq!(tokens, vec![Token::LParen, Token::Minus, Token::Symbol("a"), Token::NumberLiteral("1"), Token::RParen])
    }
    #[test]
    fn test_bool_ops() {
        let tokens = tokenize("(and (> a b) (< a c) (>= a d) (<= a e))").unwrap();
        assert_eq!(
            tokens,
        vec![
            Token::LParen,
            Token::And,
            Token::LParen,
            Token::Gt,
            Token::Symbol("a"),
            Token::Symbol("b"),
            Token::RParen,
            Token::LParen,
            Token::Lt,
            Token::Symbol("a"),
            Token::Symbol("c"),
            Token::RParen,
            Token::LParen,
            Token::Ge,
            Token::Symbol("a"),
            Token::Symbol("d"),
            Token::RParen,
            Token::LParen,
            Token::Le,
            Token::Symbol("a"),
            Token::Symbol("e"),
            Token::RParen,
            Token::RParen,
        ])
    }
}
