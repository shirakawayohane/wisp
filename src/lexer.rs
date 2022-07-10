use anyhow::{Result};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Symbol(String),
    NumberLiteral(String),
    Add,
    LParen,
    RParen,
}

const SPECIAL_CHARS: &'static [char] = &['(', ')', '[', ']', '{', '}', '#'];

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
                ' ' | '\t' => {
                    src = &src[1..];
                    continue;
                }
                '(' => Token::LParen,
                ')' => Token::RParen,
                '+' => Token::Add,
                _ => {
                    if c.is_digit(10) {
                        eaten = src.find(|c: char| c != '.' && !c.is_digit(10)).unwrap();
                        let value_str = &src[0..eaten];
                        Token::NumberLiteral(value_str.to_string())
                    } else {
                        eaten = src
                            .find(|c: char| {
                                c.is_whitespace() || c == ',' || SPECIAL_CHARS.contains(&c)
                            })
                            .unwrap();
                        let name = &src[0..eaten];
                        match name {
                            _ => Token::Symbol(name.to_string()),
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
