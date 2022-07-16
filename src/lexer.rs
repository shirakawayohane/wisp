use anyhow::Result;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Token<'a> {
    Symbol(&'a str),
    NumberLiteral(&'a str),
    Add,
    Sub,
    LParen,
    RParen,
    Colon,
}

const SPECIAL_CHARS: &'static [char] = &['(', ')', ':', ','];

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
                '+' => Token::Add,
                '-' => Token::Sub,
                ':' => Token::Colon,
                _ => {
                    if c.is_digit(10) {
                        eaten = src.find(|c: char| c != '.' && !c.is_digit(10)).unwrap();
                        let value_str = &src[0..eaten];
                        Token::NumberLiteral(value_str)
                    } else {
                        eaten = src
                            .find(|c: char| {
                                c.is_whitespace() || c == ',' || SPECIAL_CHARS.contains(&c)
                            })
                            .unwrap();
                        let name = &src[0..eaten];
                        match name {
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
    fn test_add() {
        let tokens = tokenize(
            "(defn addTwo : i32
                       (a :i32 b: i32) 
                         (+ a b))",
        )
            .unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::LParen,
                Token::Symbol("defn"),
                Token::Symbol("addTwo"),
                Token::Colon,
                Token::Symbol("i32"),
                Token::LParen,
                Token::Symbol("a"),
                Token::Colon,
                Token::Symbol("i32"),
                Token::Symbol("b"),
                Token::Colon,
                Token::Symbol("i32"),
                Token::RParen,
                Token::LParen,
                Token::Add,
                Token::Symbol("a"),
                Token::Symbol("b"),
                Token::RParen,
                Token::RParen,
            ]
        )
    }

    #[test]
    fn test_sub() {
        let tokens = tokenize("(- a 1)").unwrap();
        assert_eq!(tokens, vec![Token::LParen, Token::Sub, Token::Symbol("a"), Token::NumberLiteral("1"), Token::RParen])
    }
}
