use std::fmt::Display;
use crate::pos::Pos;

#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind<'a> {
    LParen,          // (
    RParen,          // )
    LBracket,        // [
    RBracket,        // ]
    LBrace,          // {
    RBrace,          // }
    Sharp,           // #
    Colon,           // :
    And,             // &
    Pipe,            // |
    Comma, // , Note: Comma is almostly same as whitespace, but used for delimiting type annotation.
    F32Literal(f32), // e.g 3.14
    Symbol(&'a str), // e.g hoge
}

impl<'a> Display for TokenKind<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LBracket => write!(f, "["),
            TokenKind::RBracket => write!(f, "]"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::Sharp => write!(f, "#"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::And => write!(f, "&"),
            TokenKind::Pipe => write!(f, "|"),
            TokenKind::F32Literal(val) => write!(f, "{}", val),
            TokenKind::Symbol(sym) => write!(f, "{}", sym),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Token<'a> {
    pub pos: Pos,
    pub body: TokenKind<'a>,
}

impl<'a> Token<'a> {
    pub fn new(pos: Pos, body: TokenKind<'a>) -> Self {
        Token { pos, body }
    }
}

#[test]
pub fn tokenize_test() {
    assert_eq!(
        tokenize("(defn add [a : i32 b : i32] (3.14, 3.14))")
            .unwrap()
            .iter()
            .map(|t| t.body)
            .collect::<Vec<_>>(),
        vec![
            TokenKind::LParen,
            TokenKind::Symbol("defn"),
            TokenKind::Symbol("add"),
            TokenKind::LBracket,
            TokenKind::Symbol("a"),
            TokenKind::Colon,
            TokenKind::Symbol("i32"),
            TokenKind::Symbol("b"),
            TokenKind::Colon,
            TokenKind::Symbol("i32"),
            TokenKind::RBracket,
            TokenKind::LParen,
            TokenKind::F32Literal(3.14),
            TokenKind::Comma,
            TokenKind::F32Literal(3.14),
            TokenKind::RParen,
            TokenKind::RParen
        ]
    );
    // assert!(!tokenize("3.14.159").is_err())
}

#[test]
pub fn tokenize_pos_test() {
    let tokens = tokenize(
        "(test
1
2)",
    )
    .unwrap();
    assert_eq!(
        tokens.iter().map(|token| token.pos).collect::<Vec<Pos>>(),
        vec![
            Pos::new(1, 1, 1),
            Pos::new(1, 2, 5),
            Pos::new(2, 1, 1),
            Pos::new(3, 1, 1),
            Pos::new(3, 2, 2)
        ]
    )
}

#[allow(dead_code)]
const SPECIAL_CHARS: &'static [char] = &['(', ')', '[', ']', '{', '}', '#'];

pub fn tokenize<'a>(source: &'a str) -> Result<Vec<Token<'a>>, String> {
    let mut ret = Vec::new();
    let mut src = source;
    let mut line = 1;
    let mut pos_in_line = 1;
    loop {
        if let Some(c) = src.chars().next() {
            let mut eaten = 1;
            let body = match c {
                '\n' => {
                    line += 1;
                    pos_in_line = 1;
                    src = &src[1..];
                    continue;
                }
                ' ' | '\t' => {
                    pos_in_line += 1;
                    src = &src[1..];
                    continue;
                }
                '(' => TokenKind::LParen,
                ')' => TokenKind::RParen,
                '[' => TokenKind::LBracket,
                ']' => TokenKind::RBracket,
                '{' => TokenKind::LBrace,
                '}' => TokenKind::RBrace,
                '#' => TokenKind::Sharp,
                ':' => TokenKind::Colon,
                ',' => TokenKind::Comma,
                '&' => TokenKind::And,
                '|' => TokenKind::Pipe,
                _ => {
                    dbg!(c);
                    if c.is_digit(10) {
                        eaten = src.find(|c: char| c != '.' && !c.is_digit(10)).unwrap();
                        match (&src[0..eaten]).parse::<f32>() {
                            Ok(val) => TokenKind::F32Literal(val),
                            Err(err) => {
                                return Result::Err(format!(
                                    "Invalid syntax for number literal {}
                                    {}",
                                    &src[0..eaten],
                                    err
                                ))
                            }
                        }
                    } else {
                        eaten = src
                            .find(|c: char| {
                                c.is_whitespace() || c == ',' || SPECIAL_CHARS.contains(&c)
                            })
                            .unwrap();
                        let name = &src[0..eaten];
                        TokenKind::Symbol(name)
                    }
                }
            };
            let pos = Pos::new(line, pos_in_line, pos_in_line + eaten - 1);
            ret.push(Token::new(pos, body));
            pos_in_line += eaten;
            src = &src[eaten..];
        } else {
            break;
        }
    }
    return Ok(ret);
}
