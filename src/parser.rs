use crate::lexer::{tokenize, Token};
use anyhow::{anyhow, ensure, Context, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAST {
    I32,
    F32,
    Bool,
    Unit,
    Array(Box<TypeAST>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AST<'a> {
    Module(Vec<AST<'a>>),
    NumberLiteral(&'a str),
    BoolLiteral(bool),
    Symbol(&'a str),
    SymbolWithAnnotation(&'a str, TypeAST),
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Gt,
    Ge,
    Lt,
    Le,
    And,
    Or,
    Not,
    List(Vec<AST<'a>>),
    Vector(Vec<AST<'a>>),
}

fn parse_type(tokens: &mut Vec<Token>) -> Result<TypeAST> {
    Ok(match tokens.pop() {
        Some(Token::Symbol("i32")) => TypeAST::I32,
        Some(Token::Symbol("f32")) => TypeAST::F32,
        Some(Token::Symbol("bool")) => TypeAST::Bool,
        Some(Token::LBracket) => {
            let item_type = parse_type(tokens)?;
            ensure!(!tokens.is_empty(), "not enough tokens");
            ensure!(*tokens.last().unwrap() == Token::RBracket, "expected ']'");
            tokens.pop();
            TypeAST::Array(Box::new(item_type))
        }
        _ => todo!(),
    })
}

pub fn parse<'a>(tokens: &mut Vec<Token<'a>>) -> Result<AST<'a>> {
    let first_token = tokens
        .pop()
        .with_context(|| "Parse error. Not enough tokens")?;
    Ok(match first_token {
        Token::LParen => {
            tokens.push(Token::LParen);
            parse_list(tokens)?
        }
        Token::LBracket => {
            tokens.push(Token::LBracket);
            parse_vector(tokens)?
        }
        Token::NumberLiteral(val) => AST::NumberLiteral(val),
        Token::Plus => AST::Add,
        Token::Minus => AST::Sub,
        Token::Asterisk => AST::Mul,
        Token::Slash => AST::Div,
        Token::Eq => AST::Eq,
        Token::Gt => AST::Gt,
        Token::Ge => AST::Ge,
        Token::Lt => AST::Lt,
        Token::Le => AST::Le,
        Token::Symbol(name) => {
            if let Some(Token::Colon) = tokens.last() {
                tokens.pop();
                AST::SymbolWithAnnotation(name, parse_type(tokens)?)
            } else {
                AST::Symbol(name)
            }
        }
        Token::RParen => unreachable!(),
        Token::RBracket => {
            dbg!(&tokens);
            unreachable!()
        }
        Token::Colon => unreachable!("Colon should be processed in Symbol arm"),
        Token::True => AST::BoolLiteral(true),
        Token::False => AST::BoolLiteral(false),
        Token::And => AST::And,
        Token::Or => AST::Or,
        Token::Not => AST::Not,
    })
}

fn parse_sorrounded_by<'a>(
    tokens: &mut Vec<Token<'a>>,
    open: Token,
    close: Token,
) -> Result<Vec<AST<'a>>> {
    ensure!(tokens.len() > 0, "Parse error. Not enough tokens");
    ensure!(
        tokens.last().unwrap() == &open,
        "Expected {} {}",
        open,
        tokens.last().unwrap()
    );
    tokens.pop();
    let mut nodes = Vec::new();
    while let Some(token) = tokens.last() {
        if *token == close {
            tokens.pop();
            break;
        }
        let node = parse(tokens)?;
        nodes.push(node);
    }
    Result::Ok(nodes)
}

fn parse_vector<'a>(tokens: &mut Vec<Token<'a>>) -> Result<AST<'a>> {
    Ok(AST::Vector(parse_sorrounded_by(
        tokens,
        Token::LBracket,
        Token::RBracket,
    )?))
}

fn parse_list<'a>(tokens: &mut Vec<Token<'a>>) -> Result<AST<'a>> {
    Ok(AST::List(parse_sorrounded_by(
        tokens,
        Token::LParen,
        Token::RParen,
    )?))
}

fn parse_module<'a>(tokens: &mut Vec<Token<'a>>) -> Result<AST<'a>> {
    let mut lists = Vec::new();
    while !tokens.is_empty() {
        if tokens.last() != Some(&Token::LParen) {
            return Err(anyhow!("Toplevel forms must be a list."));
        }
        lists.push(parse_list(tokens)?)
    }
    Ok(AST::Module(lists))
}

pub fn parse_source(source: &str) -> Result<AST> {
    let mut tokens = tokenize(source).with_context(|| format!("tokenize error"))?;
    tokens.reverse();
    parse_module(&mut tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_bin_ops() {
        let ast = parse_source(
            "(defn calc : f32
                [a : f32, b : i32]
                    (* 3.14 (/ (+ a (- b 1)) 2)))",
        )
        .unwrap();
        assert_eq!(
            ast,
            AST::Module(vec![AST::List(vec![
                AST::Symbol("defn"),
                AST::SymbolWithAnnotation("calc", TypeAST::F32),
                AST::Vector(vec![
                    AST::SymbolWithAnnotation("a", TypeAST::F32),
                    AST::SymbolWithAnnotation("b", TypeAST::I32),
                ]),
                AST::List(vec![
                    AST::Mul,
                    AST::NumberLiteral("3.14"),
                    AST::List(vec![
                        AST::Div,
                        AST::List(vec![
                            AST::Add,
                            AST::Symbol("a"),
                            AST::List(vec![AST::Sub, AST::Symbol("b"), AST::NumberLiteral("1")])
                        ]),
                        AST::NumberLiteral("2")
                    ])
                ])
            ])])
        )
    }
    #[test]
    fn test_bool() {
        let ast = parse_source(
            "
        (defn get-true: bool [] true)
        ",
        )
        .unwrap();
        assert_eq!(
            ast,
            AST::Module(vec![AST::List(vec![
                AST::Symbol("defn"),
                AST::SymbolWithAnnotation("get-true", TypeAST::Bool),
                AST::Vector(vec![]),
                AST::BoolLiteral(true)
            ])])
        )
    }
    #[test]
    fn test_parse_array_type() {
        let tokens = &mut vec![Token::LBracket, Token::Symbol("i32"), Token::RBracket];
        tokens.reverse();
        let ast = parse_type(tokens).unwrap();
        dbg!(&tokens);
        assert!(tokens.is_empty());
        assert_eq!(ast, TypeAST::Array(Box::new(TypeAST::I32)))
    }
    #[test]
    fn test_array_arg() {
        let ast = parse_source(
            "
        (defn first: i32
            [arr: [i32]]
            (0 arr))
        ",
        )
        .unwrap();
        assert_eq!(
            ast,
            AST::Module(vec![AST::List(vec![
                AST::Symbol("defn"),
                AST::SymbolWithAnnotation("first", TypeAST::I32),
                AST::Vector(vec![AST::SymbolWithAnnotation(
                    "arr",
                    TypeAST::Array(Box::new(TypeAST::I32))
                )]),
                AST::List(vec![AST::NumberLiteral("0"), AST::Symbol("arr")])
            ])])
        )
    }
}
