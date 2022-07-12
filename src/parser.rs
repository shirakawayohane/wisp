use crate::lexer::{tokenize, Token};
use anyhow::{anyhow, Context, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
    Module(Vec<AST>),
    NumberLiteral(String),
    Symbol(String),
    Add,
    List(Vec<AST>),
}

fn parse_list(tokens: &mut Vec<Token>) -> Result<AST> {
    let token = tokens.pop();
    if token != Some(Token::LParen) {
        return Err(anyhow!(
            "List must be start with LParen, but found {:?}",
            token
        ));
    }
    let mut list = Vec::new();
    while !tokens.is_empty() {
        let token = match tokens.pop() {
            None => return Err(anyhow!("Did not find enough tokens")),
            Some(t) => t,
        };
        list.push(match token {
            Token::LParen => {
                tokens.push(Token::LParen);
                parse_list(tokens)?
            }
            Token::NumberLiteral(val) => AST::NumberLiteral(val.to_string()),
            Token::Add => AST::Add,
            Token::Symbol(name) => AST::Symbol(name.to_string()),
            Token::RParen => return Ok(AST::List(list)),
        });
    }
    Ok(AST::List(list))
}

fn parse_module(tokens: &mut Vec<Token>) -> Result<AST> {
    let mut lists = Vec::new();
    while !tokens.is_empty() {
        dbg!(&tokens.last());
        if tokens.last() != Some(&Token::LParen) {
            return Err(anyhow!("Toplevel forms must be a list."));
        }
        lists.push(parse_list(tokens)?)
    }
    Ok(AST::Module(lists))
}

pub fn parse(source: &str) -> Result<AST> {
    let mut tokens = tokenize(source).with_context(|| format!("tokenize error"))?;
    tokens.reverse();
    parse_module(&mut tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        let ast = parse("(defn addTwo (a b) (+ a b))").unwrap();
        assert_eq!(
            ast,
            AST::Module(vec![AST::List(vec![
                AST::Symbol("defn".to_string()),
                AST::Symbol("addTwo".to_string()),
                AST::List(vec![
                    AST::Symbol("a".to_string()),
                    AST::Symbol("b".to_string())
                ]),
                AST::List(vec![
                    AST::Add,
                    AST::Symbol("a".to_string()),
                    AST::Symbol("b".to_string())
                ])
            ])])
        )
    }
}
