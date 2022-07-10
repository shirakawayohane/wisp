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
        return Err(anyhow!("List must be start with LParen"));
    }
    let list = Vec::new();
    while !tokens.is_empty() {
        let token = match tokens.pop() {
            None => return Err(anyhow!("Did not find enough tokens")),
            Some(t) => t,
        };
        let mut list = Vec::new();
        list.push(match token {
            Token::LParen => parse_list(tokens)?,
            Token::NumberLiteral(val) => AST::NumberLiteral(val),
            Token::Add => AST::Add,
            Token::Symbol(name) => AST::Symbol(name),
            Token::RParen => return Ok(AST::List(list)),
        });
    }
    Ok(AST::List(list))
}

fn parse_module(tokens: &mut Vec<Token>) -> Result<AST> {
    let mut lists = Vec::new();
    while !tokens.is_empty() {
        if tokens.last() != Some(&Token::LParen) {
            return Err(anyhow!("Toplevel form must be a list."));
        }
        lists.push(parse_list(tokens)?)
    }
    Ok(AST::Module(lists))
}

pub fn parse(source: &str) -> Result<AST> {
    let mut tokens = tokenize(source).with_context(|| format!("tokenize error"))?;
    parse_module(&mut tokens)
}
