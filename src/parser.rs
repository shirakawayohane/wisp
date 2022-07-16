use crate::lexer::{tokenize, Token};
use anyhow::{anyhow, bail, Context, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAST {
    I32,
    F32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AST<'a> {
    Module(Vec<AST<'a>>),
    NumberLiteral(&'a str),
    Symbol(&'a str),
    SymbolWithAnnotation(&'a str, TypeAST),
    Add,
    Sub,
    List(Vec<AST<'a>>),
}

fn parse_type(tokens: &mut Vec<Token>) -> Result<TypeAST> {
    Ok(match tokens.pop() {
        Some(Token::Symbol("i32")) => TypeAST::I32,
        Some(Token::Symbol("f32")) => TypeAST::F32,
        _ => todo!(),
    })
}

fn parse_list<'a>(tokens: &mut Vec<Token<'a>>) -> Result<AST<'a>> {
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
            None => bail!("Did not find enough tokens"),
            Some(t) => t,
        };
        list.push(match token {
            Token::LParen => {
                tokens.push(Token::LParen);
                parse_list(tokens)?
            }
            Token::NumberLiteral(val) => AST::NumberLiteral(val),
            Token::Add => AST::Add,
            Token::Sub => AST::Sub,
            Token::Symbol(name) => {
                if let Some(Token::Colon) = tokens.last() {
                    tokens.pop();
                    AST::SymbolWithAnnotation(name, parse_type(tokens)?)
                } else {
                    AST::Symbol(name)
                }
            }
            Token::RParen => return Ok(AST::List(list)),
            Token::Colon => unreachable!("Colon should be processed in Symbol arm"),
        });
    }
    Ok(AST::List(list))
}

fn parse_module<'a>(tokens: &mut Vec<Token<'a>>) -> Result<AST<'a>> {
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
    fn test_bin_ops() {
        let ast = parse(
            "(defn calc : f32
                      (a : f32 b : i32)
                        (+ a (- b 1.0)))",
        )
            .unwrap();
        assert_eq!(
            ast,
            AST::Module(vec![AST::List(vec![
                AST::Symbol("defn"),
                AST::SymbolWithAnnotation("calc", TypeAST::F32),
                AST::List(vec![
                    AST::SymbolWithAnnotation("a", TypeAST::F32),
                    AST::SymbolWithAnnotation("b", TypeAST::I32),
                ]),
                AST::List(vec![AST::Add, AST::Symbol("a"),
                               AST::List(vec![
                                   AST::Sub,
                                   AST::Symbol("b"),
                                   AST::NumberLiteral("1.0")])])])]))
    }
}
