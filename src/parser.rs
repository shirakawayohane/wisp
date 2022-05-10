use crate::tokenizer::{Token, TokenKind};
use crate::pos::Pos;

pub enum Type<'a> {
    Symbol(&'a Token<'a>),
}

pub struct Param<'a> {
    pub name: String,
    pub type_annotation: Type<'a>,
}

#[allow(dead_code)]
#[derive(Debug)]
pub enum ASTNodeKind<'a> {
    Module(Vec<ASTNode<'a>>), // Root of AST
    List(Vec<ASTNode<'a>>),
    Vector(Vec<ASTNode<'a>>),
    Symbol(&'a Token<'a>),
    F32Literal(&'a Token<'a> /* F32Literal */),
    Type(&'a Token<'a>, Box<ASTNode<'a>> /* List or Symbol */),
}

#[derive(Debug)]
pub struct ASTNode<'a> {
    kind: ASTNodeKind<'a>,
    pos: Pos
}

impl<'a> ASTNode<'a> {
    pub fn new(kind: ASTNodeKind<'a>, pos: Pos) -> Self {
        ASTNode { kind, pos }
    }
}

type ParseResult<'a> = Result<(ASTNode<'a>, &'a [Token<'a>]), String>;

/*
expr := list | vector | symbol | f32literal | type
*/
pub fn parse_expr<'a>(tokens: &'a [Token<'a>]) -> ParseResult<'a> {
    let first_token = &tokens[0];
    match first_token.body {
        TokenKind::Comma => parse_expr(&tokens[1..]), // skip comma
        TokenKind::LParen => parse_list(tokens),
        TokenKind::LBracket => parse_vector(tokens),
        TokenKind::Colon => parse_type(tokens),
        TokenKind::Symbol(sym) => ParseResult::Ok((ASTNode::new(ASTNodeKind::Symbol(&first_token), first_token.pos), &tokens[1..])),
        TokenKind::F32Literal(token) => {
            ParseResult::Ok((ASTNode::new(ASTNodeKind::F32Literal(&first_token), first_token.pos), &tokens[1..]))
        }
        _ => ParseResult::Err("Unexpected token".to_string()),
    }
}

fn parse_exprs_sorrounded_by<'a>(
    tokens: &'a [Token<'a>],
    open: TokenKind,
    close: TokenKind,
) -> Result<(Vec<ASTNode<'a>>, &'a [Token<'a>]), String> {
    // validation
    if tokens[0].body != open {
        return Result::Err(format!("Expected {} {}", open, tokens[0].pos));
    }
    let mut exprs = Vec::new();
    let mut rest = &tokens[1..];
    loop {
        if rest[0].body == close {
            rest = &rest[1..];
            break;
        }
        let (expr, _rest) = parse_expr(rest)?;
        rest = _rest;
        exprs.push(expr);
    }
    Result::Ok((exprs, rest))
}

/*
list := (expr*)
*/
pub fn parse_list<'a>(tokens: &'a [Token<'a>]) -> ParseResult<'a> {
    let (exprs, rest) = parse_exprs_sorrounded_by(tokens, TokenKind::LParen, TokenKind::RParen)?;
    ParseResult::Ok((ASTNode::List(exprs), rest))
}
#[test]
fn parse_list_test() {
    let lp = Token::new(Pos::new(1, 1, 1), TokenKind::LParen);
    let one = Token::new(Pos::new(1, 2, 4), TokenKind::Symbol("one"));
    let two = Token::new(Pos::new(1, 6, 8), TokenKind::Symbol("two"));
    let rp = Token::new(Pos::new(1, 9, 9), TokenKind::RParen);
    let tokens = &[lp, one, two, rp];
    let (list, rest) = parse_list(tokens).unwrap();
    let literal = ASTNode::List(vec![ASTNode::Symbol(&one), ASTNode::Symbol(&two)]);
    assert_eq!(list, literal);
    assert!(rest.is_empty());
}

/*
vector := [expr*]
*/
pub fn parse_vector<'a>(tokens: &'a [Token<'a>]) -> ParseResult<'a> {
    let (exprs, rest) =
        parse_exprs_sorrounded_by(tokens, TokenKind::LBracket, TokenKind::RBracket)?;
    ParseResult::Ok((ASTNode::Vector(exprs), rest))
}
#[test]
fn parse_vector_test() {
    let lb = Token::new(Pos::new(1, 1, 1), TokenKind::LBracket);
    let one = Token::new(Pos::new(1, 2, 4), TokenKind::Symbol("one"));
    let two = Token::new(Pos::new(1, 6, 8), TokenKind::Symbol("two"));
    let rb = Token::new(Pos::new(1, 9, 9), TokenKind::RBracket);
    let tokens = &[lb, one, two, rb];
    let (vector, rest) = parse_vector(tokens).unwrap();
    let literal = ASTNode::Vector(vec![ASTNode::Symbol(&one), ASTNode::Symbol(&two)]);
    assert_eq!(vector, literal);
    assert!(rest.is_empty());
}

/*
type = : symbol
       : type,?
       : type | type【todo】
       : type & type【todo】
       : (symbol type)【todo】
*/
pub fn parse_type<'a>(tokens: &'a [Token<'a>]) -> ParseResult<'a> {
    // validation
    if tokens[0].body != TokenKind::Colon {
        return ParseResult::Err("Expected : for type annotation".into());
    }

    let rest = &tokens[1..];
    if let Some(not_type_annotation_token_index) = rest.iter().position(|token| match token.body {
        TokenKind::Symbol(_) => false,
        _ => true,
    }) {
        let type_annotation_tokens = &rest[..not_type_annotation_token_index];
        if type_annotation_tokens.len() == 1 {
            match type_annotation_tokens[0].body {
                TokenKind::Symbol(_) => ParseResult::Ok((
                    ASTNode::Type(
                        &tokens[0],
                        Box::new(ASTNode::Symbol(&type_annotation_tokens[0])),
                    ),
                    &rest[1..],
                )),
                _ => ParseResult::Err(format!("Invalid type syntax \n{}", rest[0].pos)),
            }
        } else {
            todo!("Type annotation other than single Symbol (e.g i32) is not supported for now!")
        }
    } else {
        ParseResult::Err(format!(
            "Could not find end of type annotation {}",
            tokens[0].pos
        ))
    }
}

#[test]
fn parse_type_test() {
    let colon = Token::new(Pos::new(1, 1, 1), TokenKind::Colon);
    let i32_sym = Token::new(Pos::new(1, 3, 3), TokenKind::Symbol("f32"));
    let comma = Token::new(Pos::new(1, 4, 4), TokenKind::Comma);
    let tokens = [colon, i32_sym, comma];
    let (type_ast, rest) = parse_type(&tokens).unwrap();
    assert_eq!(rest, &[comma]);
    assert_eq!(
        type_ast,
        ASTNode::Type(&colon, Box::new(ASTNode::Symbol(&i32_sym)))
    );
}

pub fn parse_module<'a>(tokens: &'a [Token<'a>]) -> ParseResult<'a> {
    todo!()
}
