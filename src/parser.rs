use crate::pos::SourceRange;
use crate::tokenizer::{Token, TokenKind};

pub struct Param {
    pub name: String,
    pub type_annotation: TypeSyntax,
}

/*
type := f32
*/
#[derive(Debug, PartialEq)]
pub enum TypeSyntax {
    F32,
}

impl TypeSyntax {
    pub fn is_primitive(self) -> bool {
        match self {
            TypeSyntax::F32 => true,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum ASTKind<'a> {
    Module(Vec<ASTNode<'a>>), // Root of AST
    List(Vec<ASTNode<'a>>),
    Vector(Vec<ASTNode<'a>>),
    Symbol(&'a str),
    F32Literal(f32),
    Type(TypeSyntax /* List or Symbol */),
}

#[derive(Debug, PartialEq)]
pub struct ASTNode<'a> {
    pub kind: ASTKind<'a>,
    pub range: SourceRange,
}

type ParseResult<'a> = Result<(ASTNode<'a>, &'a [Token<'a>]), String>;

/*
expr := list | vector | symbol | f32literal | type
*/
pub fn parse_expr<'a>(tokens: &'a [Token<'a>]) -> ParseResult<'a> {
    let first_token = &tokens[0];
    match first_token.kind {
        TokenKind::Comma => parse_expr(&tokens[1..]), // skip comma
        TokenKind::LParen => parse_list(tokens),
        TokenKind::LBracket => parse_vector(tokens),
        TokenKind::Colon => parse_type(tokens),
        TokenKind::Symbol(name) => ParseResult::Ok((
            ASTNode {
                kind: ASTKind::Symbol(name),
                range: first_token.range,
            },
            &tokens[1..],
        )),
        TokenKind::F32Literal(value) => ParseResult::Ok((
            ASTNode {
                kind: ASTKind::F32Literal(value),
                range: first_token.range,
            },
            &tokens[1..],
        )),
        _ => ParseResult::Err("Unexpected token".to_string()),
    }
}

fn parse_exprs_sorrounded_by<'a>(
    tokens: &'a [Token<'a>],
    open: TokenKind,
    close: TokenKind,
) -> Result<(Vec<ASTNode<'a>>, &'a [Token<'a>], &'a [Token<'a>]), String> {
    // validation
    if tokens[0].kind != open {
        return Result::Err(format!("Expected {} {}", open, tokens[0].range));
    }
    let mut exprs = Vec::new();
    let mut rest = &tokens[1..];
    loop {
        if rest[0].kind == close {
            rest = &rest[1..];
            break;
        }
        let (expr, _rest) = parse_expr(rest)?;
        rest = _rest;
        exprs.push(expr);
    }
    Result::Ok((exprs, &tokens[0..(tokens.len() - rest.len())], rest))
}

pub fn get_source_loc_from_tokens<'a>(tokens: &'a [Token<'a>]) -> Option<SourceRange> {
    let first_range = tokens.first()?.range;
    let last_range = tokens.last()?.range;
    Option::Some(SourceRange::new(
        first_range.line_from,
        first_range.from,
        last_range.line_from,
        last_range.to,
    ))
}

/*
list := (expr*)
*/
pub fn parse_list<'a>(tokens: &'a [Token<'a>]) -> ParseResult<'a> {
    let (exprs, used, rest) =
        parse_exprs_sorrounded_by(tokens, TokenKind::LParen, TokenKind::RParen)?;
    ParseResult::Ok((
        ASTNode {
            kind: ASTKind::List(exprs),
            range: get_source_loc_from_tokens(used).unwrap(),
        },
        rest,
    ))
}

#[test]
fn parse_list_test() {
    let lp = Token {
        kind: TokenKind::LParen,
        range: SourceRange::new(1, 1, 1, 1),
    };
    let one = Token {
        kind: TokenKind::Symbol("one"),
        range: SourceRange::new(1, 2, 1, 4),
    };
    let two = Token {
        kind: TokenKind::Symbol("two"),
        range: SourceRange::new(1, 6, 1, 8),
    };
    let rp = Token {
        kind: TokenKind::RParen,
        range: SourceRange::new(1, 9, 1, 9),
    };
    let tokens = &[lp, one, two, rp];
    let (list, rest) = parse_list(tokens).unwrap();
    let expect = ASTNode {
        kind: ASTKind::List(vec![
            ASTNode {
                kind: ASTKind::Symbol("one"),
                range: SourceRange::new(1, 2, 1, 4),
            },
            ASTNode {
                kind: ASTKind::Symbol("two"),
                range: SourceRange::new(1, 6, 1, 8),
            },
        ]),
        range: SourceRange::new(1, 1, 1, 9),
    };
    assert_eq!(list, expect);
    assert!(rest.is_empty());
}

/*
vector := [expr*]
*/
pub fn parse_vector<'a>(tokens: &'a [Token<'a>]) -> ParseResult<'a> {
    let (exprs, used, rest) =
        parse_exprs_sorrounded_by(tokens, TokenKind::LBracket, TokenKind::RBracket)?;
    ParseResult::Ok((
        ASTNode {
            kind: ASTKind::Vector(exprs),
            range: get_source_loc_from_tokens(used).unwrap(),
        },
        rest,
    ))
}

#[test]
fn parse_vector_test() {
    let lp = Token {
        kind: TokenKind::LParen,
        range: SourceRange::new(1, 1, 1, 1),
    };
    let one = Token {
        kind: TokenKind::Symbol("one"),
        range: SourceRange::new(1, 2, 1, 4),
    };
    let two = Token {
        kind: TokenKind::Symbol("two"),
        range: SourceRange::new(1, 6, 1, 8),
    };
    let rp = Token {
        kind: TokenKind::RParen,
        range: SourceRange::new(1, 9, 1, 9),
    };
    let tokens = &[lp, one, two, rp];
    let (list, rest) = parse_list(tokens).unwrap();
    let expect = ASTNode {
        kind: ASTKind::Vector(vec![
            ASTNode {
                kind: ASTKind::Symbol("one"),
                range: SourceRange::new(1, 2, 1, 4),
            },
            ASTNode {
                kind: ASTKind::Symbol("two"),
                range: SourceRange::new(1, 6, 1, 8),
            },
        ]),
        range: SourceRange::new(1, 1, 1, 9),
    };
    assert_eq!(list, expect);
    assert!(rest.is_empty());
}

/*
type = : f32
*/
pub fn parse_type<'a>(tokens: &'a [Token<'a>]) -> ParseResult<'a> {
    // validation
    if tokens[0].kind != TokenKind::Colon {
        return ParseResult::Err("Expected : for type annotation".into());
    }

    let rest = &tokens[1..];
    if let Some(not_type_annotation_token_index) = rest.iter().position(|token| match token.kind {
        TokenKind::Symbol(_) => false,
        _ => true,
    }) {
        let type_annotation_tokens = &rest[..not_type_annotation_token_index];
        if type_annotation_tokens.len() == 1 {
            match type_annotation_tokens[0].kind {
                TokenKind::Symbol(sym) => match sym {
                    "f32" => ParseResult::Ok((
                        ASTNode {
                            kind: ASTKind::Type(TypeSyntax::F32),
                            range: SourceRange::new(0, 0, 0, 0),
                        },
                        &rest[not_type_annotation_token_index..],
                    )),
                    _ => ParseResult::Err("Type reference is not supported for now!".into()),
                },
                _ => ParseResult::Err(format!("Invalid type syntax \n{}", rest[0].range)),
            }
        } else {
            todo!("Type annotation other than single Symbol (e.g i32) is not supported for now!")
        }
    } else {
        ParseResult::Err(format!(
            "Could not find end of type annotation {}",
            tokens[0].range
        ))
    }
}

#[test]
fn parse_type_test() {
    let colon = Token {
        range: SourceRange::new(1, 1, 1, 1),
        kind: TokenKind::Colon,
    };
    let f32_sym = Token {
        range: SourceRange::new(1, 3, 1, 3),
        kind: TokenKind::Symbol("f32"),
    };
    let comma = Token {
        range: SourceRange::new(1, 4, 1, 4),
        kind: TokenKind::Comma,
    };
    let tokens = [colon, f32_sym, comma];
    let (type_ast, rest) = parse_type(&tokens).unwrap();
    assert_eq!(rest, &[comma]);
    assert_eq!(
        type_ast,
        ASTNode {
            kind: ASTKind::Type(TypeSyntax::F32),
            range: SourceRange::new(1, 1, 1, 1)
        }
    );
}

pub fn parse_module<'a>(tokens: &'a [Token<'a>]) -> ParseResult<'a> {
    todo!()
}
