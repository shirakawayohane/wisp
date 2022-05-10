// WIP!

use crate::{parser::ASTNode, tokenizer::{Token, TokenKind}};

pub enum Type {
    F32,
    /*
    Object([(String, &Type)])
    */
}

pub struct Param<'a> {
    name: String,
    type_ref: &'a Type,
}

pub enum Expression<'a> {
    VariableDecl(String, &'a Type),
    FunctionCall(String),
    SymbolReference(String),
}

pub struct FunctionBody<'a> {
    exprs: Vec<Expression<'a>>
}

pub struct Function<'a> {
    pub name: String,
    pub params: &'a [Param<'a>],
    pub body: &'a FunctionBody<'a>
}

pub struct SemanticModel<'a> {
    pub types: Vec<&'a Type>,
    pub functions: Vec<Function<'a>>,
}

type TransformResult = Result<(), String>;

pub fn transform_function<'a>(model: &mut SemanticModel<'a>, defn_node: &'a ASTNode<'a>) {

}

pub fn transform_ast<'a>(modules: &[&'a ASTNode<'a>]) -> Result<SemanticModel<'a>, String> {
    let mut model = SemanticModel {
        types: Vec::new(),
        functions: Vec::new()
    };
    
    for module in modules {
        match module {
            ASTNode::Module(top_levels) => {
                for top_level in top_levels {
                    match top_level {
                        ASTNode::List(items) => {
                            if let Some(first) = items.first() {
                                match first {
                                    ASTNode::Symbol(token) => {
                                        match token.body {
                                            TokenKind::Symbol("");
                                        }
                                    },
                                    _ => return Result::Err("Top level list must starts with Symbol e.g defn".into())
                                }
                            } else {
                                return Result::Err(format!("Top level list must not be empty."))
                            }
                        }
                        _ => return Result::Err("Only lists are accepted as top level declaration for now.".into())
                    }
                }
            },
            _ => panic!("this function only accepts ASTNode::Module as a root of 'modules'")
        }
    }

    Result::Ok(model)
}