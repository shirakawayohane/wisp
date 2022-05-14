/*
This module is for semantic level analysis and for compile time optiomizations.
*/

use crate::parser::{ASTKind, ASTNode, TypeSyntax};

pub enum Type {
    F32,
    /*
    Object([(String, &Type)])
    */
}

pub struct Param {
    pub name: String,
    pub type_syntax: TypeSyntax,
}

pub enum Expression<'a> {
    VariableDecl(String, &'a Type),
    FunctionCall(String),
    SymbolReference(String),
}

pub struct Function<'a> {
    pub name: String,
    pub params: &'a [Param],
    pub return_type: TypeSyntax,
    pub exprs: Vec<Expression<'a>>,
}

pub struct SemanticModel<'a> {
    pub types: Vec<&'a Type>,
    pub functions: Vec<Function<'a>>,
}

type TransformResult = Result<(), String>;

pub fn transform_function<'a>(
    model: &mut SemanticModel<'a>,
    defn_node: &'a ASTNode<'a>,
) -> TransformResult {
    todo!()
}

pub fn transform_toplevel<'a>(
    model: &mut SemanticModel<'a>,
    top_level: &'a ASTNode<'a>,
) -> TransformResult {
    match &top_level.kind {
        ASTKind::List(items) => {
            if let Some(first) = items.first() {
                match first.kind {
                    ASTKind::Symbol(sym) => match sym {
                        "defn" => transform_function(model, top_level),
                        _ => return Result::Err("Top level list can be only a function starts from 'defn' keyword for now.".into())
                    },
                    _ => {
                        return Result::Err(
                            "Top level list must starts with Symbol e.g defn"
                                .into(),
                        )
                    }
                }
            } else {
                return Result::Err(format!("Top level list must not be empty."));
            }
        }
        _ => {
            return Result::Err("Only lists are accepted as top level declaration for now.".into())
        }
    }
}

pub fn transform_module<'a>(
    model: &mut SemanticModel<'a>,
    modules: &[&'a ASTNode<'a>],
) -> TransformResult {
    for module in modules {
        match &module.kind {
            ASTKind::Module(top_levels) => {
                for top_level in top_levels {
                    transform_toplevel(model, top_level)?;
                }
            }
            _ => panic!("this function only accepts ASTNode::Module as a root of 'modules'"),
        }
    }
    Ok(())
}
