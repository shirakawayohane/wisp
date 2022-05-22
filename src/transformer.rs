/*
This module is for semantic level analysis and for compile time optiomizations.
*/

use crate::parser::{ASTKind, ASTNode};

pub enum Type {
    Unit,
    F32,
    /*
    Object([(String, &Type)])
    */
}

impl Type {
    pub fn get_size(&self) -> usize {
        match &self {
            Self::Unit => 0,
            Self::F32 => 4
        }
    }
}

pub struct Variable {
    pub name: String,
    pub ty: Type,
}

pub enum Expression<'a> {
    LetScope(Vec<Variable>, Vec<Expression<'a>>),
    FunctionCall(&'a Function<'a>, Vec<Expression<'a>>),
    SymbolReference(Variable),
    F32Const(f32)
}

impl<'a> Expression<'a> {
    pub fn get_type(&self) -> Type {
        match self {
            Self::LetScope(_, exprs) => {
                if exprs.len() == 0 {
                    Type::Unit
                } else {
                    exprs.last().unwrap().get_type()
                }
            },
            Self::FunctionCall(func, args) => func.return_type,
            Self::SymbolReference(p) => p.ty,
            Self::F32Const(_) => Type::F32
        }
    }
}

pub struct Function<'a> {
    pub name: String,
    pub index: u32,
    pub params: Vec<Variable>,
    pub return_type: Type,
    pub exprs: Vec<Expression<'a>>,
}

pub struct SemanticModel<'a> {
    pub types: Vec<Type>,
    pub functions: Vec<Function<'a>>,
}

type TransformResult = Result<(), String>;

pub fn transform_function(
    model: &mut SemanticModel,
    defn_node: &ASTNode,
) -> TransformResult {
    todo!()
}

pub fn transform_toplevel(
    model: &mut SemanticModel,
    top_level: &ASTNode,
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
    model: &mut SemanticModel,
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
