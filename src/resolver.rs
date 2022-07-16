use std::collections::HashMap;

use crate::{parser::TypeAST, emitter::PrimitiveType};

#[derive(Default, Debug)]
pub struct TypeEnv {
    env: HashMap<String, Box<Type>>
}

#[derive(Hash, PartialEq, Eq, Debug)]
pub enum Type {
    I32,
    F32,
}

pub fn resolve_type<'a>(t: &TypeAST, type_env: &TypeEnv) -> Type {
    dbg!(&type_env.env);
    match t {
        TypeAST::I32 => Type::I32,
        TypeAST::F32 => Type::F32,
    }
}

pub fn dissolve_type(t: Type) -> Vec<PrimitiveType> {
    match t {
        Type::I32 => {
            vec![PrimitiveType::I32]
        },
        Type::F32 => {
            vec![PrimitiveType::F32]
        }
    }
}