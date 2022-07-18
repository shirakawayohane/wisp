use std::{collections::HashMap, rc::Rc};

use crate::{parser::TypeAST, emitter::PrimitiveType};

#[derive(Default, Debug)]
pub struct TypeEnv {
    env: HashMap<String, Box<Type>>
}

#[derive(Hash, PartialEq, Eq, Debug)]
pub enum Type {
    I32,
    F32,
    Unit,
}

pub fn resolve_type<'a>(t: &TypeAST, type_env: &TypeEnv) -> Rc<Type> {
    dbg!(&type_env.env);
    match t {
        TypeAST::I32 => Rc::new(Type::I32),
        TypeAST::F32 => Rc::new(Type::F32),
        TypeAST::Unit => Rc::new(Type::Unit),
    }
}

pub fn dissolve_type(t: Rc<Type>) -> Vec<PrimitiveType> {
    match *t {
        Type::I32 => {
            vec![PrimitiveType::I32]
        },
        Type::F32 => {
            vec![PrimitiveType::F32]
        },
        Type::Unit => {
            vec![]
        }
    }
}