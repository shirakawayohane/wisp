use std::{collections::HashMap, rc::Rc, fmt::Display};

use crate::{emitter::{WasmPrimitiveType}, parser::TypeAST};

#[derive(Default, Debug)]
pub struct TypeEnv {
    env: HashMap<String, Box<Type>>,
}

#[derive(Hash, PartialEq, Eq, Debug)]
pub enum Type {
    I32,
    F32,
    Bool,
    Unit,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::F32 => write!(f, "f32"),
            Type::Bool => write!(f, "bool"),
            Type::Unit => write!(f, "()")
        }
    }
}

pub fn resolve_type<'a>(t: &TypeAST, type_env: &TypeEnv) -> Rc<Type> {
    dbg!(&type_env.env);
    match t {
        // ToDo: Optimization
        TypeAST::I32 => Rc::new(Type::I32),
        TypeAST::F32 => Rc::new(Type::F32),
        TypeAST::Bool => Rc::new(Type::Bool),
        TypeAST::Unit => Rc::new(Type::Unit),
    }
}

pub fn dissolve_type(t: Rc<Type>) -> Vec<WasmPrimitiveType> {
    match *t {
        Type::I32 | Type::Bool => {
            vec![WasmPrimitiveType::I32]
        }
        Type::F32 => {
            vec![WasmPrimitiveType::F32]
        }
        Type::Unit => {
            vec![]
        }
    }
}
