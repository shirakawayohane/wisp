use std::{collections::HashMap, rc::Rc, fmt::Display};
use crate::{emitter::{WasmPrimitiveType}, parser::TypeAST};
use anyhow::Result;

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
    Array(Rc<Type>)
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::F32 => write!(f, "f32"),
            Type::Bool => write!(f, "bool"),
            Type::Unit => write!(f, "()"),
            Type::Array(a) => {
                write!(f, "[")?;
                a.fmt(f)?;
                write!(f, "]")
            }
        }
    }
}

pub fn resolve_type<'a>(t: &TypeAST, type_env: &TypeEnv) -> Result<Rc<Type>> {
    dbg!(&type_env.env);
    Ok(match t {
        // ToDo: Optimization
        TypeAST::I32 => Rc::new(Type::I32),
        TypeAST::F32 => Rc::new(Type::F32),
        TypeAST::Bool => Rc::new(Type::Bool),
        TypeAST::Unit => Rc::new(Type::Unit),
        TypeAST::Array(a) => {
            let item_type = resolve_type(a, type_env)?;
            Rc::new(Type::Array(item_type))
        }
    })
}

pub fn get_size(t: Rc<Type>) -> u32 {
    match *t {
        Type::I32 => 4,
        Type::F32 => 4,
        Type::Bool => 4,
        Type::Unit => 0,
        Type::Array(_) => 4, // size of pointer
    }
}

pub fn get_primitive_types(t: Rc<Type>) -> Vec<Option<WasmPrimitiveType>> {
    match *t {
        Type::I32 | Type::Bool => {
            vec![Some(WasmPrimitiveType::I32)]
        }
        Type::F32 => {
            vec![Some(WasmPrimitiveType::F32)]
        }
        Type::Unit => {
            vec![None]
        }
        Type::Array(_) => vec![Some(WasmPrimitiveType::I32)], // pointer,
    }
}
