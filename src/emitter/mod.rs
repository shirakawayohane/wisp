pub mod encoder;
mod expression;
mod function;
mod intrinsic_ops;
mod special_forms;
mod global;
mod vector;

pub use encoder::compile_into_wasm;

use crate::{
    env::{Env, Pointer, Variable},
    parser::{parse_source, TypeAST, AST},
    resolver::{get_primitive_types, resolve_type, Type, TypeEnv},
};

use anyhow::{anyhow, bail, Result};
use std::{cell::RefCell, collections::HashMap, fmt::Display, hash::Hash, rc::Rc};

use self::{function::*, special_forms::*, global::*};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportKind {
    Func,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub enum WasmPrimitiveType {
    I32 = 0x7F,
    F32 = 0x7D,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Export {
    pub export_type: ExportKind,
    pub name: String,
    pub func_index: u32,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub enum SignatureType {
    Func = 0x60,
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct Signature {
    pub sig_type: SignatureType,
    pub params: Vec<WasmPrimitiveType>,
    pub results: Vec<WasmPrimitiveType>,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub signature_index: u32,
    pub arg_types: Vec<Rc<Type>>,
    pub result_type: Rc<Type>,
    pub body: Vec<OpCode>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum GlobalValue {
    I32(i32),
    F32(f32),
}

#[derive(Debug, PartialEq)]
pub struct Global {
    is_mutable: bool,
    value: GlobalValue,
}

#[derive(PartialEq, Debug)]
pub enum OpCode {
    If(Option<WasmPrimitiveType>),
    Else,
    Drop,
    End,
    LocalGet(u32),
    LocalSet(u32),
    LocalTee(u32),
    GlobalGet(u32),
    #[allow(dead_code)]
    GlobalSet(u32),
    LocalDecl(WasmPrimitiveType),
    Call(u32),
    I32Store { offset: u32, alignment: u32 },
    I32Store8 { offset: u32, alignment: u32 },
    I32Load { offset: u32, alignment: u32 },
    I32Load8U { offset: u32, alignment: u32 },
    F32Store { offset: u32, alignment: u32 },
    F32Load { offset: u32, alignment: u32 },
    I32Const(i32),
    F32Const(f32),
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32Xor,
    I32Eq,
    I32GtS,
    I32GeS,
    I32And,
    I32Or,
    I32LtS,
    I32LeS,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Eq,
    F32Gt,
    F32Ge,
    F32Lt,
    F32Le,
    F32Neg,
    F32ConvertI32S,
}

#[derive(Debug, Default)]
pub struct Module {
    pub signatures: HashMap<Signature, u16>,
    pub exports: Vec<Export>,
    pub functions: Rc<RefCell<HashMap<String, (u32, Function)>>>,
    pub globals: Rc<RefCell<HashMap<String, (u32, Global)>>>,
}

#[derive(Debug, PartialEq, Eq)]
enum IntrinsicOperator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Gt,
    Ge,
    Lt,
    Le,
    And,
    Or,
    Not,
}

impl Display for IntrinsicOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                IntrinsicOperator::Add => "+",
                IntrinsicOperator::Sub => "-",
                IntrinsicOperator::Mul => "*",
                IntrinsicOperator::Div => "/",
                IntrinsicOperator::Eq => "=",
                IntrinsicOperator::Gt => ">",
                IntrinsicOperator::Ge => ">=",
                IntrinsicOperator::Lt => "<",
                IntrinsicOperator::Le => "<=",
                IntrinsicOperator::And => "and",
                IntrinsicOperator::Or => "or",
                IntrinsicOperator::Not => "not",
            }
        )
    }
}

fn emit_toplevel(module: &mut Module, ast: &AST, env: Rc<RefCell<Env>>) -> Result<()> {
    match ast {
        AST::List(list) => match list.first().unwrap() {
            AST::Symbol(s) => match *s {
                "defn" | "export" => emit_func(module, ast, env),
                "define" => emit_global(module, &list[1..], false, env),
                "defmut" => emit_global(module, &list[1..], true, env),
                _ => bail!("Top level form must be function decl or global variable, found {:?}", s),
            },
            _ => bail!("Top level form must be function decl or global variable, found {:?}", ast),
        },
        _ => bail!("Toplevel form must be a list"),
    }
}

const STACK_POINTER: (u32, Global) = (0, Global {
    is_mutable: true,
    value: GlobalValue::I32(1048576)
});

const HEAP_BASE: (u32, Global) = (0, Global {
    is_mutable: false,
    value: GlobalValue::I32(2097152)
});

fn emit_builtin_vars(module: &mut Module) -> Result<()> {
    module.globals.borrow_mut().insert("__stack_pointer".to_string(),  STACK_POINTER);
    module.globals.borrow_mut().insert("__heap_base".to_string(),  HEAP_BASE);
    Ok(())
}

fn emit_module(module: &mut Module, ast: &AST) -> Result<()> {
    let env = Env::create();
    emit_builtin_vars(module)?;
    let toplevels = match ast {
        AST::Module(tops) => tops,
        _ => return Err(anyhow!("Invalid argument.")),
    };
    for toplevel in toplevels {
        emit_toplevel(module, toplevel, env.clone())?;
    }
    Ok(())
}

pub fn emit(module: &mut Module, source: &str) -> Result<()> {
    let module_ast = parse_source(source)?;
    emit_module(module, &module_ast)
}
