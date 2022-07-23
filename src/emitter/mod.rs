mod intrinsic_ops;
mod special_forms;
mod function;
mod expression;
pub mod encoder;

pub use encoder::compile_into_wasm;

use crate::{
    env::{Env, Pointer, Variable},
    parser::{parse_source, TypeAST, AST},
    resolver::{get_primitive_types, resolve_type, Type, TypeEnv},
};

use anyhow::{anyhow, Result};
use std::{cell::RefCell, collections::HashMap, fmt::Display, hash::Hash, rc::Rc};

use self::{special_forms::{emit_scope}, function::emit_func};

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

#[derive(PartialEq, Debug)]
pub enum OpCode {
    If(Option<WasmPrimitiveType>),
    Else,
    Drop,
    End,
    LocalGet(u8),
    LocalSet(u8),
    LocalDecl(WasmPrimitiveType),
    Call(u32),
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

#[derive(Debug, PartialEq, Default)]
pub struct Module {
    pub signatures: HashMap<Signature, u16>,
    pub exports: Vec<Export>,
    pub functions: Rc<RefCell<HashMap<String, (usize, Function)>>>,
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

fn emit_toplevel(module: &mut Module, ast: &AST) -> Result<()> {
    // TODO: Impl Global Variables
    // toplevel can only be a function for now.
    emit_func(module, ast, Rc::new(RefCell::new(Env::default())))
}
fn emit_module(module: &mut Module, ast: &AST) -> Result<()> {
    let toplevels = match ast {
        AST::Module(tops) => tops,
        _ => return Err(anyhow!("Invalid argument.")),
    };
    for toplevel in toplevels {
        emit_toplevel(module, toplevel)?;
    }
    Ok(())
}
pub fn emit(module: &mut Module, source: &str) -> Result<()> {
    let module_ast = parse_source(source)?;
    emit_module(module, &module_ast)
}
