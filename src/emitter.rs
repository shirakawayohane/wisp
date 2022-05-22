use crate::{
    encoder::write_u32,
    parser::{Type, TypeSyntax},
    transformer::{Expression, Function, Param, SemanticModel},
};
use anyhow::{anyhow, Result};
use std::{
    collections::{vec_deque, HashMap, VecDeque},
    fmt::write,
    hash::Hash,
    intrinsics::unreachable,
    io::Write,
    ops::Index,
};
use thiserror::Error;

#[allow(dead_code)]
#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub enum Section {
    Custom = 0,
    Type = 1,
    Import = 2,
    Func = 3,
    Table = 4,
    Memory = 5,
    Global = 6,
    Export = 7,
    Start = 8,
    Element = 9,
    Code = 10,
    Data = 11,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum Valtype {
    I32 = 0x7f,
    I64 = 0x7e,
    F32 = 0x7d,
    F64 = 0x7c,
    Unit = 0x40,
}

enum Opcodes {
    Block = 0x02,
    Loop = 0x03,
    Br = 0x0c,
    BrIf = 0x0d,
    End = 0x0b,
    Call = 0x10,
    GetLocal = 0x20,
    SetLocal = 0x21,
    I32Store8 = 0x3a,
    I32Const = 0x41,
    I64Const = 0x42,
    F32Const = 0x43,
    F64Const = 0x44,
    I32Eqz = 0x45,
    I32Eq = 0x46,
    I32Ne = 0x47,
    I32LtS = 0x48,
    I32LtU = 0x49,
    I32GtS = 0x4A,
    I32GtU = 0x4B,
    I32LeS = 0x4C,
    I32LeU = 0x4D,
    I32GeS = 0x4E,
    I32GeU = 0x4F,
    I64Eqz = 0x50,
    I64Eq = 0x51,
    I64Ne = 0x52,
    I64LtS = 0x53,
    I64LtU = 0x54,
    I64GtS = 0x55,
    I64GtU = 0x56,
    I64LeS = 0x57,
    I64GeU = 0x58,
    F32Eq = 0x5b,
    F32Lt = 0x5d,
    F32Gt = 0x5e,
    I32And = 0x71,
    F32Add = 0x92,
    F32Sub = 0x93,
    F32Mul = 0x94,
    F32Div = 0x95,
    I32TruncF32S = 0xa8
}

const FunctionType: u8 = 0x60;

#[derive(Error, Debug)]
pub enum EmitError {
    #[error("Emitting code failed due to io error: {0}")]
    IoError(std::io::Error),
    #[error(transparent)]
    Other(#[from] anyhow::Error), // source and Display delegate to anyhow::Error
}

impl From<std::io::Error> for EmitError {
    fn from(io_error: std::io::Error) -> Self {
        EmitError::IoError(io_error)
    }
}

pub type EmitResult = anyhow::Result<usize>;

const WasmMagicNumber: [u8; 4] = [0x00, 0x61, 0x73, 0x6d];

const WasmModuleVersion: [u8; 4] = [0x01_u8, 0x00_u8, 0x00_u8, 0x00_u8];

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
struct FuncType {
    param_types: Vec<Valtype>,
    result_types: Vec<Valtype>,
}

#[inline]
fn desolve_type(type_syntax: &TypeSyntax) -> Vec<Valtype> {
    match type_syntax {
        TypeSyntax::F32 => vec![Valtype::F32],
    }
}
fn get_func_type(func: &Function) -> FuncType {
    FuncType {
        param_types: func
            .params
            .iter()
            .flat_map(|param| desolve_type(&param.type_syntax))
            .collect::<Vec<_>>(),
        result_types: desolve_type(&func.return_type),
    }
}
// e.g (func (param f32 f32) (result i32)) => func 2(num params) i32 i32 1(num_results) i32
fn emit_function_type(func_type: &FuncType) -> Vec<u8> {
    [
        vec![FunctionType],
        write_u32(func_type.param_types.len() as u32),
        func_type.param_types.iter().map(|t| (*t) as u8).collect(),
        write_u32(func_type.result_types.len() as u32),
        func_type.result_types.iter().map(|t| (*t) as u8).collect(),
    ]
    .concat()
}

type Frame = HashMap<String, (usize, TypeSyntax)>;

fn emit_block(Block type)

fn emit_expression(frames: &mut VecDeque<Frame>, expr: &Expression) -> Vec<u8> {
    match expr {
        Expression::LetScope(params, child_exprs) => {
            frames.push_front(
                params
                    .iter()
                    .map(|param| {
                        (
                            param.name,
                            (
                                frames
                                    .iter()
                                    .map(|x| x.len())
                                    .reduce(|a, b| a + b)
                                    .unwrap_or(0),
                                param.type_syntax,
                            ),
                        )
                    })
                    .collect(),
            );
            
            vec![]
        }
        Expression::SymbolReference(sym) => {
            vec![]
        }
        _ => todo!(),
    }
}

fn emit_function_body(frames: VecDeque<Frame>, func: &Function) -> Vec<u8> {
    let mut code = Vec::new();
    for expr in func.exprs {
        code.append(&mut emit_expression(frames, &expr))
    }
    code
}

fn emit_module(model: &SemanticModel) -> Vec<u8> {
    let mut func_types = HashMap::<FuncType, u32>::new();
    let mut func_type_idxs: Vec<u32> = Vec::new();
    for function in &model.functions {
        let func_type = get_func_type(&function);
        let next_index = func_types.len() as u32;
        let index = *func_types.entry(func_type).or_insert(next_index);
        if next_index == index {
            func_type_idxs.push(index);
        }
    }

    let mut type_section = Vec::new();
    // num types
    type_section.append(&mut write_u32(func_types.len() as u32));
    for t in func_types.keys() {
        type_section.append(&mut emit_function_type(t));
    }

    let num_functions = func_type_idxs.len() as u32;

    let mut func_section = Vec::new();
    // num functions
    func_section.append(&mut write_u32(num_functions));
    for idx in func_type_idxs {
        func_section.append(&mut write_u32(idx));
    }

    let mut code_section = Vec::new();
    // num functions
    code_section.append(&mut write_u32(num_functions));
    for func in &model.functions {
        code_section.append(&mut emit_function_body(func));
    }

    let mut module = [WasmMagicNumber, WasmModuleVersion].concat();
    module.push(Section::Type as u8);
    module.append(&mut write_u32(type_section.len() as u32));
    module.append(&mut type_section);
    module.push(Section::Func as u8);
    module.append(&mut write_u32(func_section.len() as u32));
    module.append(&mut func_section);
    module.push(Section::Code as u8);
    module.append(&mut write_u32(code_section.len() as u32));
    module.append(&mut code_section);

    module
}
