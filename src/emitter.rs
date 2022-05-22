use crate::{
    encoder::write_u32,
    transformer::{Expression, Function, Variable, SemanticModel, Type}, opcodes::Opcode,
};
use std::{
    collections::{vec_deque, HashMap, VecDeque},
    hash::Hash, io::{Write, BufWriter}
};
use anyhow::{Result, Context};
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

#[test]
fn valtype_bytemucking_test() {
    let mut valtypes = vec![Valtype::I32, Valtype::I64, Valtype::F32, Valtype::F64];
    let num_types = valtypes.len();
    unsafe {
        let bytes = &valtypes as *const _ as *const u8;
        let slice = std::slice::from_raw_parts(bytes, num_types);
    }
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
fn desolve_type(type_syntax: &Type) -> Vec<Valtype> {
    match type_syntax {
        Type::F32 => vec![Valtype::F32],
    }
}
fn get_func_type(func: &Function) -> FuncType {
    FuncType {
        param_types: func
            .params
            .iter()
            .flat_map(|param| desolve_type(&param.ty))
            .collect::<Vec<_>>(),
        result_types: desolve_type(&func.return_type),
    }
}
// e.g (func (param f32 f32) (result i32)) => func 2(num params) i32 i32 1(num_results) i32
fn emit_function_type(w: &mut impl Write, func_type: &FuncType) -> Result<()> {
    // (func
    w.write(&[FunctionType])?;
    let num_params = func_type.param_types.len();
    // (param
    write_u32(w, num_params as u32)?;
    unsafe {
        let bytes = &func_type.param_types as *const _ as *const u8;
        let slice = std::slice::from_raw_parts(bytes, num_params);
        w.write(slice)?;
    } // (param.... )
    // (result
    let num_results = func_type.result_types.len();
    write_u32(w, num_results as u32)?;
    unsafe {
        let bytes = &func_type.result_types as *const _ as *const u8;
        let slice = std::slice::from_raw_parts(bytes, num_results);
        w.write(slice)?;
    }
    Ok(())
}

struct FrameItem {
    ty: Type,
    desolved: Vec<Valtype>,
    local_idx: u32,
}
type Frame = HashMap<String, FrameItem>;

fn get_frame_from_params(params: &Vec<Variable>, current_idx: u32) -> Frame {
    // params
    //     .iter()
    //     .map(|param| {
    //         (
    //             param.name,
    //             (
    //                 frames
    //                     .iter()
    //                     .map(|x| x.len())
    //                     .reduce(|a, b| a + b)
    //                     .unwrap_or(0),
    //                 param.ty,
    //             ),
    //         )
    //     })
    //     .collect()
    todo!()
}

fn emit_expression(w: &mut impl Write, frames: &mut VecDeque<Frame>, locals: &mut Vec<Valtype>, expr: &Expression)
 -> Result<()> {
    match expr {
        Expression::LetScope(params, exprs) => {
            for param in params {
                locals.append(&mut desolve_type(&param.ty));
            }
            frames.push_front(get_frame_from_params(params, ));
            w.write(&[Opcode::Block as u8]);
            for expr in exprs {
                emit_expression(w, frames, locals, expr)?;
            }
            Ok(())
        },
        Expression::SymbolReference(v) => {
            // スタックにおけるインデックスとプリミティブ型がわかればよい。
            // プリミティブの場合はLoad。
            match v.ty {

            }
        },
        Expression::FunctionCall(func, args) => {
            for arg in args {
                emit_expression(w, frames, locals, arg)?;
            }
            w.write(&[Opcode::Call as u8])?;
            write_u32(w, func.index);
            Ok(())
        },
        _ => todo!(),
    }
}

fn emit_function_body(w: &mut impl Write, frames: &mut VecDeque<Frame>, func_table: &HashMap<String, u32>, func: &Function) -> Result<()> {
    let mut func_body = Vec::new();
    let mut locals = Vec::<Valtype>::new();
    for expr in func.exprs {
        emit_expression(&mut func_body, frames, &mut locals, &expr)?;
    }
    Ok(())
}

fn emit_module<W: Write>(w: &mut BufWriter<W>, model: &SemanticModel) -> Result<()> {
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

    w.write(&WasmMagicNumber)?;
    w.write(&WasmModuleVersion)?;

    // Type section
    {
        let mut type_section = Vec::new();
        for t in func_types.keys() {
            emit_function_type(&mut type_section, t)?;
        }
        w.write(&[Section::Type as u8])?;
        write_u32(w, type_section.len() as u32)?;
        w.write(&type_section)?;
        w.flush()?;
    }

    let num_functions = func_type_idxs.len() as u32;
    { // Func Section
        let mut func_section = Vec::new();
        // num functions
        write_u32(&mut func_section, num_functions)?;
        for idx in func_type_idxs {
            write_u32(&mut func_section, idx)?;
        }
        w.write(&func_section)?;
        w.flush()?;
    }
    { // Code Section
        let mut code_section = Vec::new();
        write_u32(&mut code_section, num_functions)?;
        for func in &model.functions {
            let frames = Vec::new();
            emit_function_body(&mut code_section, &mut frames, func_table, func)?;
        }
        write_u32(w, code_section.len() as u32)?;
        w.write(&code_section);
        w.flush();
    }
    Ok(())
}
