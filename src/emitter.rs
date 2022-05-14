use crate::{parser::TypeSyntax, transformer::Function};
use anyhow::{anyhow, Context, Result};
use bytemuck::{Pod, Zeroable};
use std::{
    collections::{binary_heap::Iter, HashMap},
    hash::Hash,
    io::Write,
    ops::Generator,
    result,
};
use thiserror::Error;

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

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Valtype {
    I32 = 0x7f,
    I64 = 0x7e,
    F32 = 0x7d,
    F64 = 0x7c,
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

pub fn write_wasm_magic_number(writer: &mut impl Write) -> EmitResult {
    Ok(writer.write(&[0x00, 0x61, 0x73, 0x6d])?)
}

pub fn write_wasm_module_version(writer: &mut impl Write) -> EmitResult {
    Ok(writer.write(&[0x01_u8, 0x00_u8, 0x00_u8, 0x00_u8])?)
}

#[test]
pub fn test_write_f32() {
    let mut buf = Vec::new();
    write_f32(&mut buf, 3.14).unwrap();
    assert_eq!(buf, vec![0xc3, 0xf5, 0x48, 0x40]);
}

pub fn write_f32(writer: &mut impl Write, value: f32) -> Result<usize, std::io::Error> {
    writer.write(&value.to_le_bytes())
}

#[test]
pub fn test_write_u32() {
    let mut buf = Vec::new();
    write_u32(&mut buf, 123456).unwrap();
    assert_eq!(buf, [0xC0, 0xC4, 0x07]);
}

pub fn write_u32<W: Write>(writer: &mut W, val: u32) -> Result<usize, std::io::Error> {
    let mut n = val;
    let mut buf = [0_u8; 4];
    let mut written = 0;
    loop {
        let mut byte: u8 = (n & 0x7f) as u8;
        n >>= 7;
        if n != 0 {
            byte |= 0x80;
        }
        buf[written] = byte;
        written += 1;
        if n == 0 {
            return writer.write(&buf[..written]);
        }
    }
}

#[test]
pub fn test_write_name() {
    let mut buf = Vec::new();
    write_name(&mut buf, "addTwo").unwrap();
    assert_eq!(buf, &[0x06, 0x61, 0x64, 0x64, 0x54, 0x77, 0x6f]);
}

pub fn write_name<'a>(writer: &mut impl Write, name: &'a str) -> Result<usize, std::io::Error> {
    let bytes = name.as_bytes();
    let len = bytes.len();
    let string_length_length = write_u32(writer, len as u32)?;
    let string_length = writer.write(bytes)?;
    Ok(string_length_length + string_length)
}

pub trait Len {
    fn len(&self) -> usize;
}

impl<T> Len for Vec<T> {
    fn len(&self) -> usize {
        self.len()
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
struct FuncType {
    param_types: Vec<Valtype>,
    result_types: Vec<Valtype>,
}

pub struct Emitter<W: Write + Len> {
    writer_initializer: fn() -> W,
    sections: HashMap<Section, W>,
    func_map: HashMap<String, usize>,
    type_map: HashMap<String, usize>,
}

impl<W: Write + Len> Emitter<W> {
    pub fn new(writer_initializer: fn() -> W) -> Self {
        Emitter {
            writer_initializer,
            sections: HashMap::new(),
            func_map: HashMap::new(),
            type_map: HashMap::new(),
        }
    }
    fn section_writer(&mut self, section: Section) -> &mut W {
        self.sections
            .entry(section)
            .or_insert((self.writer_initializer)())
    }
    fn register_function(&mut self, name: String) -> Result<usize> {
        if self.func_map.get(&name).is_some() {
            return Err(anyhow!("function {} has already declared.", name));
        }
        let index = self.func_map.len();
        self.func_map.insert(name, index);
        Ok(index)
    }
    fn register_type(&mut self, name: String) -> Result<usize> {
        if self.type_map.get(&name).is_some() {
            return Err(anyhow!("type {} has already declared.", name));
        }
        let index = self.type_map.len();
        self.func_map.insert(name, index);
        Ok(index)
    }

    #[inline]
    fn desolve_type(&self, type_syntax: &TypeSyntax) -> Vec<Valtype> {
        match type_syntax {
            TypeSyntax::F32 => vec![Valtype::F32],
        }
    }
    fn get_func_type(&self, func: Function) -> FuncType {
        FuncType {
            param_types: func
                .params
                .iter()
                .flat_map(|param| self.desolve_type(&param.type_syntax))
                .collect::<Vec<_>>(),
            result_types: self.desolve_type(&func.return_type),
        }
    }
    // e.g (func (param f32 f32) (result i32)) => func 2(num params) i32 i32 1(num_results) i32
    pub fn emit_function_type(&mut self, func_type: FuncType) -> Result<()> {
        let section_writer = self.section_writer(Section::Type);
        write_u32(section_writer, func_type.param_types.len() as u32)?;
        section_writer.write(&func_type.param_types as &[u8])?;
        write_u32(section_writer, func_type.result_types.len() as u32)?;
        section_writer.write(&mut result_bytes)?;
        Ok(())
    }
    pub fn emit_function_body(&mut self, func: Function) -> Result<()> {
        todo!()
    }
    pub fn emit_function<'a>(&mut self, function: Function<'a>) -> Result<()> {
        self.emit_function_type(function);
        self.emit_function_body(function);
    }
}
