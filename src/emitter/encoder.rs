use super::*;
use std::io::Write;

pub fn encode_leb128<N: Into<u64>>(writer: &mut impl Write, n: N) -> Result<usize, std::io::Error> {
    leb128::write::unsigned(writer, n.into())
}
pub fn encode_s_leb128<N: Into<i64>>(
    writer: &mut impl Write,
    n: N,
) -> Result<usize, std::io::Error> {
    leb128::write::signed(writer, n.into())
}

pub fn encode_string(writer: &mut impl Write, name: &str) -> Result<usize, std::io::Error> {
    let bytes = name.as_bytes();
    let size_len = encode_leb128(writer, bytes.len() as u64)?;
    writer.write(bytes)?;
    Ok(bytes.len() + size_len)
}

use crate::emitter::{
    emit, Export, ExportKind, Function, Module, OpCode, Signature, WasmPrimitiveType,
};
use anyhow::Result;
use std::io::BufWriter;

fn encode_global(writer: &mut impl Write, global: &Global) -> Result<()> {
    let primitive_type = match global.value {
        GlobalValue::I32(_) => WasmPrimitiveType::I32,
        GlobalValue::F32(_) => WasmPrimitiveType::F32,
    };
    writer.write(&[primitive_type as u8, if global.is_mutable { 1 } else { 0 }])?;
    match global.value {
        GlobalValue::I32(v) => {
            encode_leb128(writer, v as u64)?;
        }
        GlobalValue::F32(v) => {
            writer.write(&v.to_le_bytes())?;
        }
    }
    writer.write(&[0x0B])?; // end
    Ok(())
}

fn encode_global_section(writer: &mut impl Write, globals: &[&Global]) -> Result<()> {
    writer.write(&[0x06])?;
    let global_section = &mut Vec::new();
    encode_leb128(global_section, globals.len() as u64)?;
    for global in globals {
        encode_global(writer, global)?;
    }
    encode_leb128(writer, global_section.len() as u64)?;
    writer.write(global_section)?;
    Ok(())
}

fn write_signature<W: Write>(writer: &mut W, signature: &Signature) -> Result<()> {
    // signature type
    writer.write(&[signature.sig_type as u8])?;
    // num params
    writer.write(&[signature.params.len() as u8])?;
    // params
    writer.write(
        &signature
            .params
            .iter()
            .map(|p| *p as u8)
            .collect::<Vec<_>>()[..],
    )?;
    // num results
    writer.write(&[signature.results.len() as u8])?;
    // results
    writer.write(
        &signature
            .results
            .iter()
            .map(|p| *p as u8)
            .collect::<Vec<_>>()[..],
    )?;
    Ok(())
}

fn write_export(writer: &mut impl Write, export: &Export) -> Result<()> {
    encode_string(writer, &export.name)?;
    writer.write(&[match export.export_type {
        ExportKind::Func => 0x00,
    }])?;
    encode_leb128(writer, export.func_index)?;
    Ok(())
}

fn write_function_body(writer: &mut impl Write, func: &Function) -> Result<()> {
    let mut i32_locals: u8 = 0;
    let mut f32_locals: u8 = 0;
    let mut opcodes = Vec::new();
    for opcode in &func.body {
        match opcode {
            OpCode::LocalDecl(v) => match v {
                WasmPrimitiveType::I32 => {
                    i32_locals += 1;
                }
                WasmPrimitiveType::F32 => {
                    f32_locals += 1;
                }
            },
            _ => {
                opcodes.push(opcode);
            }
        }
    }
    // First, bundle local decls.
    let local_decl_count = [i32_locals > 0, f32_locals > 0]
        .iter()
        .filter(|x| **x)
        .count();
    encode_leb128(writer, local_decl_count as u64)?;
    if i32_locals > 0 {
        encode_leb128(writer, i32_locals)?; // local type count
        writer.write(&[0x7f])?; // i32
    }
    if f32_locals > 0 {
        encode_leb128(writer, f32_locals)?; // local type count
        writer.write(&[0x7d])?;
    }

    for opcode in &opcodes {
        match opcode {
            OpCode::LocalDecl(_) => unreachable!(),
            OpCode::LocalGet(n) => {
                writer.write(&[0x20])?;
                encode_leb128(writer, *n)?;
            }
            OpCode::LocalSet(index) => {
                writer.write(&[0x21])?;
                encode_leb128(writer, *index)?;
            }
            OpCode::GlobalGet(n) => {
                writer.write(&[0x23])?;
                encode_leb128(writer, *n)?;
            }
            OpCode::GlobalSet(index) => {
                writer.write(&[0x24])?;
                encode_leb128(writer, *index)?;
            }
            OpCode::F32Const(n) => {
                writer.write(&[0x43])?;
                writer.write(&n.to_le_bytes())?;
            }
            OpCode::I32Const(n) => {
                writer.write(&[0x41])?;
                encode_s_leb128(writer, *n)?;
            }
            OpCode::Call(index) => {
                writer.write(&[0x10])?;
                encode_leb128(writer, *index)?;
            }
            OpCode::If(primitive_type) => {
                writer.write(&[
                    0x04,
                    match *primitive_type {
                        Some(pt) => pt as u8,
                        None => 0x40,
                    },
                ])?;
            }
            _ => {
                writer.write(&[match opcode {
                    OpCode::If(_)
                    | OpCode::F32Const(_)
                    | OpCode::I32Const(_)
                    | OpCode::LocalGet(_)
                    | OpCode::LocalSet(_)
                    | OpCode::GlobalGet(_)
                    | OpCode::GlobalSet(_)
                    | OpCode::Call(_)
                    | OpCode::LocalDecl(_) => unreachable!(),
                    OpCode::Else => 0x05,
                    OpCode::Drop => 0x1A,
                    OpCode::End => 0x0B,
                    OpCode::I32Eq => 0x46,
                    OpCode::I32LtS => 0x48,
                    OpCode::I32GtS => 0x4A,
                    OpCode::I32LeS => 0x4C,
                    OpCode::I32GeS => 0x4E,
                    OpCode::F32Eq => 0x5B,
                    OpCode::F32Gt => 0x5E,
                    OpCode::F32Ge => 0x60,
                    OpCode::F32Lt => 0x5D,
                    OpCode::F32Le => 0x5F,
                    OpCode::I32Add => 0x6A,
                    OpCode::I32Sub => 0x6B,
                    OpCode::I32Mul => 0x6C,
                    OpCode::I32DivS => 0x6D,
                    OpCode::I32And => 0x71,
                    OpCode::I32Or => 0x72,
                    OpCode::I32Xor => 0x73,
                    OpCode::F32Neg => 0x8C,
                    OpCode::F32Add => 0x92,
                    OpCode::F32Sub => 0x93,
                    OpCode::F32Mul => 0x94,
                    OpCode::F32Div => 0x95,
                    OpCode::F32ConvertI32S => 0xB2,
                }])?;
            }
        }
    }
    Ok(())
}

fn write_type_section(writer: &mut impl Write, signatures: Vec<&Signature>) -> Result<()> {
    writer.write(&[0x01])?; // section Type: 1
    let mut type_section = Vec::new();
    // write num type signatures
    encode_leb128(&mut type_section, signatures.len() as u64)?;
    for signature in signatures {
        write_signature(&mut type_section, &signature)?;
    }
    encode_leb128(writer, type_section.len() as u64)?; // section size
    writer.write(&type_section[..])?;
    writer.flush()?;
    Ok(())
}

fn write_function_section(writer: &mut impl Write, functions: &Vec<&Function>) -> Result<()> {
    writer.write(&[0x03])?; // section function: 3
    let mut func_section = Vec::new();
    let num_functions = functions.len();
    encode_leb128(&mut func_section, num_functions as u64)?;
    for func in functions {
        encode_leb128(&mut func_section, func.signature_index)?;
    }
    let section_size = func_section.len();
    encode_leb128(writer, section_size as u64)?;
    writer.write(&func_section)?;
    Ok(())
}

fn write_export_section(writer: &mut impl Write, exports: &Vec<&Export>) -> Result<()> {
    writer.write(&[0x07])?; // section function: 7
    let mut export_section = Vec::new();
    let num_exports = exports.len();
    encode_leb128(&mut export_section, num_exports as u64)?;
    for export in exports {
        write_export(&mut export_section, &export)?;
    }
    let section_size = export_section.len();
    encode_leb128(writer, section_size as u64)?;
    writer.write(&export_section)?;
    Ok(())
}

fn write_code_section(writer: &mut impl Write, functions: &Vec<&Function>) -> Result<()> {
    writer.write(&[0x0A])?; // section code: 10
    let mut code_section = Vec::new();
    let num_functions = functions.len();
    encode_leb128(&mut code_section, num_functions as u64)?;
    for func in functions {
        let mut func_body_bytes = Vec::new();
        write_function_body(&mut func_body_bytes, func)?;
        // write func body size
        encode_leb128(&mut code_section, func_body_bytes.len() as u64)?;
        code_section.write(&func_body_bytes)?;
    }
    let section_size = code_section.len();
    encode_leb128(writer, section_size as u64)?;
    writer.write(&code_section)?;
    Ok(())
}

pub fn compile_into_wasm<W: Write>(writer: &mut BufWriter<W>, source: &str) -> Result<()> {
    let module = &mut Module::default();
    emit(module, source).unwrap();

    let mut signatures_with_index = module.signatures.iter().collect::<Vec<_>>();
    signatures_with_index.sort_by(|a, b| a.1.partial_cmp(b.1).unwrap());
    let signatures = signatures_with_index
        .into_iter()
        .map(|x| x.0)
        .collect::<Vec<_>>();
    let module_funcs = module.functions.borrow();
    let mut functions_with_index = module_funcs.iter().map(|x| x.1).collect::<Vec<_>>();
    functions_with_index.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
    let functions = functions_with_index
        .iter()
        .map(|x| &x.1)
        .collect::<Vec<_>>();

    writer.write(&[0x00, 0x61, 0x73, 0x6d])?; // WASM magic number
    writer.write(&[0x01, 0x00, 0x00, 0x00])?; // WASM binary version

    write_type_section(writer, signatures)?;
    writer.flush()?;

    write_function_section(writer, &functions)?;
    writer.flush()?;

    let module_globals = module.globals.borrow();
    let mut globals_with_index = module_globals.iter().map(|x| x.1).collect::<Vec<_>>();
    globals_with_index.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
    let globals = globals_with_index.iter().map(|x| &x.1).collect::<Vec<_>>();
    encode_global_section(writer, &globals)?;

    write_export_section(
        writer,
        &module.exports.iter().map(|x| x).collect::<Vec<_>>(),
    )?;
    writer.flush()?;

    write_code_section(writer, &functions)?;
    writer.flush()?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_signed() {
        let mut buf = Vec::new();
        encode_leb128(&mut buf, 0 as u8).unwrap();
        assert_eq!(buf, vec![0x00]);

        buf.clear();
        encode_leb128(&mut buf, 1 as u32).unwrap();
        assert_eq!(buf, vec![0x01]);

        buf.clear();
        encode_leb128(&mut buf, 63 as u64).unwrap();
        assert_eq!(buf, vec![0x3f]);

        buf.clear();
        encode_s_leb128(&mut buf, 64 as u8).unwrap();
        assert_eq!(buf, vec![0xc0, 0x00]);

        buf.clear();
        encode_s_leb128(&mut buf, 8191 as u32).unwrap();
        assert_eq!(buf, vec![0xff, 0x3f]);

        buf.clear();
        encode_s_leb128(&mut buf, 8192 as u32).unwrap();
        assert_eq!(buf, vec![0x80, 0xc0, 0x00]);
    }
    #[test]
    fn test_unsigned() {
        let mut buf = Vec::new();
        encode_leb128(&mut buf, 0 as u8).unwrap();
        assert_eq!(buf, vec![0x00]);

        buf.clear();
        encode_leb128(&mut buf, 1 as u32).unwrap();
        assert_eq!(buf, vec![0x01]);

        buf.clear();
        encode_leb128(&mut buf, 63 as u64).unwrap();
        assert_eq!(buf, vec![0x3f]);

        buf.clear();
        encode_leb128(&mut buf, 64 as u8).unwrap();
        assert_eq!(buf, vec![0x40]);

        buf.clear();
        encode_leb128(&mut buf, 8191 as u32).unwrap();
        assert_eq!(buf, vec![0xff, 0x3f]);

        buf.clear();
        encode_leb128(&mut buf, 8192 as u32).unwrap();
        assert_eq!(buf, vec![0x80, 0x40]);
    }
    #[test]
    fn test_string() {
        let mut buf = Vec::new();
        encode_string(&mut buf, "").unwrap();
        assert_eq!(buf, vec![0x00]);

        buf.clear();
        encode_string(&mut buf, "abc").unwrap();
        assert_eq!(buf, vec![0x03, 0x61, 0x62, 0x63]);
    }
    #[test]
    fn test_bin_ops() {
        let mut buf = Vec::<u8>::new();
        unsafe {
            let mut writer = BufWriter::new((&mut buf as *mut Vec<_>).as_mut().unwrap());
            compile_into_wasm(
                &mut writer,
                "(defn calc : f32
                [a : f32 b : i32]
                  (* 10 (/ (+ a (- b 1)) 2))",
            )
            .unwrap();
        }
        assert_eq!(
            buf,
            vec![
                0x00, 0x61, 0x73, 0x6d, // wasm header
                0x01, 0x00, 0x00, 0x00, // wasm binary version
                0x01, // type section
                0x07, // section size
                0x01, // num types
                0x60, // type func
                0x02, // num params
                0x7D, // f32
                0x7F, // i32
                0x01, // num results
                0x7D, // f32
                0x03, // function section
                0x02, // section size
                0x01, // num funcs
                0x00, // signature index
                0x06, // global section
                0x01, // section size
                0x00, // num globals
                0x07, // export section
                0x01, // section size,
                0x00, // num exports
                0x0A, // code section
                0x15, // section size
                0x01, // num functions
                0x13, // func body size
                0x00, // local decl count
                0x41, 0x0A, // i32 const 10
                0xB2, // f32_convert_i32_s
                0x20, 0x00, // local.get 0
                0x20, 0x01, // local.get 1,
                0x41, 0x01, // i32.const 1
                0x6B, // i32.sub
                0xB2, // f32_convert_i32_s
                0x92, // f32.add
                0x41, 0x02, // i32.const 2
                0xB2, // f32_convert_i32_s
                0x95, // f32.div
                0x94, // f32.mul
                0x0B, // END
            ]
        );
    }
}
