use std::io::{Result as IoResult, Write};

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

pub enum Valtype {
    I32 = 0x7f,
    F32 = 0x7d,
}

pub type EmitResult = IoResult<usize>;

pub fn emit_wasm_magic_number<W: Write>(writer: &mut W) -> EmitResult {
    writer.write(&[0x00, 0x61, 0x73, 0x6d])
}

pub fn emit_wasm_module_version<W: Write>(writer: &mut W) -> EmitResult {
    writer.write(&[0x01_u8, 0x00_u8, 0x00_u8, 0x00_u8])
}

#[test]
pub fn test_emit_f32() {
    let mut buf = Vec::new();
    emit_f32(&mut buf, 3.14).unwrap();
    assert_eq!(buf, vec![0xc3, 0xf5, 0x48, 0x40]);
}

pub fn emit_f32<W: Write>(writer: &mut W, value: f32) -> EmitResult {
    writer.write(&value.to_le_bytes())
}

#[test]
pub fn test_emit_u32() {
    let mut buf = Vec::new();
    emit_u32(&mut buf, 123456).unwrap();
    assert_eq!(buf, [0xC0, 0xC4, 0x07]);
}

pub fn emit_u32<W: Write>(writer: &mut W, val: u32) -> EmitResult {
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
pub fn test_emit_name() {
    let mut buf = Vec::new();
    emit_name(&mut buf, "addTwo").unwrap();
    assert_eq!(buf, &[0x06, 0x61, 0x64, 0x64, 0x54, 0x77, 0x6f]);
}

pub fn emit_name<'a>(writer: &mut impl Write, name: &'a str) -> EmitResult {
    let bytes = name.as_bytes();
    let len = bytes.len();
    let string_length_length = emit_u32(writer, len as u32)?;
    let string_length = writer.write(bytes)?;
    EmitResult::Ok(string_length_length + string_length)
}

#[test]
fn emit_module_test() {
    let mut buf = Vec::new();
    emit_module(&mut buf).unwrap();
    // assert_eq!(
    //     buf,
    //     vec![
    //         0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 8, 104, 111, 103, 101, 104, 111, 103,
    //         101
    //     ]
    // );
    // TODO: まともなテストを書く
}

pub fn emit_module(writer: &mut impl Write) -> EmitResult {
    emit_wasm_magic_number(writer)?;
    emit_wasm_module_version(writer)?;
    emit_name(writer, "hogehoge")
}
