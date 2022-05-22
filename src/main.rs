use std::{
    fs::{self, File},
    io::{BufWriter, Write}, collections::HashMap,
};

use parser::parse_module;
use tokenizer::tokenize;

mod emitter;
mod encoder;
mod parser;
mod pos;
mod tokenizer;
mod transformer;
mod opcodes;

#[derive(Debug)]
enum CompileError {
    TokenizeError,
    ParseError,
    EmitError,
}

struct Hoge {
    value: i32
}

fn compile<W: Write>(source: &str, writer: &mut W) -> Result<(), CompileError> {
    let tokens = tokenize(source).unwrap();
    let ast = parse_module(tokens.as_slice()).unwrap();
    todo!();
}

fn main() {
    let source = fs::read_to_string("./sample.wisp").unwrap();
    let mut writer = BufWriter::new(File::create("./sample.wasm").unwrap());
    compile(&source, &mut writer).unwrap();
}
