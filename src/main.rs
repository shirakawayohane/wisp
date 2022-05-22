#![feature(generators, generator_trait)]
#![feature(conservative_impl_trait)]

use std::{
    fmt::Binary,
    fs::{self, File},
    io::{BufRead, BufWriter, Write},
};

use emitter::EmitResult;
use parser::parse_module;
use tokenizer::tokenize;

mod emitter;
mod encoder;
mod parser;
mod pos;
mod tokenizer;
mod transformer;

#[derive(Debug)]
enum CompileError {
    TokenizeError,
    ParseError,
    EmitError,
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
