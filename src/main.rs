use anyhow::{ensure, Result};
use std::{fs::File, io::{BufWriter}, path::{Path, PathBuf}};

use crate::emitter::compile_into_wasm;

mod lexer;
mod parser;
mod env;
mod emitter;
mod resolver;
mod compiler;

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    ensure!(args.len() > 1, "wispc needs 1 or more args.");
    let source_path = Path::new(&args[1]);
    let target_path = if args.len() > 2 {
        PathBuf::from(&args[2])
    } else {
        Path::new(&args[1]).with_extension("wasm")
    };
    let source = std::fs::read_to_string(&source_path)?;
    let target_file = File::create(&target_path)?;
    let mut writer = BufWriter::new(target_file);
    compile_into_wasm(&mut writer, &source)?;
    Ok(())
}