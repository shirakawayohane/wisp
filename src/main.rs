use anyhow::{ensure, Result};
use emitter::Emitter;
use std::{env, fs::File, io::{BufWriter}, path::{Path, PathBuf}};

mod emitter;
mod lexer;
mod parser;

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    ensure!(args.len() > 0, "wispc needs 1 or more args.");
    let source_path = Path::new(&args[0]);
    let target_path = if args.len() > 1 {
        PathBuf::from(&args[1])
    } else {
        Path::new(&args[0]).with_extension("hoge")
    };
    let source = std::fs::read_to_string(&source_path)?;
    let target_file = File::open(&target_path)?;
    let mut emitter = Emitter::new(BufWriter::new(target_file));
    emitter.emit(&source)?;
    Ok(())
}
