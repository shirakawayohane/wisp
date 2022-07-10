use crate::parser::{parse, AST};
use anyhow::{anyhow, bail, ensure, Result, Context};
use std::{cell::RefCell, collections::{HashMap}, io::Write, vec};

struct Sections {
    type_section: Vec<u8>,
    num_funcs: u32,
    func_section: Vec<u8>,
    code_section: Vec<u8>,
    // TODO: exports
    // num_exports: u32,
    // export_section: Vec<u8>,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub enum PrimitiveType {
    I32,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
enum SignatureType {
    Func = 0x60
}

#[derive(PartialEq, Eq, Debug, Hash)]
struct Signature {
    sig_type: SignatureType,
    params: Vec<PrimitiveType>,
    results: Vec<PrimitiveType>
}

#[derive(PartialEq, Eq, Debug)]
enum OpCode {
    I32Add = 0x6A,
    End = 0x0B,
}

const DEFN_KEYWORD: &str = "defn";

fn emit_signature<W: Write>(writer: &mut W, signature: &Signature) -> Result<()> {
    // signature type
    writer.write(&[signature.sig_type as u8])?;
    // num params
    writer.write(&[signature.params.len() as u8])?;
    // params
    writer.write(&signature.params.iter().map(|p| *p as u8).collect::<Vec<_>>()[..])?;
    // num results
    writer.write(&[signature.sig_type as u8])?;
    // results
    writer.write(&signature.results.iter().map(|r| *r as u8).collect::<Vec<_>>()[..])?;
    Ok(())
}

pub struct Emitter<W: Write> {
    writer: RefCell<W>,
    sections: Sections,
    type_signatures: HashMap<Signature, usize>
}

impl<'a, W: Write> Emitter<W> {
    pub fn new(writer: W) -> Self {
        Emitter {
            writer: RefCell::new(writer),
            sections: Sections {
                type_section: Vec::new(),
                num_funcs: 0,
                func_section: Vec::new(),
                code_section: Vec::new(),
            },
            type_signatures: HashMap::new()
        }
    }
    fn emit_add<W2: Write>(&self, writer: &mut W2, lhs: &AST, rhs: &AST) -> Result<()> {
        self.emit_obj(writer, lhs)?;
        self.emit_obj(writer, rhs)?; // rhs
        self.writer.borrow_mut().write(&[OpCode::I32Add as u8])?;
        Ok(())
    }
    fn emit_list<W2: Write>(&self, writer: &mut W2, ast: &AST) -> Result<()> {
        match ast {
            AST::List(list) => {
                ensure!(
                    list.len() == 3,
                    "Binary op can only be evaluated with 2 args"
                );
                match &list[0] {
                    AST::Add => {
                        self.emit_add(writer, &list[1], &list[2])?;
                    }
                    _ => todo!("Only + operator can be emitted for now."),
                }
            }
            _ => bail!("Invalid argument. emit_list only accepts AST::List"),
        }
        Ok(())
    }
    fn emit_obj<W2: Write>(&self, writer: &mut W2, ast: &AST) -> Result<()> {
        match ast {
            AST::List(_) => {
                self.emit_list(writer, ast)?;
            }
            AST::NumberLiteral(literal) => {
                let val = literal
                    .parse::<i32>().with_context(|| "Failed to parse number")?;
                self.writer.borrow_mut().write(&val.to_le_bytes())?;
            }
            _ => todo!("Implement let and emit_symbol"),
        }
        Ok(())
    }
    fn emit_func(&mut self, ast: &AST) -> Result<()> {
        if let AST::List(func_list) = ast {
            match &func_list[0] {
                AST::Symbol(s) => {
                    ensure!(
                        s == DEFN_KEYWORD,
                        "Function must be start with symbol 'defn'"
                    );
                }
                _ => bail!("Function must be start with symbol 'defn'"),
            }
            let mut args = Vec::new();
            match &func_list[1] {
                AST::List(list) => {
                    for arg in list {
                        if let AST::Symbol(arg) = arg {
                            args.push(arg);
                        } else {
                            bail!("Invalid syntax. Function arg must be symbol");
                        }
                    }
                }
                _ => bail!("Invalid syntax. Fnuction args must be a list."),
            };
            // TODO: local variables
            let mut func_body = Vec::new();
            let forms = &func_list[2..];
            for form in forms {
                self.emit_obj(&mut func_body, form)?;
            }
            func_body.push(OpCode::End as u8);

            let signature = Signature {
                sig_type: SignatureType::Func,
                params: args.iter().map(|_| PrimitiveType::I32).collect::<Vec<_>>(),
                // TODO: result can only be single I32 for now.
                results: vec![PrimitiveType::I32]
            };
            let signature_index = match self.type_signatures.get(&signature) {
                Some(index) => *index,
                None => {
                    let index = self.type_signatures.len() + 1;
                    emit_signature( &mut self.sections.type_section, &signature)?;
                    index
                }
            };
            self.sections.num_funcs += 1;
            self.sections.code_section.write(&func_body[..])?;
            self.sections.func_section.write(&signature_index.to_le_bytes())?;
        } else {
            bail!("Invalid argument.");
        }
        Ok(())
    }
    fn emit_toplevel(&mut self, ast: &AST) -> Result<()> {
        // toplevel can only be a function for now.
        self.emit_func(ast)
    }
    fn emit_module(&mut self, ast: &AST) -> Result<()> {
        self.writer.borrow_mut().write(&[0x00, 0x61, 0x73, 0x6d])?; // WASM magic number
        self.writer.borrow_mut().write(&[0x01, 0x00, 0x00, 0x00])?; // WASM binary version
        let toplevels = match ast {
            AST::Module(tops) => tops,
            _ => return Err(anyhow!("Invalid argument.")),
        };
        for toplevel in toplevels {
            self.emit_toplevel(toplevel)?;
        }
        Ok(())
    }
    pub fn emit(&mut self, source: &str) -> Result<()> {
        let module = parse(source)?;
        self.emit_module(&module)?;
        Ok(())
    }
}
