use crate::{
    env::{Env, Pointer},
    parser::{parse, AST},
    encoder::{encode_s_leb128, encode_leb128}
};
use anyhow::{anyhow, bail, ensure, Context, Result};
use std::{collections::HashMap, io::Write, rc::Rc, vec};
use dbg_hex::dbg_hex;

enum SectionCode {
    Type = 0x01,
    Function = 0x03,
    Code = 0x0A,
}

#[derive(Debug)]
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
    I32 = 0x7F,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
enum SignatureType {
    Func = 0x60,
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
struct Signature {
    sig_type: SignatureType,
    params: Vec<PrimitiveType>,
    results: Vec<PrimitiveType>,
}

#[derive(PartialEq, Eq, Debug)]
enum OpCode {
    I32Add = 0x6A,
    End = 0x0B,
    LocalGet = 0x20,
}

const DEFN_KEYWORD: &str = "defn";

fn emit_signature<W: Write>(writer: &mut W, signature: &Signature) -> Result<()> {
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
            .map(|r| *r as u8)
            .collect::<Vec<_>>()[..],
    )?;
    Ok(())
}

pub struct Emitter<'a, W: Write> {
    writer: &'a mut W,
    sections: Sections,
    type_signatures: HashMap<Signature, u16>,
    function_names: Vec<String>,
}

impl<'a, W: Write> Emitter<'a, W> {
    pub fn new(writer: &'a mut W) -> Self {
        Emitter {
            writer,
            sections: Sections {
                type_section: Vec::new(),
                num_funcs: 0,
                func_section: Vec::new(),
                code_section: Vec::new(),
            },
            type_signatures: HashMap::new(),
            function_names: Vec::new(),
        }
    }
    fn emit_add<W2: Write>(
        &mut self,
        writer: &mut W2,
        lhs: &AST,
        rhs: &AST,
        env: &mut Env,
    ) -> Result<()> {
        self.emit_obj(writer, lhs, env)?;
        self.emit_obj(writer, rhs, env)?;
        writer.write(&[OpCode::I32Add as u8])?;
        Ok(())
    }
    fn emit_list<W2: Write>(&mut self, writer: &mut W2, ast: &AST, env: &mut Env) -> Result<()> {
        match ast {
            AST::List(list) => {
                ensure!(
                    list.len() == 3,
                    "Binary op can only be evaluated with 2 args"
                );
                match &list[0] {
                    AST::Add => {
                        self.emit_add(writer, &list[1], &list[2], env)?;
                    }
                    _ => todo!("Only + operator can be emitted for now."),
                }
            }
            _ => bail!("Invalid argument. emit_list only accepts AST::List"),
        }
        Ok(())
    }
    fn emit_obj<W2: Write>(&mut self, writer: &mut W2, ast: &AST, env: &mut Env) -> Result<()> {
        match ast {
            AST::List(_) => {
                self.emit_list(writer, ast, env)?;
            }
            // TODO: Infer type
            AST::NumberLiteral(literal) => {
                let val = literal
                    .parse::<i32>()
                    .with_context(|| "Failed to parse number")?;
                encode_s_leb128(writer, val)?;
            }
            AST::Symbol(name) => match env.get(name) {
                None => bail!("Symbol {} not found in this scope", name),
                Some(pointer) => match pointer {
                    Pointer::Local(index) => {
                        writer.write(&[OpCode::LocalGet as u8])?;
                        encode_leb128(writer, index)?;
                    }
                },
            },
            _ => todo!(),
        }
        Ok(())
    }
    fn emit_func(&mut self, ast: &AST, env: Rc<Env>) -> Result<()> {
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
            let func_name = match &func_list[1] {
                AST::Symbol(name) => name,
                _ => bail!(
                    "Function name expected after 'defn', but found {:?}",
                    func_list[1]
                ),
            };
            self.function_names.push(func_name.to_string());
            let mut args = Vec::new();
            match &func_list[2] {
                AST::List(list) => {
                    for arg in list {
                        if let AST::Symbol(arg) = arg {
                            args.push(arg);
                        } else {
                            bail!("Invalid syntax. Function arg must be symbol");
                        }
                    }
                }
                _ => bail!(
                    "Invalid syntax. Function args must be a list, but found {:?}",
                    &func_list[1]
                ),
            };
            let mut new_env = Env::extend(env.clone());
            let mut local_index = 0;
            for arg in &args {
                new_env.set(arg, Pointer::Local(local_index));
                local_index += 1;
            }
            // TODO: local variables
            let mut func_body = Vec::new();
            func_body.push(0x00); // local decl count
            let forms = &func_list[3..];
            for form in forms {
                self.emit_obj(&mut func_body, form, &mut new_env)?;
            }
            func_body.push(OpCode::End as u8);

            let signature = Signature {
                sig_type: SignatureType::Func,
                // TODO: param can only be I32 for now.
                params: args.iter().map(|_| PrimitiveType::I32).collect::<Vec<_>>(),
                // TODO: result can only be single I32 for now.
                results: vec![PrimitiveType::I32],
            };
            let signature_index = match self.type_signatures.get(&signature) {
                Some(index) => *index,
                None => {
                    let index = self.type_signatures.len() as u16;
                    self.type_signatures.insert(signature.clone(), index);
                    emit_signature(&mut self.sections.type_section, &signature)?;
                    index
                }
            };
            dbg!(signature_index);
            self.sections.num_funcs += 1;
            encode_leb128(&mut self.sections.code_section, func_body.len() as u64)?;
            self.sections.code_section.write(&func_body[..])?;
            encode_leb128(&mut self.sections.func_section, signature_index)?;
            dbg_hex!(&self.sections);
        } else {
            bail!("Invalid argument.");
        }
        Ok(())
    }
    fn emit_toplevel(&mut self, ast: &AST) -> Result<()> {
        // toplevel can only be a function for now.
        self.emit_func(ast, Rc::new(Env::default()))
    }
    fn emit_module(&mut self, ast: &AST) -> Result<()> {
        self.writer.write(&[0x00, 0x61, 0x73, 0x6d])?; // WASM magic number
        self.writer.write(&[0x01, 0x00, 0x00, 0x00])?; // WASM binary version
        let toplevels = match ast {
            AST::Module(tops) => tops,
            _ => return Err(anyhow!("Invalid argument.")),
        };
        for toplevel in toplevels {
            self.emit_toplevel(toplevel)?;
        }
        let writer = &mut self.writer;
        // Emit type section
        writer.write(&[SectionCode::Type as u8])?;
        let num_types = self.type_signatures.len();
        dbg!(num_types);
        let mut num_types_bytes = Vec::new();
        encode_leb128(&mut num_types_bytes, num_types as u64)?;
        let type_section_size = self.sections.type_section.len() + num_types_bytes.len();
        encode_leb128(writer, type_section_size as u64)?;
        writer.write(&num_types_bytes)?;
        writer.write(&self.sections.type_section)?;

        // Emit function section
        writer.write(&[SectionCode::Function as u8])?;
        let num_functions = self.sections.num_funcs;
        dbg!(num_functions);
        let mut num_funcs_bytes = Vec::new();
        encode_leb128(&mut num_funcs_bytes, num_functions as u8)?;
        let func_section_size = num_funcs_bytes.len() + self.sections.func_section.len();
        encode_leb128(writer, func_section_size as u64)?;
        writer.write(&num_funcs_bytes)?;
        writer.write(&self.sections.func_section)?;

        // Emit code section
        writer.write(&[SectionCode::Code as u8])?;
        let code_section_size = num_funcs_bytes.len() + self.sections.code_section.len();
        // self.writer.write(&code_section_size.to_le_bytes())?;
        encode_leb128(writer, code_section_size as u64)?;
        self.writer.write(&num_funcs_bytes)?;
        self.writer.write(&self.sections.code_section)?;
        Ok(())
    }

    pub fn emit(&mut self, source: &str) -> Result<()> {
        let module = parse(source)?;
        self.emit_module(&module)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_add() {
        let mut buf = Vec::new();
        let mut emitter = Emitter::new(&mut buf);
        emitter.emit("(defn addTwo (a b) (+ a b))").unwrap();
        assert_eq!(
            buf,
            vec![0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
                0x01, // type section
                0x07, // section size
                0x01, // num types
                0x60, // type: func
                0x02, // num params
                0x7F, // i32
                0x7F, // i32
                0x01, // num results
                0x7F, // i32
                0x03, // function section
                0x02, // section size
                0x01, // num funcs
                0x00, // signature index
                0x0A, // code section
                0x09, // section size
                0x01, // num functions
                0x07, // func body size
                0x00, // local decl count
                0x20, 0x00, // local.get 0
                0x20, 0x01, // local.get 1,
                0x6A, // i32.add
                0x0B, // END
                ]);
    }
}
