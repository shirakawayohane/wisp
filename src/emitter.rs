use crate::{
    encoder::{encode_leb128, encode_s_leb128, encode_string},
    env::{Env, Pointer},
    parser::{parse, AST},
    resolver::{dissolve_type, resolve_type, TypeEnv},
};
use anyhow::{anyhow, bail, ensure, Context, Result};
use std::{collections::HashMap, hash::Hash, io::Write, rc::Rc};

enum SectionCode {
    Type = 0x01,
    Function = 0x03,
    Export = 0x07,
    Code = 0x0a,
}

#[derive(Debug)]
struct Sections {
    type_section: Vec<u8>,
    func_section: Vec<u8>,
    code_section: Vec<u8>,
    export_section: Vec<u8>,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub enum PrimitiveType {
    I32 = 0x7f,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum ExportKind {
    Func = 0x00,
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
    End = 0x0b,
    LocalGet = 0x20,
    I32Const = 0x41,
    I32Add = 0x6a,
    I32Sub = 0x6b,
}

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
    num_exports: u16,
}

enum BinOp {
    Add,
    Sub
}

impl<'a, W: Write> Emitter<'a, W> {
    pub fn new(writer: &'a mut W) -> Self {
        Emitter {
            writer,
            sections: Sections {
                type_section: Vec::new(),
                func_section: Vec::new(),
                code_section: Vec::new(),
                export_section: Vec::new(),
            },
            type_signatures: HashMap::new(),
            function_names: Vec::new(),
            num_exports: 0,
        }
    }
    fn emit_bin_exp<W2: Write>(
        &mut self,
        writer: &mut W2,
        op: BinOp,
        lhs: &AST,
        rhs: &AST,
        env: &mut Env,
    ) -> Result<()> {
        self.emit_obj(writer, lhs, env)?;
        self.emit_obj(writer, rhs, env)?;
        // ToDo: Change binop according to lhs and rhs type.
        let opcode = match op {
            BinOp::Add => OpCode::I32Add,
            BinOp::Sub => OpCode::I32Sub,
        };
        writer.write(&[opcode as u8])?;
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
                        self.emit_bin_exp(writer, BinOp::Add, &list[1], &list[2], env)?;
                    },
                    AST::Sub => {
                        self.emit_bin_exp(writer, BinOp::Sub, &list[1], &list[2], env)?;
                    },
                    _ => todo!("Only + and - operator can be emitted for now."),
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
                writer.write(&[OpCode::I32Const as u8])?;
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
            let mut slice = &func_list[..];
            let (is_export, name, result_type_ast, args, forms) = match func_list[0] {
                AST::Symbol(s) => {
                    let is_export = if s == "export" {
                        ensure!(
                            slice[1] == AST::Symbol("defn"),
                            "Failed to compile function. 'defn' is expected after 'export'"
                        );
                        slice = &slice[2..];
                        true
                    } else {
                        ensure!(s == "defn", "Failed to compile function. func list must start with 'export' or 'defn'");
                        slice = &slice[1..];
                        false
                    };
                    let (name, type_ast) = match &slice[0] {
                        AST::SymbolWithAnnotation(s, type_ast) => (*s, type_ast),
                        _ => bail!("A symbol with type annotaion is expected after 'defn'"),
                    };
                    dbg!(&slice[1]);
                    let mut args = Vec::new();
                    match &slice[1] {
                        AST::List(list) => {
                            for arg in list {
                                args.push(match arg {
                                    AST::SymbolWithAnnotation(name, type_ast) => (*name, type_ast),
                                    _ => bail!("Function argument should be a symbol annotated with ':'")
                                });
                            }
                        }
                        _ => bail!("Function args list is required after 'defn'"),
                    };
                    let forms = Vec::from(&slice[2..]);
                    (is_export, name, type_ast, args, forms)
                }
                _ => todo!(),
            };
            let func_index = self.function_names.len();
            self.function_names.push(name.to_string());
            let mut new_env = Env::extend(env.clone());
            let mut local_index = 0;
            for arg in &args {
                new_env.set(arg.0, Pointer::Local(local_index));
                local_index += 1;
            }
            // TODO: local variables
            let mut func_body = Vec::new();
            func_body.push(0x00); // local decl count

            for form in forms {
                println!("emitting form {:?}", form);
                self.emit_obj(&mut func_body, &form, &mut new_env)?;
                dbg!(&func_body);
            }
            func_body.push(OpCode::End as u8);

            // TODO: Impl type symbol functionality
            let empty_type_env = TypeEnv::default();
            let signature = Signature {
                sig_type: SignatureType::Func,
                params: args
                    .iter()
                    .flat_map(|(_, type_ast)| {
                        dissolve_type(resolve_type(type_ast, &empty_type_env))
                    })
                    .collect::<Vec<_>>(),
                results: dissolve_type(resolve_type(result_type_ast, &empty_type_env)),
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
            encode_leb128(&mut self.sections.code_section, func_body.len() as u64)?;
            self.sections.code_section.write(&func_body[..])?;
            encode_leb128(&mut self.sections.func_section, signature_index)?;

            if is_export {
                // emit export
                let export_section = &mut self.sections.export_section;
                encode_string(export_section, name)?; // export name
                export_section.write(&[ExportKind::Func as u8])?; // export kind
                encode_leb128(export_section, func_index as u64)?; // export func index
                self.num_exports += 1;
            }
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
        let num_functions = self.function_names.len();
        dbg!(num_functions);
        let mut num_funcs_bytes = Vec::new();
        encode_leb128(&mut num_funcs_bytes, num_functions as u8)?;
        let func_section_size = num_funcs_bytes.len() + self.sections.func_section.len();
        encode_leb128(writer, func_section_size as u64)?;
        writer.write(&num_funcs_bytes)?;
        writer.write(&self.sections.func_section)?;

        // Emit export section
        writer.write(&[SectionCode::Export as u8])?;
        let mut num_exports_bytes = Vec::new();
        encode_leb128(&mut num_exports_bytes, self.num_exports as u64)?;
        let export_section_size = self.sections.export_section.len() + num_exports_bytes.len();
        encode_leb128(writer, export_section_size as u64)?;
        writer.write(&num_exports_bytes)?;
        writer.write(&self.sections.export_section)?;

        // Emit code section
        writer.write(&[SectionCode::Code as u8])?;
        let code_section_size = num_funcs_bytes.len() + self.sections.code_section.len();
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
    fn test_bin_ops() {
        let mut buf = Vec::new();
        let mut emitter = Emitter::new(&mut buf);
        emitter
            .emit(
                "(defn calc : i32
                                (a : i32 b : i32)
                                 (+ a (- b 1)))",
            )
            .unwrap();
        assert_eq!(
            buf,
            vec![
                0x00, 0x61, 0x73, 0x6d, // wasm header
                0x01, 0x00, 0x00, 0x00, // wasm binary version
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
                0x07, // export section
                0x01, // section size,
                0x00, // num exports
                0x0A, // code section
                0x0C, // section size
                0x01, // num functions
                0x0A, // func body size
                0x00, // local decl count
                0x20, 0x00, // local.get 0
                0x20, 0x01, // local.get 1,
                0x41, 0x01, // i32.const 1
                0x6B, // i32.sub
                0x6A, // i32.add
                0x0B, // END
            ]
        );
    }

    #[test]
    fn test_export() {
        let mut buf = Vec::new();
        let mut emitter = Emitter::new(&mut buf);
        emitter.emit("(export defn addTwo : i32 \
                                (a : i32 b: i32)\
                                    (+ a b))").unwrap();
        assert_eq!(
            buf,
            vec![
                0x00, 0x61, 0x73, 0x6d, // wasm header
                0x01, 0x00, 0x00, 0x00, // wasm binary version
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
                0x07, // export section
                0x0a, // section size
                0x01, // num_exports
                0x06, // size of name string
                0x61, 0x64, 0x64, 0x54, 0x77, 0x6f, // addTwo in utf8
                0x00, // export type: func
                0x00, // export function index
                0x0A, // code section
                0x09, // section size
                0x01, // num functions
                0x07, // func body size
                0x00, // local decl count
                0x20, 0x00, // local.get 0
                0x20, 0x01, // local.get 1,
                0x6A, // i32.add
                0x0B, // END
            ]
        )
    }
}
