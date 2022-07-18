use crate::{
    env::{Env, Pointer, Variable},
    parser::{parse, AST},
    resolver::{dissolve_type, resolve_type, Type, TypeEnv},
};
use anyhow::{anyhow, bail, ensure, Result};
use std::{collections::HashMap, hash::Hash, rc::Rc};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportKind {
    Func,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub enum PrimitiveType {
    I32 = 0x7F,
    F32 = 0x6F,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Export {
    pub export_type: ExportKind,
    pub name: String,
    pub func_index: u32,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub enum SignatureType {
    Func = 0x60,
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct Signature {
    pub sig_type: SignatureType,
    pub params: Vec<PrimitiveType>,
    pub results: Vec<PrimitiveType>,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub signature_index: u32,
    pub body: Vec<OpCode>,
}

#[derive(PartialEq, Debug)]
pub enum OpCode {
    End,
    LocalDeclCount(u8),
    LocalGet(u8),
    I32Const(i32),
    F32Const(f32),
    I32Add,
    I32Sub,
    F32Add,
    F32Sub,
    F32ConvertI32S,
}

#[derive(Default, Debug, PartialEq)]
pub struct Module {
    pub signatures: HashMap<Signature, u16>,
    pub exports: Vec<Export>,
    pub functions: Vec<Function>,
}

pub struct Emitter<'a> {
    module: &'a mut Module,
}

enum BinOp {
    Add,
    Sub,
}

impl<'a> Emitter<'a> {
    pub fn new(module: &'a mut Module) -> Self {
        Emitter { module }
    }
    fn emit_bin_exp(
        &mut self,
        codes: &mut Vec<OpCode>,
        op: BinOp,
        lhs: &AST,
        rhs: &AST,
        env: &mut Env,
    ) -> Result<Type> {
        let mut lhs_temp_vec = Vec::new();
        let mut rhs_temp_vec = Vec::new();
        let lhs_type = self.emit_obj(&mut lhs_temp_vec, lhs, env)?;
        let rhs_type = self.emit_obj(&mut rhs_temp_vec, rhs, env)?;
        let (opcode, result_type) = match op {
            BinOp::Add => match *lhs_type {
                Type::I32 => match *rhs_type {
                    Type::F32 => {
                        // cast lhs to f32
                        lhs_temp_vec.push(OpCode::F32ConvertI32S);
                        (OpCode::F32Add, Type::F32)
                    }
                    Type::I32 => (OpCode::I32Add, Type::I32),
                },
                Type::F32 => match *rhs_type {
                    Type::I32 => {
                        // cast rhs to f32
                        rhs_temp_vec.push(OpCode::F32ConvertI32S);
                        (OpCode::F32Add, Type::F32)
                    }
                    Type::F32 => (OpCode::F32Add, Type::F32),
                },
            },
            BinOp::Sub => match *lhs_type {
                Type::I32 => match *rhs_type {
                    Type::F32 => {
                        // cast lhs to f32
                        lhs_temp_vec.push(OpCode::F32ConvertI32S);
                        (OpCode::F32Sub, Type::F32)
                    }
                    Type::I32 => (OpCode::I32Sub, Type::I32),
                },
                Type::F32 => match *rhs_type {
                    Type::I32 => {
                        // cast rhs to f32
                        rhs_temp_vec.push(OpCode::F32ConvertI32S);
                        (OpCode::F32Sub, Type::F32)
                    }
                    Type::F32 => (OpCode::F32Sub, Type::F32),
                },
            },
        };
        codes.append(&mut lhs_temp_vec);
        codes.append(&mut rhs_temp_vec);
        codes.push(opcode);
        Ok(result_type)
    }
    fn emit_list(&mut self, codes: &mut Vec<OpCode>, ast: &AST, env: &mut Env) -> Result<Rc<Type>> {
        match ast {
            AST::List(list) => {
                ensure!(
                    list.len() == 3,
                    "Binary op can only be evaluated with 2 args"
                );
                Ok(match &list[0] {
                    AST::Add => {
                        let result_type =
                            self.emit_bin_exp(codes, BinOp::Add, &list[1], &list[2], env)?;
                        Rc::new(result_type)
                    }
                    AST::Sub => {
                        let result_type =
                            self.emit_bin_exp(codes, BinOp::Sub, &list[1], &list[2], env)?;
                        Rc::new(result_type)
                    }
                    _ => todo!("Only + and - operator can be emitted for now."),
                })
            }
            _ => bail!("Invalid argument. emit_list only accepts AST::List"),
        }
    }
    fn emit_obj(&mut self, codes: &mut Vec<OpCode>, ast: &AST, env: &mut Env) -> Result<Rc<Type>> {
        match ast {
            AST::List(_) => return self.emit_list(codes, ast, env),
            // TODO: Infer type
            AST::NumberLiteral(literal) => {
                if let Ok(i32_val) = literal.parse::<i32>() {
                    codes.push(OpCode::I32Const(i32_val));
                    return Ok(Rc::new(Type::I32));
                } else if let Ok(f32_val) = literal.parse::<f32>() {
                    codes.push(OpCode::F32Const(f32_val));
                    return Ok(Rc::new(Type::F32));
                } else {
                    bail!("Failed to parse number");
                }
            }
            AST::Symbol(name) => match env.get(name) {
                None => bail!("Symbol {} not found in this scope", name),
                Some(variable) => match variable.pointer {
                    Pointer::Local(index) => {
                        codes.push(OpCode::LocalGet(index));
                        return Ok(variable.t.clone());
                    }
                },
            },
            _ => todo!(),
        }
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
                    let mut args = Vec::new();
                    match &slice[1] {
                        AST::List(list) => {
                            for arg in list {
                                args.push(match arg {
                                    AST::SymbolWithAnnotation(name, type_ast) => (*name, type_ast),
                                    _ => bail!(
                                        "Function argument should be a symbol annotated with ':'"
                                    ),
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

            // TODO: Impl type symbol functionality
            let empty_type_env = TypeEnv::default();

            let func_index = self.module.functions.len();
            let mut new_env = Env::extend(env.clone());
            let mut local_index = 0;
            for arg in &args {
                new_env.set(
                    arg.0,
                    Variable {
                        pointer: Pointer::Local(local_index),
                        t: Rc::new(resolve_type(arg.1, &empty_type_env)),
                    },
                );
                local_index += 1;
            }
            // TODO: local variables
            let mut func_body = Vec::new();
            func_body.push(OpCode::LocalDeclCount(0));

            for form in forms {
                self.emit_obj(&mut func_body, &form, &mut new_env)?;
            }
            func_body.push(OpCode::End);

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
            let signature_index = match self.module.signatures.get(&signature) {
                Some(index) => *index,
                None => {
                    let index = self.module.signatures.len() as u16;
                    self.module.signatures.insert(signature.clone(), index);
                    index
                }
            };

            self.module.functions.push(Function {
                name: name.to_string(),
                signature_index: signature_index as u32,
                body: func_body,
            });

            if is_export {
                self.module.exports.push(Export {
                    export_type: ExportKind::Func,
                    name: name.to_string(),
                    func_index: func_index as u32,
                });
            }
        } else {
            bail!("Invalid argument.");
        }
        Ok(())
    }
    fn emit_toplevel(&mut self, ast: &AST) -> Result<()> {
        // TODO: Impl Global Variables
        // toplevel can only be a function for now.
        self.emit_func(ast, Rc::new(Env::default()))
    }
    fn emit_module(&mut self, ast: &AST) -> Result<()> {
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
        let module_ast = parse(source)?;
        self.emit_module(&module_ast)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bin_ops() {
        let mut module: Module = Module::default();
        let mut emitter = Emitter::new(&mut module);
        emitter
            .emit(
                "(defn calc : f32
                                (a : f32 b : i32)
                                 (+ a (- b 1)))",
            )
            .unwrap();
        assert_eq!(module.exports, []);
        assert_eq!(module.signatures.len(), 1);
        assert_eq!(
            *module.signatures.keys().next().unwrap(),
            Signature {
                sig_type: SignatureType::Func,
                params: vec![PrimitiveType::F32, PrimitiveType::I32],
                results: vec![PrimitiveType::F32]
            }
        );
        assert_eq!(
            module.functions[0],
            Function {
                name: "calc".to_string(),
                signature_index: 0,
                body: vec![
                    OpCode::LocalDeclCount(0),
                    OpCode::LocalGet(0),
                    OpCode::LocalGet(1),
                    OpCode::I32Const(1),
                    OpCode::I32Sub,
                    OpCode::F32ConvertI32S,
                    OpCode::F32Add,
                    OpCode::End
                ]
            }
        )
    }
    #[test]
    fn test_export() {
        let mut module: Module = Module::default();
        let mut emitter = Emitter::new(&mut module);
        emitter
            .emit("(export defn addTwo : i32
                    (a : i32 b: i32)
                        (+ a b))",
            )
            .unwrap();
        assert_eq!(module.exports, vec![Export {
            export_type: ExportKind::Func,
            func_index: 0,
            name: "addTwo".to_string()
        }])
    }
}
