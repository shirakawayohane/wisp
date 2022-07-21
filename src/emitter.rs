use crate::{
    env::{Env, Pointer, Variable},
    parser::{parse_source, TypeAST, AST},
    resolver::{get_primitive_types, resolve_type, Type, TypeEnv},
};
use anyhow::{anyhow, bail, ensure, Context, Result};
use std::{borrow::Borrow, cell::RefCell, collections::HashMap, fmt::Display, hash::Hash, rc::Rc};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportKind {
    Func,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub enum WasmPrimitiveType {
    I32 = 0x7F,
    F32 = 0x7D,
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
    pub params: Vec<WasmPrimitiveType>,
    pub results: Vec<WasmPrimitiveType>,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub signature_index: u32,
    pub arg_types: Vec<Rc<Type>>,
    pub result_type: Rc<Type>,
    pub body: Vec<OpCode>,
}

#[derive(PartialEq, Debug)]
pub enum OpCode {
    If(Option<WasmPrimitiveType>),
    Else,
    Drop,
    End,
    LocalGet(u8),
    LocalSet(u8),
    LocalDecl(WasmPrimitiveType),
    Call(u32),
    I32Const(i32),
    F32Const(f32),
    I32Add,
    I32Sub,
    I32Mul,
    I32DivS,
    I32Xor,
    I32Eq,
    I32GtS,
    I32GeS,
    I32And,
    I32Or,
    I32LtS,
    I32LeS,
    F32Add,
    F32Sub,
    F32Mul,
    F32Div,
    F32Eq,
    F32Gt,
    F32Ge,
    F32Lt,
    F32Le,
    F32Neg,
    F32ConvertI32S,
}

#[derive(Debug, PartialEq, Default)]
pub struct Module {
    pub signatures: HashMap<Signature, u16>,
    pub exports: Vec<Export>,
    pub functions: Rc<RefCell<HashMap<String, (usize, Function)>>>,
}

pub struct Emitter<'a> {
    module: &'a mut Module,
}

#[derive(Debug, PartialEq, Eq)]
enum IntrinsicOperator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Gt,
    Ge,
    Lt,
    Le,
    And,
    Or,
    Not,
}

impl Display for IntrinsicOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                IntrinsicOperator::Add => "+",
                IntrinsicOperator::Sub => "-",
                IntrinsicOperator::Mul => "*",
                IntrinsicOperator::Div => "/",
                IntrinsicOperator::Eq => "=",
                IntrinsicOperator::Gt => ">",
                IntrinsicOperator::Ge => ">=",
                IntrinsicOperator::Lt => "<",
                IntrinsicOperator::Le => "<=",
                IntrinsicOperator::And => "and",
                IntrinsicOperator::Or => "or",
                IntrinsicOperator::Not => "not",
            }
        )
    }
}

impl<'a> Emitter<'a> {
    pub fn new(module: &'a mut Module) -> Self {
        Emitter { module }
    }
    fn emit_intrinsic_exp(
        &mut self,
        op: IntrinsicOperator,
        codes: &mut Vec<OpCode>,
        args: &[AST],
        env: Rc<RefCell<Env>>,
    ) -> Result<Rc<Type>> {
        ensure!(args.len() > 0);
        if args.len() == 1 {
            let arg = &args[0];
            match op {
                IntrinsicOperator::Add => match *self.emit_obj(codes, arg, env)? {
                    Type::Bool | Type::Unit => {
                        bail!("Invalid argument for unary op. expected numeric type")
                    }
                    Type::I32 => Ok(Rc::new(Type::I32)),
                    Type::F32 => Ok(Rc::new(Type::F32)),
                },
                IntrinsicOperator::Sub => match *self.emit_obj(codes, arg, env)? {
                    Type::Bool | Type::Unit => {
                        bail!("Invalid argument for unary op. expected numeric type")
                    }
                    Type::I32 => {
                        codes.push(OpCode::I32Const(-1));
                        codes.push(OpCode::I32Xor);
                        Ok(Rc::new(Type::I32))
                    }
                    Type::F32 => {
                        codes.push(OpCode::F32Neg);
                        Ok(Rc::new(Type::F32))
                    }
                },
                IntrinsicOperator::Mul => match *self.emit_obj(codes, arg, env)? {
                    Type::Bool | Type::Unit => {
                        bail!("Invalid argument for unary op. expected numeric type")
                    }
                    Type::I32 => Ok(Rc::new(Type::I32)),
                    Type::F32 => Ok(Rc::new(Type::F32)),
                },
                IntrinsicOperator::Div => {
                    codes.push(OpCode::F32Const(1.0));
                    match *self.emit_obj(codes, arg, env)? {
                        Type::Bool | Type::Unit => {
                            bail!("Invalid argument for unary op. expected numeric type")
                        }
                        Type::I32 => {
                            codes.push(OpCode::F32ConvertI32S);
                            codes.push(OpCode::I32DivS);
                            Ok(Rc::new(Type::I32))
                        }
                        Type::F32 => {
                            codes.push(OpCode::F32Div);
                            Ok(Rc::new(Type::F32))
                        }
                    }
                }
                IntrinsicOperator::Eq
                | IntrinsicOperator::Gt
                | IntrinsicOperator::Ge
                | IntrinsicOperator::Lt
                | IntrinsicOperator::Le
                | IntrinsicOperator::And
                | IntrinsicOperator::Or
                | IntrinsicOperator::Not => {
                    bail!("Comp operators cannot evaluated with 1 arg");
                }
            }
        } else {
            match op {
                IntrinsicOperator::Add
                | IntrinsicOperator::Sub
                | IntrinsicOperator::Mul
                | IntrinsicOperator::Div => {
                    let last_codes = codes;
                    let current_codes = &mut Vec::new();
                    let mut last_result = self.emit_obj(last_codes, &args[0], env.clone())?;
                    for arg in &args[1..] {
                        let current_result = self.emit_obj(current_codes, arg, env.clone())?;
                        let opcode = match *last_result {
                            Type::Bool | Type::Unit => bail!("hoge"),
                            Type::I32 => match *current_result {
                                Type::Bool | Type::Unit => bail!("hoge"),
                                Type::I32 => match op {
                                    IntrinsicOperator::Add => OpCode::I32Add,
                                    IntrinsicOperator::Sub => OpCode::I32Sub,
                                    IntrinsicOperator::Mul => OpCode::I32Mul,
                                    IntrinsicOperator::Div => OpCode::I32DivS,
                                    _ => unreachable!(),
                                },
                                Type::F32 => {
                                    last_codes.push(OpCode::F32ConvertI32S);
                                    last_result = current_result;
                                    match op {
                                        IntrinsicOperator::Add => OpCode::F32Add,
                                        IntrinsicOperator::Sub => OpCode::F32Sub,
                                        IntrinsicOperator::Mul => OpCode::F32Mul,
                                        IntrinsicOperator::Div => OpCode::F32Div,
                                        _ => unreachable!(),
                                    }
                                }
                            },
                            Type::F32 => match *current_result {
                                Type::Bool | Type::Unit => bail!("hoge"),
                                Type::I32 => {
                                    current_codes.push(OpCode::F32ConvertI32S);
                                    match op {
                                        IntrinsicOperator::Add => OpCode::F32Add,
                                        IntrinsicOperator::Sub => OpCode::F32Sub,
                                        IntrinsicOperator::Mul => OpCode::F32Mul,
                                        IntrinsicOperator::Div => OpCode::F32Div,
                                        _ => unreachable!(),
                                    }
                                }
                                Type::F32 => match op {
                                    IntrinsicOperator::Add => OpCode::F32Add,
                                    IntrinsicOperator::Sub => OpCode::F32Sub,
                                    IntrinsicOperator::Mul => OpCode::F32Mul,
                                    IntrinsicOperator::Div => OpCode::F32Div,
                                    _ => unreachable!(),
                                },
                            },
                        };
                        last_codes.append(current_codes);
                        last_codes.push(opcode);
                    }
                    Ok(last_result)
                }
                IntrinsicOperator::Eq
                | IntrinsicOperator::Gt
                | IntrinsicOperator::Ge
                | IntrinsicOperator::Lt
                | IntrinsicOperator::Le
                | IntrinsicOperator::And
                | IntrinsicOperator::Or
                | IntrinsicOperator::Not => {
                    let left_codes = codes;
                    let right_codes = &mut Vec::new();
                    for i in 0..args.len() - 1 {
                        let left = &args[i];
                        let right = &args[i + 1];
                        let left_type = self.emit_obj(left_codes, left, env.clone())?;
                        let right_type = self.emit_obj(right_codes, right, env.clone())?;
                        match *left_type {
                            Type::Unit => bail!("Unit type cannot be compared."),
                            Type::Bool => {
                                match op {
                                    IntrinsicOperator::Eq => right_codes.push(OpCode::I32Eq),
                                    IntrinsicOperator::And => right_codes.push(OpCode::I32And),
                                    IntrinsicOperator::Or => right_codes.push(OpCode::I32Or),
                                    IntrinsicOperator::Not => {
                                        right_codes.push(OpCode::I32Const(-1));
                                        right_codes.push(OpCode::I32Xor);
                                    }
                                    _ => bail!(
                                        "cannot compare types {} and {}",
                                        *left_type,
                                        *right_type
                                    ),
                                };
                            }
                            Type::I32 => match *right_type {
                                Type::Bool | Type::Unit => bail!("hoge"),
                                Type::F32 => {
                                    left_codes.push(OpCode::F32ConvertI32S);
                                    match op {
                                        IntrinsicOperator::Eq => right_codes.push(OpCode::F32Eq),
                                        IntrinsicOperator::Gt => right_codes.push(OpCode::F32Gt),
                                        IntrinsicOperator::Ge => right_codes.push(OpCode::F32Ge),
                                        IntrinsicOperator::Lt => right_codes.push(OpCode::F32Lt),
                                        IntrinsicOperator::Le => right_codes.push(OpCode::F32Le),
                                        _ => bail!("cannot calc {} for numeric types", op),
                                    }
                                }
                                Type::I32 => match op {
                                    IntrinsicOperator::Eq => right_codes.push(OpCode::I32Eq),
                                    IntrinsicOperator::Gt => right_codes.push(OpCode::I32GtS),
                                    IntrinsicOperator::Ge => right_codes.push(OpCode::I32GeS),
                                    IntrinsicOperator::Lt => right_codes.push(OpCode::I32LtS),
                                    IntrinsicOperator::Le => right_codes.push(OpCode::I32LeS),
                                    _ => bail!("cannot calc {} for numeric types", op),
                                },
                            },
                            Type::F32 => match *right_type {
                                Type::Bool | Type::Unit => bail!("hoge"),
                                Type::F32 => {
                                    left_codes.push(OpCode::F32ConvertI32S);
                                    match op {
                                        IntrinsicOperator::Eq => right_codes.push(OpCode::F32Eq),
                                        IntrinsicOperator::Gt => right_codes.push(OpCode::F32Gt),
                                        IntrinsicOperator::Ge => right_codes.push(OpCode::F32Ge),
                                        IntrinsicOperator::Lt => right_codes.push(OpCode::F32Lt),
                                        IntrinsicOperator::Le => right_codes.push(OpCode::F32Le),
                                        _ => bail!("cannot calc {} for numeric types", op),
                                    }
                                }
                                Type::I32 => {
                                    right_codes.push(OpCode::F32ConvertI32S);
                                    match op {
                                        IntrinsicOperator::Eq => right_codes.push(OpCode::F32Eq),
                                        IntrinsicOperator::Gt => right_codes.push(OpCode::F32Gt),
                                        IntrinsicOperator::Ge => right_codes.push(OpCode::F32Ge),
                                        IntrinsicOperator::Lt => right_codes.push(OpCode::F32Lt),
                                        IntrinsicOperator::Le => right_codes.push(OpCode::F32Le),
                                        _ => bail!("cannot calc {} for numeric types", op),
                                    }
                                }
                            },
                        }
                        if i > 0 {
                            right_codes.push(OpCode::I32And)
                        }
                        left_codes.append(right_codes);
                    }
                    Ok(Rc::new(Type::Bool))
                }
            }
        }
    }
    fn emit_function_call(
        &mut self,
        codes: &mut Vec<OpCode>,
        index: u32,
        func: &Function,
        args: &[AST],
        env: Rc<RefCell<Env>>,
    ) -> Result<Rc<Type>> {
        for arg in args {
            self.emit_obj(codes, arg, env.clone())?;
        }
        codes.push(OpCode::Call(index as u32));
        Ok(func.result_type.clone())
    }
    fn emit_scope(
        &mut self,
        codes: &mut Vec<OpCode>,
        forms: &[AST],
        env: Rc<RefCell<Env>>,
    ) -> Result<Rc<Type>> {
        for (index, form) in forms.iter().enumerate() {
            let last = index == forms.len() - 1;
            if last {
                let result_type = self.emit_obj(codes, &form, env.clone())?;
                return Ok(result_type);
            } else {
                let emitted_type = self.emit_obj(codes, &form, env.clone())?;
                let stack_cnt = get_primitive_types(emitted_type)
                    .iter()
                    .filter(|x| x.is_some())
                    .count();
                // Drop unused results
                for _ in 0..stack_cnt {
                    codes.push(OpCode::Drop);
                }
            }
        }
        unreachable!();
    }
    fn emit_let(
        &mut self,
        codes: &mut Vec<OpCode>,
        ast: &AST,
        env: Rc<RefCell<Env>>,
    ) -> Result<Rc<Type>> {
        if let AST::List(list) = ast {
            let slice = &list[..];
            ensure!(slice.len() > 2);
            let (binding_vector, forms) = match &slice[0] {
                AST::Symbol("let") => (&slice[1], &slice[2..]),
                _ => unreachable!(),
            };
            if let AST::Vector(bindings) = binding_vector {
                ensure!(
                    bindings.len() % 2 == 0,
                    "let accepts only even number forms."
                );
                let new_env = Rc::new(RefCell::new(Env::extend(env)));
                for i in 0..bindings.len() / 2 {
                    match bindings[i * 2] {
                        AST::Symbol(variable_name) => {
                            let value = &bindings[i * 2 + 1];
                            let value_type = self.emit_obj(codes, value, new_env.clone())?;
                            let local_index = (*new_env.clone()).borrow().count_local_vars() as u8;
                            let pointer = Pointer::Local(local_index);
                            // prohibit local var redefinition
                            match new_env.borrow_mut().set(
                                variable_name,
                                Variable {
                                    pointer,
                                    t: value_type.clone(),
                                },
                            ) {
                                None => (),
                                Some(_) => bail!("redefinition of {}", variable_name),
                            }
                            match value_type.borrow() {
                                Type::F32 => {
                                    codes.push(OpCode::LocalDecl(WasmPrimitiveType::F32));
                                    codes.push(OpCode::LocalSet(local_index));
                                }
                                Type::I32 => {
                                    codes.push(OpCode::LocalDecl(WasmPrimitiveType::I32));
                                    codes.push(OpCode::LocalSet(local_index));
                                }
                                Type::Bool => {
                                    codes.push(OpCode::LocalDecl(WasmPrimitiveType::I32));
                                    codes.push(OpCode::LocalSet(local_index));
                                }
                                Type::Unit => bail!("hoge"),
                            }
                        }
                        AST::SymbolWithAnnotation(_, _) => {
                            todo!("Impl local decl with type annotation");
                        }
                        _ => bail!(
                            "let binding accepts only symbol for odd-numbered forms, found {:?}",
                            bindings[i * 2]
                        ),
                    }
                }
                return self.emit_scope(codes, forms, new_env);
            } else {
                bail!("A binding vector is expected after 'let'")
            }
        } else {
            unreachable!()
        }
    }
    fn emit_if(
        &mut self,
        codes: &mut Vec<OpCode>,
        ast: &AST,
        env: Rc<RefCell<Env>>,
    ) -> Result<Rc<Type>> {
        let forms = match ast {
            AST::List(forms) => {
                ensure!(
                    forms.len() == 4,
                    "if expects just 3 forms, found {}",
                    forms.len()
                );
                ensure!(*forms.first().unwrap() == AST::Symbol("if"));
                &forms[1..]
            }
            _ => unreachable!(),
        };
        let bool_exp = &forms[0];
        let true_exp = &forms[1];
        let false_exp = &forms[2];
        ensure!(
            *self.emit_obj(codes, bool_exp, env.clone())? == Type::Bool,
            "first form of if must be bool expression"
        );
        let temp_codes = &mut Vec::new();
        let true_form_type = self.emit_obj(temp_codes, true_exp, env.clone())?;
        temp_codes.push(OpCode::Else);
        let false_form_type = self.emit_obj(temp_codes, false_exp, env.clone())?;
        
        // ToDo: Improve flexibility
        ensure!(
            *true_form_type == *false_form_type,
            "mismatched types. found {} and {}",
            true_form_type,
            false_form_type
        );

        let primitive_type = get_primitive_types(true_form_type.clone());
        if primitive_type.len() != 1 {
            unimplemented!("tuple is not implemented");
        }
        codes.push(OpCode::If(
            *get_primitive_types(true_form_type.clone()).first().unwrap(),
        ));
        codes.append(temp_codes);
        codes.push(OpCode::End);
        Ok(true_form_type.clone())
    }
    fn emit_list(
        &mut self,
        codes: &mut Vec<OpCode>,
        ast: &AST,
        env: Rc<RefCell<Env>>,
    ) -> Result<Rc<Type>> {
        match ast {
            AST::List(list) => {
                let first = &list[0];
                Ok(match first {
                    // Intrinsic operators
                    AST::Add
                    | AST::Sub
                    | AST::Mul
                    | AST::Div
                    | AST::Eq
                    | AST::Gt
                    | AST::Ge
                    | AST::Lt
                    | AST::Le
                    | AST::And
                    | AST::Or
                    | AST::Not => {
                        let op = match first {
                            AST::Add => IntrinsicOperator::Add,
                            AST::Sub => IntrinsicOperator::Sub,
                            AST::Mul => IntrinsicOperator::Mul,
                            AST::Div => IntrinsicOperator::Div,
                            AST::Eq => IntrinsicOperator::Eq,
                            AST::Gt => IntrinsicOperator::Gt,
                            AST::Ge => IntrinsicOperator::Ge,
                            AST::Lt => IntrinsicOperator::Lt,
                            AST::Le => IntrinsicOperator::Le,
                            AST::And => IntrinsicOperator::And,
                            AST::Or => IntrinsicOperator::Or,
                            AST::Not => IntrinsicOperator::Not,
                            _ => unreachable!(),
                        };
                        self.emit_intrinsic_exp(op, codes, &list[1..], env)?
                    }
                    AST::Symbol(name) => {
                        match *name {
                            "let" => self.emit_let(codes, ast, env)?,
                            "if" => self.emit_if(codes, ast, env)?,
                            _ => {
                                // emit function call
                                let module_functions = self.module.functions.clone();
                                let module_func_refmut = module_functions.borrow_mut();
                                let (index, func) =
                                    module_func_refmut.get(*name).with_context(|| {
                                        format!("Unable to find function {:?}", &name)
                                    })?;
                                self.emit_function_call(
                                    codes,
                                    *index as u32,
                                    func,
                                    &list[1..],
                                    env,
                                )?
                            }
                        }
                    }
                    AST::Module(_)
                    | AST::NumberLiteral(_)
                    | AST::BoolLiteral(_)
                    | AST::SymbolWithAnnotation(_, _)
                    | AST::List(_)
                    | AST::Vector(_) => bail!(
                        "Only list starts with symbol and intrinsic operators can be evaluated"
                    ),
                })
            }
            _ => bail!("Invalid argument. emit_list only accepts AST::List"),
        }
    }
    fn emit_obj(
        &mut self,
        codes: &mut Vec<OpCode>,
        ast: &AST,
        env: Rc<RefCell<Env>>,
    ) -> Result<Rc<Type>> {
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
            AST::BoolLiteral(b) => {
                codes.push(OpCode::I32Const(if *b { 1 } else { 0 }));
                return Ok(Rc::new(Type::Bool));
            }
            AST::Symbol(name) => match (*env.clone()).borrow().get(name) {
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
    fn emit_func(&mut self, ast: &AST, env: Rc<RefCell<Env>>) -> Result<()> {
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
                        AST::Symbol(s) => (*s, &TypeAST::Unit),
                        _ => bail!("A symbol with type annotaion is expected after 'defn'"),
                    };
                    let mut args = Vec::new();
                    match &slice[1] {
                        AST::Vector(list) => {
                            for arg in list {
                                args.push(match arg {
                                    AST::SymbolWithAnnotation(name, type_ast) => (*name, type_ast),
                                    _ => bail!(
                                        "Function argument should be a symbol annotated with ':'"
                                    ),
                                });
                            }
                        }
                        _ => bail!("Function args vector is required after 'defn'"),
                    };
                    let forms = Vec::from(&slice[2..]);
                    (is_export, name, type_ast, args, forms)
                }
                _ => todo!(),
            };

            // TODO: Impl type symbol functionality
            let empty_type_env = TypeEnv::default();

            let func_index = (*self.module.functions.clone()).borrow().len();
            let new_env = Rc::new(RefCell::new(Env::extend(env.clone())));
            let mut local_index = 0;
            for arg in &args {
                new_env.borrow_mut().set(
                    arg.0,
                    Variable {
                        pointer: Pointer::Local(local_index),
                        t: resolve_type(arg.1, &empty_type_env),
                    },
                );
                local_index += 1;
            }

            // Resolve arg types and func return type
            let arg_types = args
                .iter()
                .map(|(_, type_ast)| resolve_type(type_ast, &empty_type_env))
                .collect::<Vec<_>>();
            let result_type = resolve_type(result_type_ast, &empty_type_env);

            let mut func_body = Vec::new();

            let scope_result_type = self.emit_scope(&mut func_body, &forms, new_env)?;

            if *result_type == Type::Unit {
                let stack_cnt = get_primitive_types(scope_result_type).len();
                // Drop unused result
                for _ in 0..stack_cnt {
                    func_body.push(OpCode::Drop);
                }
            } else if *scope_result_type != *result_type {
                // Validate return type
                bail!(
                    "mismatched return type. Expected `{:?}`, but found `{:?}`",
                    result_type_ast,
                    scope_result_type,
                )
            }

            func_body.push(OpCode::End);

            let signature = Signature {
                sig_type: SignatureType::Func,
                params: arg_types
                    .iter()
                    .flat_map(|types| {
                        get_primitive_types(types.clone())
                            .iter()
                            .filter(|x| x.is_some())
                            .map(|x| x.unwrap())
                            .collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>(),
                results: get_primitive_types(resolve_type(result_type_ast, &empty_type_env))
                    .iter()
                    .filter(|x| x.is_some())
                    .map(|x| x.unwrap())
                    .collect(),
            };
            let signature_index = match self.module.signatures.get(&signature) {
                Some(index) => *index,
                None => {
                    let index = self.module.signatures.len() as u16;
                    self.module.signatures.insert(signature.clone(), index);
                    index
                }
            };

            self.module.functions.borrow_mut().insert(
                name.to_string(),
                (
                    func_index,
                    Function {
                        arg_types,
                        result_type,
                        signature_index: signature_index as u32,
                        body: func_body,
                    },
                ),
            );

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
        self.emit_func(ast, Rc::new(RefCell::new(Env::default())))
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
        let module_ast = parse_source(source)?;
        self.emit_module(&module_ast)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arithmetic_ops() {
        let mut module: Module = Module::default();
        let mut emitter = Emitter::new(&mut module);
        emitter
            .emit(
                "(defn calc : f32
                    [a : f32 b : i32]
                        (* 10 (/ (+ a (- b 1)) 2)))
                        
                    (defn sum: f32 []
                        (+ 10 20 30.0))
                    (defn sub: f32 []
                        (- 10.0 20 30))
                    (defn mul: f32 []
                        (* 10 20.0 30))
                    (defn div: f32 []
                        (/ 10 20 30.0))",
            )
            .unwrap();
        assert_eq!(module.exports, []);
        assert_eq!(module.signatures.len(), 2);
        assert_eq!(
            *module.signatures.keys().next().unwrap(),
            Signature {
                sig_type: SignatureType::Func,
                params: vec![WasmPrimitiveType::F32, WasmPrimitiveType::I32],
                results: vec![WasmPrimitiveType::F32]
            }
        );
        let module_functions = module.functions.borrow_mut();
        assert_eq!(
            module_functions["calc"].1,
            Function {
                arg_types: vec![Rc::new(Type::F32), Rc::new(Type::I32)],
                result_type: Rc::new(Type::F32),
                signature_index: 0,
                body: vec![
                    OpCode::I32Const(10),
                    OpCode::F32ConvertI32S,
                    OpCode::LocalGet(0),
                    OpCode::LocalGet(1),
                    OpCode::I32Const(1),
                    OpCode::I32Sub,
                    OpCode::F32ConvertI32S,
                    OpCode::F32Add,
                    OpCode::I32Const(2),
                    OpCode::F32ConvertI32S,
                    OpCode::F32Div,
                    OpCode::F32Mul,
                    OpCode::End
                ]
            }
        );
        assert_eq!(
            module_functions["sum"].1.body,
            vec![
                OpCode::I32Const(10),
                OpCode::I32Const(20),
                OpCode::I32Add,
                OpCode::F32ConvertI32S,
                OpCode::F32Const(30.0),
                OpCode::F32Add,
                OpCode::End
            ]
        );
        assert_eq!(
            module_functions["sub"].1.body,
            vec![
                OpCode::F32Const(10.0),
                OpCode::I32Const(20),
                OpCode::F32ConvertI32S,
                OpCode::F32Sub,
                OpCode::I32Const(30),
                OpCode::F32ConvertI32S,
                OpCode::F32Sub,
                OpCode::End
            ]
        );
        assert_eq!(
            module_functions["mul"].1.body,
            vec![
                OpCode::I32Const(10),
                OpCode::F32ConvertI32S,
                OpCode::F32Const(20.0),
                OpCode::F32Mul,
                OpCode::I32Const(30),
                OpCode::F32ConvertI32S,
                OpCode::F32Mul,
                OpCode::End
            ]
        );
        assert_eq!(
            module_functions["div"].1.body,
            vec![
                OpCode::I32Const(10),
                OpCode::I32Const(20),
                OpCode::I32DivS,
                OpCode::F32ConvertI32S,
                OpCode::F32Const(30.0),
                OpCode::F32Div,
                OpCode::End
            ]
        )
    }
    #[test]
    fn test_unary_ops() {
        let mut module: Module = Module::default();
        let mut emitter = Emitter::new(&mut module);
        emitter
            .emit(
                "(defn neg_f32: f32
                            [n: f32]
                                (- n))
                        (defn neg_i32: i32
                            [n: i32]
                                (- n))",
            )
            .unwrap();
        assert_eq!(
            module.functions.borrow_mut()["neg_f32"].1.body,
            vec![OpCode::LocalGet(0), OpCode::F32Neg, OpCode::End]
        );
        let module_functions = module.functions.borrow_mut();
        assert_eq!(
            module_functions["neg_i32"].1.body,
            vec![
                OpCode::LocalGet(0),
                OpCode::I32Const(-1),
                OpCode::I32Xor,
                OpCode::End
            ]
        )
    }
    #[test]
    fn test_export() {
        let mut module: Module = Module::default();
        let mut emitter = Emitter::new(&mut module);
        emitter
            .emit(
                "(export defn addTwo : i32
                    [a : i32 b: i32]
                        (+ a b))",
            )
            .unwrap();
        assert_eq!(
            module.exports,
            vec![Export {
                export_type: ExportKind::Func,
                func_index: 0,
                name: "addTwo".to_string()
            }]
        )
    }
    #[test]
    fn test_drop() {
        let mut module = Module::default();
        let mut emitter = Emitter::new(&mut module);
        emitter
            .emit(
                "
            (defn add_two: i32 [a: i32, b: i32] (+ a b))
            (defn add_two_and_discard [a: i32, b: i32] (+ a b))
            (defn discard_each_form: i32 []
                (add_two 1 2)
                (add_two 1 2)
                (add_two_and_discard 1 2)
                (add_two 1 2))
        ",
            )
            .unwrap();
        let module_functions = module.functions.borrow_mut();
        assert_eq!(
            module_functions["add_two"].1.body,
            vec![
                OpCode::LocalGet(0),
                OpCode::LocalGet(1),
                OpCode::I32Add,
                OpCode::End
            ]
        );
        assert_eq!(
            module_functions["add_two_and_discard"].1.body,
            vec![
                OpCode::LocalGet(0),
                OpCode::LocalGet(1),
                OpCode::I32Add,
                OpCode::Drop,
                OpCode::End
            ]
        );
        assert_eq!(
            module_functions["discard_each_form"].1.body,
            vec![
                OpCode::I32Const(1),
                OpCode::I32Const(2),
                OpCode::Call(0),
                OpCode::Drop,
                OpCode::I32Const(1),
                OpCode::I32Const(2),
                OpCode::Call(0),
                OpCode::Drop,
                OpCode::I32Const(1),
                OpCode::I32Const(2),
                OpCode::Call(1),
                OpCode::I32Const(1),
                OpCode::I32Const(2),
                OpCode::Call(0),
                OpCode::End,
            ]
        );
    }
    #[test]
    fn test_function_call() {
        let mut module = Module::default();
        let mut emitter = Emitter::new(&mut module);
        emitter
            .emit(
                "
            (defn addTwo: i32 [a: i32, b: i32] (+ a b) )
            (export defn main []
                (addTwo 10 20))
        ",
            )
            .unwrap();
        let module_functions = module.functions.borrow_mut();
        assert_eq!(
            module_functions["main"].1.body,
            vec![
                OpCode::I32Const(10),
                OpCode::I32Const(20),
                OpCode::Call(0),
                OpCode::Drop,
                OpCode::End
            ]
        )
    }
    #[test]
    fn test_let() {
        let mut module = Module::default();
        let mut emitter = Emitter::new(&mut module);
        emitter
            .emit(
                "
            (defn addTwo: i32 []
                (let [a 10
                      b 20]
                    (+ a b)))
        ",
            )
            .unwrap();
        let module_functions = module.functions.borrow_mut();
        assert_eq!(
            module_functions["addTwo"].1.body,
            vec![
                OpCode::I32Const(10),
                OpCode::LocalDecl(WasmPrimitiveType::I32),
                OpCode::LocalSet(0),
                OpCode::I32Const(20),
                OpCode::LocalDecl(WasmPrimitiveType::I32),
                OpCode::LocalSet(1),
                OpCode::LocalGet(0),
                OpCode::LocalGet(1),
                OpCode::I32Add,
                OpCode::End
            ]
        )
    }
    #[test]
    fn test_bool() {
        let mut module = Module::default();
        let mut emitter = Emitter::new(&mut module);
        emitter
            .emit(
                "
        (defn get-true: bool []
            (> 20 10))
        (defn get-false: bool []
            (< 20 10))
        (defn get-true2: bool []
            (>= 20 10))
        (defn get-false2: bool []
            (<= 20 10))
        (defn get-true3: bool []
            (>= 20 10 5))
        (defn get-false3: bool []
            (<= 20 10 5))
        (defn check-and: bool []
            (and true true false))
        (defn check-or: bool []
            (or true true false))
        ",
            )
            .unwrap();
        let functions = module.functions.borrow_mut();
        assert_eq!(
            functions["get-true"].1.body,
            vec![
                OpCode::I32Const(20),
                OpCode::I32Const(10),
                OpCode::I32GtS,
                OpCode::End
            ]
        );
        assert_eq!(
            functions["get-false"].1.body,
            vec![
                OpCode::I32Const(20),
                OpCode::I32Const(10),
                OpCode::I32LtS,
                OpCode::End
            ]
        );
        assert_eq!(
            functions["get-true2"].1.body,
            vec![
                OpCode::I32Const(20),
                OpCode::I32Const(10),
                OpCode::I32GeS,
                OpCode::End
            ]
        );
        assert_eq!(
            functions["get-false2"].1.body,
            vec![
                OpCode::I32Const(20),
                OpCode::I32Const(10),
                OpCode::I32LeS,
                OpCode::End
            ]
        );
        assert_eq!(
            functions["get-true3"].1.body,
            vec![
                OpCode::I32Const(20),
                OpCode::I32Const(10),
                OpCode::I32GeS,
                OpCode::I32Const(10),
                OpCode::I32Const(5),
                OpCode::I32GeS,
                OpCode::I32And,
                OpCode::End
            ]
        );
        assert_eq!(
            functions["get-false3"].1.body,
            vec![
                OpCode::I32Const(20),
                OpCode::I32Const(10),
                OpCode::I32LeS,
                OpCode::I32Const(10),
                OpCode::I32Const(5),
                OpCode::I32LeS,
                OpCode::I32And,
                OpCode::End
            ]
        );
    }
    #[test]
    fn test_if() {
        let mut module = Module::default();
        let mut emitter = Emitter::new(&mut module);
        emitter
            .emit(
                "
        (defn check-if: i32 []
            (if true
                1
                2))
        (defn check-if-2: f32 []
            (if false
                1.0
                2.0))
        (defn check-if-3: bool []
            (if (> 2 1)
                true
                false))
        ",
            )
            .unwrap();
        let functions = module.functions.borrow_mut();
        assert_eq!(functions["check-if"].1.body, vec![
            OpCode::I32Const(1),
            OpCode::If(Some(WasmPrimitiveType::I32)),
            OpCode::I32Const(1),
            OpCode::Else,
            OpCode::I32Const(2),
            OpCode::End,
            OpCode::End
        ]);
        assert_eq!(functions["check-if-2"].1.body, vec![
            OpCode::I32Const(0),
            OpCode::If(Some(WasmPrimitiveType::F32)),
            OpCode::F32Const(1.0),
            OpCode::Else,
            OpCode::F32Const(2.0),
            OpCode::End,
            OpCode::End
        ]);
        assert_eq!(functions["check-if-3"].1.body, vec![
            OpCode::I32Const(2),
            OpCode::I32Const(1),
            OpCode::I32GtS,
            OpCode::If(Some(WasmPrimitiveType::I32)),
            OpCode::I32Const(1),
            OpCode::Else,
            OpCode::I32Const(0),
            OpCode::End,
            OpCode::End
        ]);
    }
}
