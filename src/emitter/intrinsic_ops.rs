use std::{cell::RefCell, rc::Rc};
use crate::{parser::AST, resolver::Type, env::Env, emitter::expression::*};
use super::*;
use anyhow::{Result, ensure, bail};

pub(super) fn emit_intrinsic_exp(
    module: &mut Module,
    op: IntrinsicOperator,
    codes: &mut Vec<OpCode>,
    args: &[AST],
    env: Rc<RefCell<Env>>,
) -> Result<Rc<Type>> {
    ensure!(args.len() > 0);
    if args.len() == 1 {
        let arg = &args[0];
        match op {
            IntrinsicOperator::Add => match *emit_obj(module, codes, arg, env)? {
                Type::Bool | Type::Unit | Type::Array(_) => {
                    bail!("Invalid argument for unary op. expected numeric type")
                }
                Type::I32 => Ok(Rc::new(Type::I32)),
                Type::F32 => Ok(Rc::new(Type::F32)),
            },
            IntrinsicOperator::Sub => match *emit_obj(module, codes, arg, env)? {
                Type::Bool | Type::Unit | Type::Array(_) => {
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
            IntrinsicOperator::Mul => match *emit_obj(module, codes, arg, env)? {
                Type::Bool | Type::Unit | Type::Array(_) => {
                    bail!("Invalid argument for unary op. expected numeric type")
                }
                Type::I32 => Ok(Rc::new(Type::I32)),
                Type::F32 => Ok(Rc::new(Type::F32)),
            },
            IntrinsicOperator::Div => {
                codes.push(OpCode::F32Const(1.0));
                match *emit_obj(module, codes, arg, env)? {
                    Type::Bool | Type::Unit | Type::Array(_) => {
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
                let mut last_result = emit_obj(module, last_codes, &args[0], env.clone())?;
                for arg in &args[1..] {
                    let current_result = emit_obj(module, current_codes, arg, env.clone())?;
                    let opcode = match *last_result {
                        Type::Bool | Type::Unit | Type::Array(_) => bail!("hoge"),
                        Type::I32 => match *current_result {
                            Type::Bool | Type::Unit | Type::Array(_) => bail!("hoge"),
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
                            Type::Bool | Type::Unit | Type::Array(_) => bail!("hoge"),
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
                    let left_type = emit_obj(module, left_codes, left, env.clone())?;
                    let right_type = emit_obj(module, right_codes, right, env.clone())?;
                    match *left_type {
                        Type::Unit => bail!("Unit type cannot be compared."),
                        Type::Array(_) => bail!("Array type cannot be compared."),
                        Type::Bool => {
                            match op {
                                IntrinsicOperator::Eq => right_codes.push(OpCode::I32Eq),
                                IntrinsicOperator::And => right_codes.push(OpCode::I32And),
                                IntrinsicOperator::Or => right_codes.push(OpCode::I32Or),
                                IntrinsicOperator::Not => {
                                    right_codes.push(OpCode::I32Const(-1));
                                    right_codes.push(OpCode::I32Xor);
                                }
                                _ => {
                                    bail!("cannot compare types {} and {}", *left_type, *right_type)
                                }
                            };
                        }
                        Type::I32 => match *right_type {
                            Type::Bool | Type::Unit | Type::Array(_) => bail!("hoge"),
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
                            Type::Bool | Type::Unit | Type::Array(_) => bail!("hoge"),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arithmetic_ops() {
        let module = &mut Module::default();
        emit(
            module,
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
        let module = &mut Module::default();
        emit(
            module,
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
    fn test_bool() {
        let module = &mut Module::default();
        emit(
            module,
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
}