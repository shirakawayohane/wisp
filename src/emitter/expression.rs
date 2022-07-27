use super::{
    intrinsic_ops::emit_intrinsic_exp,
    special_forms::{emit_if, emit_let},
    vector::*,
    *,
};
use crate::{env::Env, parser::AST, resolver::Type};
use anyhow::{bail, Context, Result, ensure};
use std::{cell::RefCell, rc::Rc};

pub(super) fn emit_list(
    module: &mut Module,
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
                    emit_intrinsic_exp(module, op, codes, &list[1..], env)?
                }
                AST::Symbol(name) => {
                    match *name {
                        "let" => emit_let(module, codes, ast, env)?,
                        "if" => emit_if(module, codes, ast, env)?,
                        _ => {
                            if let Some(var) = env.clone().borrow().get(name) {
                                match *var.t {
                                    Type::I32 => match var.pointer {
                                        Pointer::Local(addr) => {
                                            codes.push(OpCode::LocalGet(addr));
                                            emit_index_get(module, codes, &list[1], env)?
                                        }
                                        Pointer::Global(addr) => {
                                            codes.push(OpCode::GlobalGet(addr));
                                            emit_index_get(module, codes, &list[1], env)?
                                        }
                                    },
                                    _ => bail!("{} cannot be evaluated as function", name)
                                }
                            } else {
                                // emit function call
                                let module_functions = module.functions.clone();
                                let module_func_refmut = module_functions.borrow_mut();
                                let (index, func) =
                                    module_func_refmut.get(*name).with_context(|| {
                                        format!("Unable to find function {:?}", &name)
                                    })?;
                                emit_function_call(
                                    module,
                                    codes,
                                    *index as u32,
                                    func,
                                    &list[1..],
                                    env.clone(),
                                )?
                            }
                        }
                    }
                }
                AST::NumberLiteral(numstr) => {
                    let index = numstr.parse::<u32>().map_err(|_| anyhow!("list beggining with floating point cannot be evaluated"))?;
                    ensure!(list.len() == 2, "expect 2 forms, found {}", list.len());
                    codes.push(OpCode::I32Const(index as i32));
                    emit_index_get(module, codes, &list[1], env)?
                }
                AST::Module(_)
                | AST::BoolLiteral(_)
                | AST::SymbolWithAnnotation(_, _)
                | AST::List(_)
                | AST::Vector(_) => {
                    bail!("Only list starts with symbol and intrinsic operators can be evaluated")
                }
            })
        }
        _ => bail!("Invalid argument. emit_list only accepts AST::List"),
    }
}

pub(super) fn emit_function_call(
    module: &mut Module,
    codes: &mut Vec<OpCode>,
    index: u32,
    func: &Function,
    args: &[AST],
    env: Rc<RefCell<Env>>,
) -> Result<Rc<Type>> {
    for arg in args {
        emit_obj(module, codes, arg, env.clone())?;
    }
    codes.push(OpCode::Call(index as u32));
    Ok(func.result_type.clone())
}

pub(super) fn emit_obj(
    module: &mut Module,
    codes: &mut Vec<OpCode>,
    ast: &AST,
    env: Rc<RefCell<Env>>,
) -> Result<Rc<Type>> {
    match ast {
        AST::List(_) => return emit_list(module, codes, ast, env),
        AST::Vector(v) => emit_vector(module, codes, v, env),
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
                Pointer::Global(index) => {
                    codes.push(OpCode::GlobalGet(index));
                    return Ok(variable.t.clone());
                }
            },
        },
        _ => bail!("Cannot evaluate {:?}", ast),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_function_call() {
        let module = &mut Module::default();
        emit(
            module,
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
}
