use std::{cell::RefCell, rc::Rc, borrow::Borrow};
use crate::{parser::AST, resolver::Type, env::Env, emitter::expression::emit_obj};
use super::*;
use anyhow::{Result, ensure, bail};

pub(super) fn emit_scope(
    module: &mut Module,
    codes: &mut Vec<OpCode>,
    forms: &[AST],
    env: Rc<RefCell<Env>>,
) -> Result<Rc<Type>> {
    for (index, form) in forms.iter().enumerate() {
        let last = index == forms.len() - 1;
        if last {
            let result_type = emit_obj(module, codes, &form, env.clone())?;
            return Ok(result_type);
        } else {
            let emitted_type = emit_obj(module, codes, &form, env.clone())?;
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

pub(super) fn emit_let(
    module: &mut Module,
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
                        let value_type = emit_obj(module, codes, value, new_env.clone())?;
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
            return emit_scope(module, codes, forms, new_env);
        } else {
            bail!("A binding vector is expected after 'let'")
        }
    } else {
        unreachable!()
    }
}
pub(super) fn emit_if(
    module: &mut Module,
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
        *emit_obj(module, codes, bool_exp, env.clone())? == Type::Bool,
        "first form of if must be bool expression"
    );
    let temp_codes = &mut Vec::new();
    let true_form_type = emit_obj(module, temp_codes, true_exp, env.clone())?;
    temp_codes.push(OpCode::Else);
    let false_form_type = emit_obj(module, temp_codes, false_exp, env.clone())?;

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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let() {
        let module = &mut Module::default();
        emit(
            module,
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
    fn test_if() {
        let module = &mut Module::default();
        emit(
            module,
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
        assert_eq!(
            functions["check-if"].1.body,
            vec![
                OpCode::I32Const(1),
                OpCode::If(Some(WasmPrimitiveType::I32)),
                OpCode::I32Const(1),
                OpCode::Else,
                OpCode::I32Const(2),
                OpCode::End,
                OpCode::End
            ]
        );
        assert_eq!(
            functions["check-if-2"].1.body,
            vec![
                OpCode::I32Const(0),
                OpCode::If(Some(WasmPrimitiveType::F32)),
                OpCode::F32Const(1.0),
                OpCode::Else,
                OpCode::F32Const(2.0),
                OpCode::End,
                OpCode::End
            ]
        );
        assert_eq!(
            functions["check-if-3"].1.body,
            vec![
                OpCode::I32Const(2),
                OpCode::I32Const(1),
                OpCode::I32GtS,
                OpCode::If(Some(WasmPrimitiveType::I32)),
                OpCode::I32Const(1),
                OpCode::Else,
                OpCode::I32Const(0),
                OpCode::End,
                OpCode::End
            ]
        );
    }
}