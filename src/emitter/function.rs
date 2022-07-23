use super::*;
use crate::{env::Env, parser::AST, resolver::Type};
use anyhow::{bail, ensure, Result};
use std::{cell::RefCell, rc::Rc};

pub(super) fn emit_func(module: &mut Module, ast: &AST, env: Rc<RefCell<Env>>) -> Result<()> {
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
                    ensure!(
                        s == "defn",
                        "Failed to compile function. func list must start with 'export' or 'defn'"
                    );
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
                                _ => {
                                    bail!("Function argument should be a symbol annotated with ':'")
                                }
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

        let func_index = (*module.functions.clone()).borrow().len();
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

        let scope_result_type = emit_scope(module, &mut func_body, &forms, new_env)?;

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
        let signature_index = match module.signatures.get(&signature) {
            Some(index) => *index,
            None => {
                let index = module.signatures.len() as u16;
                module.signatures.insert(signature.clone(), index);
                index
            }
        };

        module.functions.borrow_mut().insert(
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
            module.exports.push(Export {
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_drop() {
        let module = &mut Module::default();
        emit(
            module,
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
    fn test_export() {
        let module = &mut Module::default();
        emit(
            module,
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
}
