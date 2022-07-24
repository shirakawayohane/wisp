use super::*;
use crate::{
    emitter::expression::emit_obj,
    env::Env,
    parser::AST,
    resolver::{get_size, Type},
};
use anyhow::{ensure, Result};
use std::{cell::RefCell, rc::Rc};

pub(super) fn emit_vector(
    module: &mut Module,
    codes: &mut Vec<OpCode>,
    items: &[AST],
    env: Rc<RefCell<Env>>,
) -> Result<Rc<Type>> {
    let mut offset: u32 = 0;
    let mut last_type: Option<Rc<Type>> = None;

    // TODO: Infer type by adding unknown type
    if items.len() == 0 {
        return Ok(Rc::new(Type::Array(Rc::new(Type::Unit))));
    }
    let stack_pointer_local_addr = codes.iter().filter(|x| 
        if let OpCode::LocalDecl(_) = x {true} else {false}
    ).count() as u32;
    codes.push(OpCode::LocalDecl(WasmPrimitiveType::I32));
    codes.push(OpCode::GlobalGet(STACK_POINTER.0));
    codes.push(OpCode::LocalTee(stack_pointer_local_addr));
    codes.push(OpCode::I32Const(items.len() as i32));
    codes.push(OpCode::I32Store {
        offset,
        alignment: 2,
    });
    offset += 4 as u32;

    for item in items {
        codes.push(OpCode::LocalGet(stack_pointer_local_addr));
        let current_type = emit_obj(module, codes, item, env.clone())?;
        if last_type.is_some() {
            ensure!(
                *last_type.clone().unwrap() == *current_type,
                "mismatch types of array element. expected {}, found {}",
                *last_type.clone().unwrap(),
                *current_type
            )
        } else {
            last_type = Some(current_type.clone());
        }
        // TODO: Consider alignment
        match *current_type {
            Type::I32 => codes.push(OpCode::I32Store {
                offset,
                alignment: 2,
            }),
            Type::F32 => codes.push(OpCode::F32Store {
                offset,
                alignment: 2,
            }),
            Type::Bool => codes.push(OpCode::I32Store8 {
                offset,
                alignment: 0,
            }),
            Type::Unit => (),
            Type::Array(_) => codes.push(OpCode::I32Store {
                offset,
                alignment: 2,
            }),
        }
        offset += get_size(current_type.clone());
    }
    let stack_cnt = &env.borrow().stack_cnt;
    stack_cnt.set(stack_cnt.get() + offset);
    codes.push(OpCode::LocalGet(stack_pointer_local_addr));
    codes.push(OpCode::I32Const(offset as i32));
    codes.push(OpCode::I32Add);
    codes.push(OpCode::GlobalSet(STACK_POINTER.0));
    codes.push(OpCode::I32Const(stack_pointer_local_addr as i32));
    Ok(Rc::new(Type::Array(last_type.unwrap())))
}

pub(super) fn emit_index_get(
    module: &mut Module,
    codes: &mut Vec<OpCode>,
    index: u32,
    vec_ast: &AST,
    env: Rc<RefCell<Env>>,
) -> Result<Rc<Type>> {
    let vec_type = emit_obj(module, codes, vec_ast, env.clone())?;
    match &*vec_type {
        Type::I32 => {
            codes.push(OpCode::I32Load {
                offset: 4 * index,
                alignment: 2,
            });
            Ok(Rc::new(Type::I32))
        }
        Type::F32 => {
            codes.push(OpCode::F32Load {
                offset: 4 * index,
                alignment: 2,
            });
            Ok(Rc::new(Type::F32))
        }
        Type::Bool => {
            codes.push(OpCode::I32Load8U {
                offset: index,
                alignment: 0,
            });
            Ok(Rc::new(Type::I32))
        }
        Type::Unit => Ok(Rc::new(Type::Unit)),
        Type::Array(elm_type) => {
            codes.push(OpCode::I32Load {
                offset: 4 * index,
                alignment: 2,
            });
            Ok(elm_type.clone())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_creating_vector() {
        let module = &mut Module::default();
        let source = "
        (defn test_vector []
            [1,2,3])
        ";
        emit(module, source).unwrap();
        let function = &module.functions.borrow()["test_vector"].1.body;
        assert_eq!(
            *function,
            vec![
                OpCode::LocalDecl(WasmPrimitiveType::I32),
                OpCode::GlobalGet(STACK_POINTER.0),
                OpCode::LocalTee(0),
                OpCode::I32Const(3),
                OpCode::I32Store {
                    offset: 0,
                    alignment: 2
                },
                OpCode::LocalGet(0),
                OpCode::I32Const(1),
                OpCode::I32Store {
                    offset: 4,
                    alignment: 2
                },
                OpCode::LocalGet(0),
                OpCode::I32Const(2),
                OpCode::I32Store {
                    offset: 8,
                    alignment: 2
                },
                OpCode::LocalGet(0),
                OpCode::I32Const(3),
                OpCode::I32Store {
                    offset: 12,
                    alignment: 2
                },
                OpCode::LocalGet(0),
                OpCode::I32Const(16),
                OpCode::I32Add,
                OpCode::GlobalSet(0),
                OpCode::I32Const(0),
                OpCode::GlobalGet(STACK_POINTER.0),
                OpCode::I32Const(16),
                OpCode::I32Sub,
                OpCode::GlobalSet(0),
                OpCode::Drop,
                OpCode::End
            ]
        )
    }
    #[test]
    fn test_return_type() {
        let module = &mut Module::default();
        let codes = &mut Vec::new();
        let items = &[
            AST::NumberLiteral("1"),
            AST::NumberLiteral("2"),
            AST::NumberLiteral("3"),
        ];
        let env = Env::create();
        let result_type = emit_vector(module, codes, items, env).unwrap();
        assert_eq!(*result_type, Type::Array(Rc::new(Type::I32)));
    }
    #[test]
    fn test_array_arg() {
        let module = &mut Module::default();
        let source = "
        (defn first: i32 
            [arr: [i32]]
            (0 arr))
        ";
        emit(module, source).unwrap();
        let body = &module.functions.borrow()["first"].1.body;
        assert_eq!(
            *body,
            vec![
                OpCode::LocalGet(0),
                OpCode::I32Load {
                    offset: 0,
                    alignment: 2
                },
                OpCode::End
            ]
        )
    }
}
