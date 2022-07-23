use super::*;
use crate::{env::Env, parser::AST, resolver::Type};
use anyhow::{bail, ensure, Context, Result};
use std::{cell::RefCell, rc::Rc};

pub(super) fn emit_global(
    module: &mut Module,
    forms: &[AST],
    is_mutable: bool,
    env: Rc<RefCell<Env>>,
) -> Result<()> {
    ensure!(forms.len() == 2, "expect two arugments");
    let (name, t, value_ast) = match &forms[0] {
        AST::Symbol(_) => bail!("define symbol should be annotated."),
        AST::SymbolWithAnnotation(name, t) => (*name, t, &forms[1]),
        _ => bail!("hoge"),
    };

    // TODO: Impl type symbol functionality
    let empty_type_env = TypeEnv::default();
    let resolved_type = resolve_type(t, &empty_type_env)?;
    let mut globals = module.globals.borrow_mut();
    let index = globals.len() as u32;

    let value = match *resolved_type {
        Type::I32 => match value_ast {
            AST::NumberLiteral(numstr) => GlobalValue::I32(
                numstr
                    .parse::<i32>()
                    .with_context(|| format!("failed to parse number"))?,
            ),
            _ => bail!("number literal expected"),
        },
        Type::F32 => match value_ast {
            AST::NumberLiteral(numstr) => GlobalValue::F32(
                numstr
                    .parse::<f32>()
                    .with_context(|| format!("failed to parse nubmer"))?,
            ),
            _ => bail!("number literal expected"),
        },
        Type::Bool => match value_ast {
            AST::BoolLiteral(b) => GlobalValue::I32(if *b { 1 } else { 0 }),
            _ => bail!("bool literal expected"),
        },
        Type::Unit => bail!("Only primitive literals are supported for global variable for now"),
        Type::Array(_) => todo!(),
    };
    globals.insert(name.to_string(), (index, Global { is_mutable, value }));

    env.borrow_mut().set(
        name,
        Variable {
            pointer: Pointer::Global(index as u32),
            t: resolved_type,
        },
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_simple_define() {
        let source = "
        (define num: i32 10)
        (define num2: f32 10.0)
        (define bool: bool true)
        (define bool2: bool false)
        (defmut num_mut: i32 10)
        (defmut num2_mut: f32 10.0)
        (defmut bool_mut: bool true)
        (defmut bool2_mut: bool false)
        ";
        let module = &mut Module::default();
        emit(module, source).unwrap();
        let globals = module.globals.borrow();
        assert_eq!(
            globals["num"].1,
            Global {
                is_mutable: false,
                value: GlobalValue::I32(10)
            }
        );
        assert_eq!(
            globals["num2"].1,
            Global {
                is_mutable: false,
                value: GlobalValue::F32(10.0)
            }
        );
        assert_eq!(
            globals["bool"].1,
            Global {
                is_mutable: false,
                value: GlobalValue::I32(1)
            }
        );
        assert_eq!(
            globals["bool2"].1,
            Global {
                is_mutable: false,
                value: GlobalValue::I32(0)
            }
        );
        assert_eq!(
            globals["num_mut"].1,
            Global {
                is_mutable: true,
                value: GlobalValue::I32(10)
            }
            
        );
        assert_eq!(
            globals["num2_mut"].1,
            Global {
                is_mutable: true,
                value: GlobalValue::F32(10.0)
            }
        );
        assert_eq!(
            globals["bool_mut"].1,
            Global {
                is_mutable: true,
                value: GlobalValue::I32(1)
            }
        );
        assert_eq!(
            globals["bool2_mut"].1,
            Global {
                is_mutable: true,
                value: GlobalValue::I32(0)
            }
        );
    }

    #[test]
    fn test_env() {
        let module = &mut Module::default();
        let env = Env::create();
        emit_global(
            module,
            &[
                AST::SymbolWithAnnotation("hoge", TypeAST::I32),
                AST::NumberLiteral("10"),
            ],
            false,
            env.clone(),
        )
        .unwrap();
        let hoge = env.borrow().get("hoge").unwrap();
        assert_eq!(
            hoge,
            Variable {
                t: Rc::new(Type::I32),
                pointer: Pointer::Global(0)
            }
        )
    }

    #[test]
    fn test_get_global() {
        let source = "
        (define num: i32 10)
        (defn get-global:i32
            []
            num)
        ";
        let mut module = Module::default();
        emit(&mut module, source).unwrap();
        let functions = module.functions.borrow_mut();
        let globals = module.globals.borrow();
        assert_eq!(
            functions["get-global"].1.body,
            vec![OpCode::GlobalGet(globals["num"].0), OpCode::End]
        )
    }
}
