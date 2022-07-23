use std::{cell::{RefCell, Cell}, collections::HashMap, rc::Rc};

use crate::resolver::Type;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Pointer {
    Local(u32),
    Global(u32),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable {
    pub pointer: Pointer,
    pub t: Rc<Type>,
}

#[derive(Debug, PartialEq, Default)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    vars: HashMap<String, Variable>,
    pub stack_cnt: Cell<u32>
}

impl Env {
    pub fn extend(parent: Rc<RefCell<Self>>) -> Env {
        Env {
            vars: HashMap::new(),
            parent: Some(parent),
            stack_cnt: Cell::new(0)
        }
    }
    pub fn get(&self, name: &str) -> Option<Variable> {
        match self.vars.get(name) {
            Some(value) => Some(value.clone()),
            None => self
                .parent
                .as_ref()
                .and_then(|o| (*o.clone()).borrow().get(name).clone()),
        }
    }

    pub fn set(&mut self, name: &str, val: Variable) -> Option<Variable> {
        self.vars.insert(name.to_string(), val)
    }

    pub fn count_local_vars(&self) -> usize {
        let mut ret = self
            .vars
            .iter()
            .filter(|(_, v)| {
                match v.pointer {
                    Pointer::Local(_) => true,
                    Pointer::Global(_) => false,
                }
            })
            .count();
        if let Some(n) = self
            .parent
            .as_ref()
            .and_then(|p| Some((*p.clone()).borrow().count_local_vars()))
        {
            ret += n;
        }
        ret
    }

    pub fn create() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Env::default()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;

    #[test]
    fn test_count_local_vars() {
        let env = Rc::new(RefCell::new(Env::default()));
        env.borrow_mut().set("a", Variable { pointer: Pointer::Local(0), t: Rc::new(Type::I32) });
        env.borrow_mut().set("b", Variable { pointer: Pointer::Local(1), t: Rc::new(Type::I32) });
        let new_env = Rc::new(RefCell::new(Env::extend(env)));
        new_env.borrow_mut().set("c", Variable { pointer: Pointer::Local(2), t: Rc::new(Type::I32) });
        assert_eq!(new_env.borrow().count_local_vars(), 3);
    }
}