use std::{collections::HashMap, rc::Rc};

use crate::resolver::Type;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Pointer {
    Local(u8),
}

#[derive(Debug, PartialEq)]
pub struct  Variable {
    pub pointer: Pointer,
    pub t: Rc<Type>
}

#[derive(Debug, PartialEq, Default)]
pub struct Env {
    parent: Option<Rc<Env>>,
    vars: HashMap<String, Variable>,
}

impl Env {
    pub fn extend(parent: Rc<Self>) -> Env {
        Env {
            vars: HashMap::new(),
            parent: Some(parent),
        }
    }
    pub fn get(&self, name: &str) -> Option<&Variable> {
        match self.vars.get(name) {
            Some(value) => Some(value),
            None => self.parent.as_ref().and_then(|o| o.get(name)),
        }
    }

    pub fn set(&mut self, name: &str, val: Variable) {
        self.vars.insert(name.to_string(), val);
    }
}
