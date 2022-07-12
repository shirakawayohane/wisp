use std::{collections::HashMap, rc::Rc};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Pointer {
    Local(u8),
}

#[derive(Debug, PartialEq, Default)]
pub struct Env {
    parent: Option<Rc<Env>>,
    vars: HashMap<String, Pointer>,
}

impl Env {
    pub fn extend(parent: Rc<Self>) -> Env {
        Env {
            vars: HashMap::new(),
            parent: Some(parent),
        }
    }
    pub fn get(&self, name: &str) -> Option<Pointer> {
        match self.vars.get(name) {
            Some(value) => Some(*value),
            None => self.parent.as_ref().and_then(|o| o.clone().get(name)),
        }
    }

    pub fn set(&mut self, name: &str, val: Pointer) {
        self.vars.insert(name.to_string(), val);
    }
}
