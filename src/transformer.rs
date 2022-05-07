// WIP!

pub enum Type {
    F32,
    /*
    Object([(String, &Type)])
    */
}

pub struct Param<'a> {
    name: String,
    type_ref: &'a Type,
}

pub enum FunctionBody {
    VariableDecl,
    FunctionCall,
    SymbolReference,
}

pub struct Function<'a> {
    pub name: String,
    pub params: &'a [Param<'a>],
    pub body: &'a [Vec<FunctionBody>],
}

pub struct SemanticModel<'a> {
    pub types: Vec<&'a Type>,
    pub functions: Vec<Function<'a>>,
}
