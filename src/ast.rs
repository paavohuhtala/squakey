#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinType {
    String,
    Float,
    Vector,
    Entity,
    Void,
}
#[derive(Debug)]
pub struct Argument<'a> {
    pub name: &'a str,
    pub ty: Type<'a>,
}

#[derive(Debug)]
pub enum Type<'a> {
    Builtin(BuiltinType),
    Function {
        return_type: BuiltinType,
        arguments: Vec<Argument<'a>>,
    },
}

#[derive(Debug)]
pub enum Declaration<'a> {
    Newline,
    Field { name: &'a str, ty: Type<'a> },
}
