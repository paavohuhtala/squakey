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
pub enum Expression<'a> {
    String(&'a str),
    Number(f32),
    Vector(f32, f32, f32),
    Identifier(&'a str),
}

#[derive(Debug)]
pub enum Statement<'a> {
    Block(Vec<Statement<'a>>),
    Expression(Expression<'a>),
    Assignment {
        lvalue: Expression<'a>,
        rvalue: Expression<'a>,
    },
}

#[derive(Debug)]
pub enum Declaration<'a> {
    Newline,
    Field {
        name: &'a str,
        ty: Type<'a>,
    },
    Function {
        name: &'a str,
        ty: Type<'a>,
        body: Option<Vec<Statement<'a>>>,
    },
}
