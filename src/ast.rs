#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinType {
    String,
    Float,
    Vector,
    Entity,
    Void,
}
#[derive(Debug, Clone)]
pub struct Argument<'a> {
    pub name: &'a str,
    pub ty: Type<'a>,
}

#[derive(Debug, Clone)]
pub enum Type<'a> {
    Builtin(BuiltinType),
    Function {
        return_type: BuiltinType,
        arguments: Vec<Argument<'a>>,
    },
    FieldReference(Box<Type<'a>>),
    Pointer(Box<Type<'a>>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Equals,
    NotEquals,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PrefixOp {
    Neg,
    Not,
}

impl InfixOp {
    // Implemented as in C, but that is probably not correct (as pointed out by @4LT)
    // https://en.cppreference.com/w/c/language/operator_precedence
    pub fn precedence(self) -> i32 {
        match self {
            InfixOp::Or => -5,
            InfixOp::And => -4,
            InfixOp::BitwiseOr => -3,
            InfixOp::BitwiseXor => -2,
            InfixOp::BitwiseAnd => -1,
            InfixOp::Equals | InfixOp::NotEquals => 0,
            InfixOp::Add | InfixOp::Sub => 1,
            InfixOp::Mul | InfixOp::Div => 2,
        }
    }

    pub fn is_associative(self) -> bool {
        !matches!(self, InfixOp::Sub | InfixOp::Div)
    }

    pub fn is_left_associative(self) -> bool {
        // Change when any non-left associative operators are implemented
        true
    }

    pub fn is_right_associative(self) -> bool {
        self.is_associative() || !self.is_left_associative()
    }
}

#[derive(Debug)]
pub enum Expression<'a> {
    String(&'a str),
    Number(f32),
    Vector(f32, f32, f32),
    Identifier(&'a str),
    Infix(InfixOp, Box<(Expression<'a>, Expression<'a>)>),
    Prefix(PrefixOp, Box<Expression<'a>>),
    Call(Box<Expression<'a>>, Vec<Expression<'a>>),
    FieldAccess(Box<Expression<'a>>, &'a str),
}

#[derive(Debug)]
pub enum Statement<'a> {
    Block(Vec<Statement<'a>>),
    Expression(Expression<'a>),
    Assignment {
        lvalue: Expression<'a>,
        rvalue: Expression<'a>,
    },
    Decl(Declaration<'a>),
    Newline,
}

#[derive(Debug)]
pub struct Block<'a>(pub Vec<Statement<'a>>);

#[derive(Debug, Clone, Copy)]
pub enum BindingModifier {
    Const,
    Var,
    Nosave,
}

#[derive(Debug)]
pub enum BindingInitializer<'a> {
    Expr(Expression<'a>),
    Block(Block<'a>),
    BuiltinReference(u32),
}

#[derive(Debug)]
pub enum Declaration<'a> {
    Newline,
    Field {
        name: &'a str,
        ty: Type<'a>,
    },
    Binding {
        name: &'a str,
        ty: Type<'a>,
        modifiers: Vec<BindingModifier>,
        initializer: Option<BindingInitializer<'a>>,
    },
}
