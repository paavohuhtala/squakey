use std::ops::Deref;

use pest::Span;
use strum::EnumIter;

use crate::grammar::Rule;

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
    pub ty: Node<'a, Type<'a>>,
}

#[derive(Debug)]
pub enum Type<'a> {
    Builtin(BuiltinType),
    Function {
        return_type: BuiltinType,
        arguments: Vec<Node<'a, Argument<'a>>>,
    },
    FieldReference(Box<Node<'a, Type<'a>>>),
    Pointer(Box<Node<'a, Type<'a>>>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, EnumIter)]
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
    LessThan,
    LessThanOrEquals,
    GreaterThan,
    GreaterThanOrEquals,
    // FTEQCC extension (><)
    CrossProduct,
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
            // TODO this is definitely wrong
            InfixOp::CrossProduct => -7,
            InfixOp::Or => -6,
            InfixOp::And => -5,
            InfixOp::BitwiseOr => -4,
            InfixOp::BitwiseXor => -3,
            InfixOp::BitwiseAnd => -2,
            InfixOp::Equals | InfixOp::NotEquals => -1,
            InfixOp::LessThan
            | InfixOp::LessThanOrEquals
            | InfixOp::GreaterThan
            | InfixOp::GreaterThanOrEquals => 0,
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

    pub fn from_rule(rule: Rule) -> Option<InfixOp> {
        match rule {
            Rule::add => Some(InfixOp::Add),
            Rule::sub => Some(InfixOp::Sub),
            Rule::mul => Some(InfixOp::Mul),
            Rule::div => Some(InfixOp::Div),
            Rule::and => Some(InfixOp::And),
            Rule::or => Some(InfixOp::Or),
            Rule::bitwise_and => Some(InfixOp::BitwiseAnd),
            Rule::bitwise_or => Some(InfixOp::BitwiseOr),
            Rule::bitwise_xor => Some(InfixOp::BitwiseXor),
            Rule::equals => Some(InfixOp::Equals),
            Rule::not_equals => Some(InfixOp::NotEquals),
            Rule::less_than => Some(InfixOp::LessThan),
            Rule::less_than_or_equals => Some(InfixOp::LessThanOrEquals),
            Rule::greater_than => Some(InfixOp::GreaterThan),
            Rule::greater_than_or_equals => Some(InfixOp::GreaterThanOrEquals),
            Rule::cross_product => Some(InfixOp::CrossProduct),
            _ => None,
        }
    }

    pub fn into_rule(self) -> Rule {
        match self {
            InfixOp::Add => Rule::add,
            InfixOp::Sub => Rule::sub,
            InfixOp::Mul => Rule::mul,
            InfixOp::Div => Rule::div,
            InfixOp::And => Rule::and,
            InfixOp::Or => Rule::or,
            InfixOp::BitwiseAnd => Rule::bitwise_and,
            InfixOp::BitwiseOr => Rule::bitwise_or,
            InfixOp::BitwiseXor => Rule::bitwise_xor,
            InfixOp::Equals => Rule::equals,
            InfixOp::NotEquals => Rule::not_equals,
            InfixOp::LessThan => Rule::less_than,
            InfixOp::LessThanOrEquals => Rule::less_than_or_equals,
            InfixOp::GreaterThan => Rule::greater_than,
            InfixOp::GreaterThanOrEquals => Rule::greater_than_or_equals,
            InfixOp::CrossProduct => Rule::cross_product,
        }
    }
}

pub type ExpressionNode<'a> = Node<'a, Expression<'a>>;

#[derive(Debug)]
pub enum Expression<'a> {
    String(&'a str),
    Number(f32),
    Vector(f32, f32, f32),
    Identifier(&'a str),
    Infix(InfixOp, Box<(ExpressionNode<'a>, ExpressionNode<'a>)>),
    Prefix(PrefixOp, Box<ExpressionNode<'a>>),
    Call(Box<ExpressionNode<'a>>, Vec<ExpressionNode<'a>>),
    FieldAccess(Box<ExpressionNode<'a>>, &'a str),
    FrameReference(&'a str),
}

#[derive(Debug)]
pub enum SwitchCase<'a> {
    Default,
    Case(ExpressionNode<'a>),
}

#[derive(Debug)]
pub struct SwitchCaseGroup<'a> {
    pub cases: Vec<Node<'a, SwitchCase<'a>>>,
    pub body: Vec<StatementNode<'a>>,
}

#[derive(Debug)]
pub enum IfCondition<'a> {
    IfTrue(ExpressionNode<'a>),
    IfFalse(ExpressionNode<'a>),
}

#[derive(Debug)]
pub struct IfCase<'a> {
    pub condition: Node<'a, IfCondition<'a>>,
    pub body: Node<'a, Block<'a>>,
}

pub type StatementNode<'a> = Node<'a, Statement<'a>>;

#[derive(Debug)]
pub enum Statement<'a> {
    Comment(Comment<'a>),
    Block(Node<'a, Block<'a>>),
    Expression(ExpressionNode<'a>),
    Assignment {
        lvalue: ExpressionNode<'a>,
        rvalue: ExpressionNode<'a>,
    },
    Decl(Node<'a, Declaration<'a>>),
    If {
        case: Node<'a, IfCase<'a>>,
        else_if: Vec<Node<'a, IfCase<'a>>>,

        // This is only used for attaching comments
        // TODO: It would be more correct if these two were in a struct
        else_keyword: Option<Node<'a, ()>>,
        else_body: Option<Node<'a, Block<'a>>>,
    },
    Switch {
        control_expr: ExpressionNode<'a>,
        case_groups: Vec<Node<'a, SwitchCaseGroup<'a>>>,
    },
    Return(Option<ExpressionNode<'a>>),
    Break,
    Continue,
    Newline,
}

#[derive(Debug)]
pub struct Block<'a>(pub Vec<StatementNode<'a>>);

#[derive(Debug, Clone, Copy)]
pub enum BindingModifier {
    Const,
    Var,
    Nosave,
}

#[derive(Debug)]
pub enum BindingInitializer<'a> {
    Expr(ExpressionNode<'a>),
    Block(Node<'a, Block<'a>>),
    BuiltinReference(u32),
    StateFunction {
        frame: &'a str,
        callback: &'a str,
        body: Node<'a, Block<'a>>,
    },
}

#[derive(Debug)]
pub struct BoundName<'a> {
    pub name: &'a str,
    pub initializer: Option<BindingInitializer<'a>>,
}

#[derive(Debug)]
pub enum Declaration<'a> {
    Newline,
    Comment(Comment<'a>),
    Field {
        name: &'a str,
        ty: Node<'a, Type<'a>>,
    },
    Binding {
        modifiers: Vec<BindingModifier>,
        ty: Node<'a, Type<'a>>,
        names: Vec<Node<'a, BoundName<'a>>>,
    },
}

#[derive(Debug)]
pub struct ModelGenCommand<'a>(pub &'a str);

#[derive(Debug)]
pub enum ProgramPart<'a> {
    ModelGen(ModelGenCommand<'a>),
    Declaration(Node<'a, Declaration<'a>>),
}

pub type Program<'a> = Vec<ProgramPart<'a>>;

#[derive(Debug)]
pub enum Comment<'a> {
    Line(&'a str),
    Block { content: &'a str, is_inline: bool },
}

#[derive(Debug)]
pub struct NodeComments<'a> {
    pub before: Vec<Comment<'a>>,
    pub after: Vec<Comment<'a>>,
}

impl<'a> NodeComments<'a> {
    pub fn new() -> Self {
        NodeComments {
            before: Vec::new(),
            after: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct Node<'a, T>
where
    T: 'a,
{
    value: T,
    span: Option<Span<'a>>,
    comments: Option<Box<NodeComments<'a>>>,
}

impl<'a, T> Node<'a, T>
where
    T: 'a,
{
    pub fn new(value: T) -> Self {
        Node {
            value,
            span: None,
            comments: None,
        }
    }

    pub fn inner(&self) -> &T {
        &self.value
    }

    pub fn with_span(mut self, span: Span<'a>) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_comments_before(mut self, mut other: Vec<Comment<'a>>) -> Self {
        if other.len() == 0 {
            return self;
        }

        let mut comments = self
            .comments
            .unwrap_or_else(|| Box::new(NodeComments::new()));

        comments.before.append(&mut other);
        self.comments = Some(comments);
        self
    }

    pub fn with_comments_after(mut self, mut other: Vec<Comment<'a>>) -> Self {
        if other.len() == 0 {
            return self;
        }

        let mut comments = self
            .comments
            .unwrap_or_else(|| Box::new(NodeComments::new()));

        comments.after.append(&mut other);
        self.comments = Some(comments);
        self
    }

    pub fn comments_after(&self) -> Option<&[Comment<'a>]> {
        match &self.comments {
            Some(comments) => {
                if comments.after.len() == 0 {
                    None
                } else {
                    Some(comments.after.as_slice())
                }
            }
            None => None,
        }
    }

    pub fn comments_before(&self) -> Option<&[Comment<'a>]> {
        match &self.comments {
            Some(comments) => {
                if comments.before.len() == 0 {
                    None
                } else {
                    Some(comments.before.as_slice())
                }
            }
            None => None,
        }
    }
}

impl<'a, T> Deref for Node<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<'a, T: IntoIterator> IntoIterator for Node<'a, T> {
    type Item = T::Item;
    type IntoIter = T::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.value.into_iter()
    }
}

pub trait AstNode
where
    Self: Sized,
{
    fn into_node<'a>(self, span: Span<'a>) -> Node<'a, Self> {
        Node::new(self).with_span(span)
    }
}

impl AstNode for Expression<'_> {}
impl AstNode for Statement<'_> {}
impl AstNode for Declaration<'_> {}
impl AstNode for Block<'_> {}
impl AstNode for Type<'_> {}
impl AstNode for Argument<'_> {}
impl AstNode for BoundName<'_> {}
impl AstNode for IfCondition<'_> {}
impl AstNode for IfCase<'_> {}
impl AstNode for SwitchCase<'_> {}
impl AstNode for SwitchCaseGroup<'_> {}
