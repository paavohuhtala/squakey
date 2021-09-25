use std::ops::Deref;

use pest::Span;

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
}

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
    Newline,
}

#[derive(Debug)]
pub struct Block<'a>(pub Vec<Node<'a, Statement<'a>>>);

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
