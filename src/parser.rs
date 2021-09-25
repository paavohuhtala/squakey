use core::panic;

use pest::{
    iterators::Pair,
    prec_climber::{Assoc, Operator, PrecClimber},
    Parser,
};

use once_cell::{self, sync::Lazy};

use crate::{ast::*, parse_util::QCPairs};
use crate::{grammar::*, parse_util::QCPair};

fn parse_builtin_type(pair: QCPair) -> BuiltinType {
    let mut inner = pair.assert_and_unwrap_children(Rule::builtin_type);
    let inner = inner.only_child();

    match inner.as_rule() {
        Rule::type_entity => BuiltinType::Entity,
        Rule::type_float => BuiltinType::Float,
        Rule::type_string => BuiltinType::String,
        Rule::type_void => BuiltinType::Void,
        Rule::type_vector => BuiltinType::Vector,
        otherwise => panic!("unexpected rule: {:?}", otherwise),
    }
}

fn parse_identifier(pair: QCPair) -> &str {
    pair.assert_rule(Rule::identifier);
    pair.as_str()
}

fn parse_argument(pair: QCPair) -> Node<Argument> {
    let span = pair.as_span();
    let mut inner = pair.assert_and_unwrap_children(Rule::argument);
    let ty = inner.next().unwrap();
    let ty = parse_type(ty);

    let name = inner.next().unwrap();
    let name = parse_identifier(name);
    (Argument { name, ty }).into_node(span)
}

fn parse_type(pair: QCPair) -> Node<Type> {
    let span = pair.as_span();
    let mut inner = pair.assert_and_unwrap_children(Rule::any_type);
    let ty_pair = inner.next().unwrap();

    let ty_span = ty_pair.as_span();
    let ty = match ty_pair.as_rule() {
        Rule::function_type => {
            let mut inner = ty_pair.children();
            let return_type = inner.next().unwrap();
            let return_type = parse_builtin_type(return_type);

            let argument_list = inner.next().unwrap().children();

            let mut arguments = Vec::new();

            for parameter in argument_list {
                arguments.push(parse_argument(parameter));
            }

            Type::Function {
                return_type,
                arguments,
            }
        }
        Rule::field_reference_type => {
            let inner = ty_pair
                .assert_and_unwrap_children(Rule::field_reference_type)
                .only_child();
            let inner = parse_type(inner);
            Type::FieldReference(Box::new(inner))
        }
        Rule::builtin_type => Type::Builtin(parse_builtin_type(ty_pair)),
        otherwise => panic!("unexpected rule {:?}", otherwise),
    }
    .into_node(ty_span);

    match inner.peek() {
        Some(rule) if rule.as_rule() == Rule::pointer_asterisk => {
            inner.next();
            return Type::Pointer(Box::new(ty)).into_node(span);
        }
        Some(otherwise) => panic!(
            "expected pointer asterisk or end of type, found {:?}",
            otherwise.as_rule()
        ),
        None => ty,
    }
}

fn parse_call_arguments(pair: QCPair) -> Vec<Node<Expression>> {
    let args = pair.assert_and_unwrap_children(Rule::call_arguments);

    let mut arguments = Vec::new();

    for arg in args {
        arguments.push(parse_expression(arg));
    }

    arguments
}

fn parse_primary_expr(pair: QCPair) -> Node<Expression> {
    let span = pair.as_span();
    match pair.as_rule() {
        Rule::string_literal => {
            let literal = pair
                .assert_and_unwrap_children(Rule::string_literal)
                .only_child()
                .as_str();
            Expression::String(literal).into_node(span)
        }
        Rule::identifier => {
            let name = pair.as_str();
            Expression::Identifier(name).into_node(span)
        }
        Rule::number_literal => {
            let number = pair.as_str().parse().unwrap();
            Expression::Number(number).into_node(span)
        }
        Rule::expression => parse_expression(pair),
        Rule::prefixed => {
            let mut children = pair.assert_and_unwrap_children(Rule::prefixed);
            let op = children.next().unwrap();

            let prefix_op = match op.as_rule() {
                Rule::neg => PrefixOp::Neg,
                Rule::not => PrefixOp::Not,
                otherwise => panic!("expected prefix op, found {:?}", otherwise),
            };

            let inner = children.next().unwrap();
            let inner = parse_unary_expression(inner);

            Expression::Prefix(prefix_op, Box::new(inner)).into_node(span)
        }
        Rule::identifier_call => {
            let mut children = pair.assert_and_unwrap_children(Rule::identifier_call);
            let target = children.next().unwrap();
            let target_span = target.as_span();
            let identifier = parse_identifier(target);

            let args = match children.next() {
                None => Vec::new(),
                Some(args) => parse_call_arguments(args),
            };

            let identifier_node = Expression::Identifier(identifier).into_node(target_span);
            Expression::Call(Box::new(identifier_node), args).into_node(span)
        }
        Rule::vector_literal => {
            let mut children = pair.assert_and_unwrap_children(Rule::vector_literal);
            let x = children.next().unwrap().as_str().parse().unwrap();
            let y = children.next().unwrap().as_str().parse().unwrap();
            let z = children.next().unwrap().as_str().parse().unwrap();

            Expression::Vector(x, y, z).into_node(span)
        }
        otherwise => {
            panic!("expected any primary expression, found {:?}", otherwise);
        }
    }
}

fn parse_unary_expression(pair: QCPair) -> Node<Expression> {
    let mut inner = pair.assert_and_unwrap_children(Rule::unary_expression);

    let primary = inner.next().unwrap();
    let primary = parse_primary_expr(primary);

    let mut expr = primary;

    while let Some(op) = inner.next() {
        let span = op.as_span();
        let mut pairs = op.assert_and_unwrap_children(Rule::selector);

        let identifier = pairs.next().unwrap();
        let identifier = parse_identifier(identifier);

        expr = Expression::FieldAccess(Box::new(expr), identifier).into_node(span);

        match pairs.next() {
            Some(rule) => {
                let span = rule.as_span();
                let args = parse_call_arguments(rule);
                expr = Expression::Call(Box::new(expr), args).into_node(span);
            }
            None => {}
        }
    }

    expr
}

fn parse_expression<'a>(pair: QCPair<'a>) -> ExpressionNode<'a> {
    let span = pair.as_span();
    let mut pairs = pair.assert_and_unwrap_children(Rule::expression);

    let infix = |lhs: ExpressionNode<'a>, op_pair: Pair<'a, Rule>, rhs: ExpressionNode<'a>| {
        let op = match op_pair.as_rule() {
            Rule::add => InfixOp::Add,
            Rule::sub => InfixOp::Sub,
            Rule::mul => InfixOp::Mul,
            Rule::div => InfixOp::Div,
            Rule::and => InfixOp::And,
            Rule::or => InfixOp::Or,
            Rule::bitwise_and => InfixOp::BitwiseAnd,
            Rule::bitwise_or => InfixOp::BitwiseOr,
            Rule::bitwise_xor => InfixOp::BitwiseXor,
            Rule::equals => InfixOp::Equals,
            Rule::not_equals => InfixOp::NotEquals,
            _ => unreachable!(),
        };

        Expression::Infix(op, Box::new((lhs, rhs))).into_node(span.clone())
    };

    static CLIMBER: Lazy<PrecClimber<Rule>> = Lazy::new(|| {
        PrecClimber::new(vec![
            Operator::new(Rule::add, Assoc::Left) | Operator::new(Rule::sub, Assoc::Left),
            Operator::new(Rule::mul, Assoc::Left) | Operator::new(Rule::div, Assoc::Left),
            Operator::new(Rule::and, Assoc::Left),
            Operator::new(Rule::or, Assoc::Left),
        ])
    });

    let pairs_iterator = pairs.pest_iterator();

    let expression = CLIMBER.climb(
        pairs_iterator,
        |pair| parse_unary_expression(QCPair::new(pair)),
        infix,
    );

    expression
}

fn parse_statement(pair: QCPair) -> Node<Statement> {
    let span = pair.as_span();
    let mut inner = pair.assert_and_unwrap_children(Rule::statement);

    let statement_pair = inner.next().unwrap();
    let statement = match statement_pair.as_rule() {
        Rule::line_comment => {
            let content = statement_pair
                .assert_and_unwrap_children(Rule::line_comment)
                .only_child()
                .as_str();
            Statement::Comment(Comment::Line(content))
        }
        Rule::assignment => {
            let mut inner = statement_pair.children();
            let left = inner.next().unwrap();
            let left = parse_expression(left);
            let right = inner.next().unwrap();
            let right = parse_expression(right);
            Statement::Assignment {
                lvalue: left,
                rvalue: right,
            }
        }
        Rule::declaration => Statement::Decl(parse_declaration(statement_pair)),
        Rule::expression_statement => {
            let mut inner = statement_pair.children();
            let expr_pair = inner.next().unwrap();
            let expr = Statement::Expression(parse_expression(expr_pair));
            inner.next().unwrap().assert_rule(Rule::end_of_statement);
            expr
        }
        Rule::newline => Statement::Newline,
        otherwise => panic!("unimplemented rule: {:?}", otherwise),
    };

    let node = Node::new(statement).with_span(span);

    inner.consume_to_end();

    node.with_comments_after(inner.comments())
}

fn parse_block(pair: QCPair) -> Node<Block> {
    let span = pair.as_span();
    let inner = pair.assert_and_unwrap_children(Rule::block);
    let mut statements = Vec::new();

    for statement_or_newline in inner {
        match statement_or_newline.as_rule() {
            Rule::newline => {
                statements.push(Statement::Newline.into_node(span.clone()));
            }
            Rule::statement => {
                statements.push(parse_statement(statement_or_newline));
            }
            otherwise => panic!("Expected statement or newline, found {:?}", otherwise),
        };
    }

    Block(statements).into_node(span)
}

fn parse_binding(pair: QCPair) -> Declaration {
    fn parse_initializer(pair: QCPair) -> BindingInitializer {
        let child = pair
            .assert_and_unwrap_children(Rule::initializer)
            .only_child();
        match child.as_rule() {
            Rule::builtin_reference => {
                let reference = child
                    .assert_and_unwrap_children(Rule::builtin_reference)
                    .only_child();
                let reference = reference
                    .as_str()
                    .parse::<u32>()
                    .expect("Expected builtin reference to be a valid 32-bit integer.");
                BindingInitializer::BuiltinReference(reference)
            }
            Rule::block => {
                let block = parse_block(child);
                BindingInitializer::Block(block)
            }
            Rule::expression => {
                let value = parse_expression(child);
                BindingInitializer::Expr(value)
            }
            otherwise => panic!("unexpected rule {:?}", otherwise),
        }
    }

    fn parse_modifiers(pair: QCPair) -> Vec<BindingModifier> {
        pair.assert_and_unwrap_children(Rule::binding_modifiers)
            .map(|pair| match pair.as_rule() {
                Rule::binding_const => BindingModifier::Const,
                Rule::binding_var => BindingModifier::Var,
                Rule::binding_nosave => BindingModifier::Nosave,
                otherwise => panic!("Expected any binding modifier, found {:?}", otherwise),
            })
            .collect()
    }

    let mut inner = pair.assert_and_unwrap_children(Rule::binding);

    let modifiers = inner.next().unwrap();
    let modifiers = parse_modifiers(modifiers);

    let ty = inner.next().unwrap();
    let ty = parse_type(ty);

    let mut names = Vec::new();

    loop {
        let next = inner.next().expect("Unexpected end of binding list");
        let next_span = next.as_span();
        match next.as_rule() {
            Rule::identifier => {
                let name = parse_identifier(next);

                let next = inner.peek().expect("Unexpected end of binding list");

                let initializer = match next.as_rule() {
                    Rule::initializer => {
                        inner.next();
                        Some(parse_initializer(next))
                    }
                    Rule::identifier | Rule::end_of_declaration => None,
                    otherwise => panic!(
                        "Expected initializer, identifier or end_of_declaration, found {:?}",
                        otherwise
                    ),
                };

                names.push(BoundName { name, initializer }.into_node(next_span));
            }
            Rule::end_of_declaration => {
                return Declaration::Binding {
                    ty,
                    modifiers,
                    names,
                }
            }
            otherwise => panic!("unexpected rule: {:?}", otherwise),
        }
    }
}

fn parse_declaration(pair: QCPair) -> Node<Declaration> {
    let span = pair.as_span();
    let mut inner = pair.assert_and_unwrap_children(Rule::declaration);
    let declaration_pair = inner.next().unwrap();

    let declaration = match declaration_pair.as_rule() {
        Rule::line_comment => {
            let content = declaration_pair
                .assert_and_unwrap_children(Rule::line_comment)
                .only_child()
                .as_str();
            Declaration::Comment(Comment::Line(content))
        }
        Rule::newline => Declaration::Newline,
        Rule::field_declaration => {
            let mut inner = declaration_pair.children();

            let ty = inner.next().unwrap();
            let ty = parse_type(ty);

            let name = inner.next().unwrap();
            let name = parse_identifier(name);

            Declaration::Field { name, ty }
        }
        Rule::binding => parse_binding(declaration_pair),

        otherwise => panic!("expected declaration, got {:?}", otherwise),
    };

    let node = Node::new(declaration).with_span(span);

    inner.consume_to_end();

    node.with_comments_after(inner.comments())
}

pub fn parse_program(input: &str) -> Vec<Node<Declaration>> {
    match QuakeCParser::parse(Rule::main, input) {
        Ok(result) => {
            let mut result = QCPairs::new(result);
            let program = result
                .next()
                .unwrap()
                .assert_and_unwrap_children(Rule::main);
            let mut declarations = Vec::new();

            for declaration_or_newline in program {
                let span = declaration_or_newline.as_span();
                let rule = declaration_or_newline.as_rule();

                if rule == Rule::EOI {
                    break;
                }

                if rule == Rule::newline {
                    declarations.push(Declaration::Newline.into_node(span));
                    continue;
                }

                declarations.push(parse_declaration(declaration_or_newline));
            }

            declarations
        }
        Err(error) => {
            println!("{}", error);
            panic!("parse error");
        }
    }
}
