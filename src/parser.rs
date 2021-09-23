use core::panic;

use pest::{
    iterators::{Pair, Pairs},
    prec_climber::{Assoc, Operator, PrecClimber},
    Parser,
};

use once_cell::{self, sync::Lazy};

use crate::ast::*;

#[derive(Parser)]
#[grammar = "quakec.pest"]
struct QuakeCParser;

trait PairExt<'a>
where
    Self: Sized + std::fmt::Debug,
{
    fn as_rule(&self) -> Rule;
    fn into_inner(self) -> Pairs<'a, Rule>;

    fn assert_and_unwrap(self, rule: Rule) -> Pairs<'a, Rule> {
        let as_rule = self.as_rule();
        if as_rule == rule {
            self.into_inner()
        } else {
            panic!("expected {:?}, found {:?}", rule, as_rule);
        }
    }

    fn assert_rule(&self, rule: Rule) {
        assert_eq!(self.as_rule(), rule);
    }

    fn assert_and_unwrap_only_child(self, rule: Rule) -> Pair<'a, Rule> {
        let mut unwrapped = self.assert_and_unwrap(rule);
        let first = unwrapped
            .next()
            .expect("Expected pair to contain at least one child.");

        match unwrapped.next() {
            Some(next) => {
                panic!("Expected exactly one child, found {:?}", next.as_rule());
            }
            None => first,
        }
    }
}

impl<'a> PairExt<'a> for Pair<'a, Rule> {
    fn as_rule(&self) -> Rule {
        self.as_rule()
    }

    fn into_inner(self) -> Pairs<'a, Rule> {
        Pair::into_inner(self)
    }
}

fn parse_builtin_type(pair: Pair<Rule>) -> BuiltinType {
    let inner = pair.assert_and_unwrap_only_child(Rule::builtin_type);

    match inner.as_rule() {
        Rule::type_entity => BuiltinType::Entity,
        Rule::type_float => BuiltinType::Float,
        Rule::type_string => BuiltinType::String,
        Rule::type_void => BuiltinType::Void,
        Rule::type_vector => BuiltinType::Vector,
        otherwise => panic!("unexpected rule: {:?}", otherwise),
    }
}

fn parse_identifier(pair: Pair<Rule>) -> &str {
    pair.assert_rule(Rule::identifier);
    pair.as_str()
}

fn parse_argument(pair: Pair<Rule>) -> Argument {
    let mut inner = pair.assert_and_unwrap(Rule::argument);
    let ty = inner.next().unwrap();
    let ty = parse_type(ty);

    let name = inner.next().unwrap();
    let name = parse_identifier(name);
    Argument { name, ty }
}

fn parse_type(pair: Pair<Rule>) -> Type {
    let mut inner = pair.assert_and_unwrap(Rule::any_type);
    let ty = inner.next().unwrap();
    let ty = match ty.as_rule() {
        Rule::function_type => {
            let mut inner = ty.into_inner();
            let return_type = inner.next().unwrap();
            let return_type = parse_builtin_type(return_type);

            let argument_list = inner.next().unwrap().into_inner();

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
            let inner = ty.assert_and_unwrap_only_child(Rule::field_reference_type);
            let inner = parse_type(inner);
            Type::FieldReference(Box::new(inner))
        }
        Rule::builtin_type => Type::Builtin(parse_builtin_type(ty)),
        otherwise => panic!("unexpected rule {:?}", otherwise),
    };

    match inner.peek() {
        Some(rule) if rule.as_rule() == Rule::pointer_asterisk => {
            inner.next();
            return Type::Pointer(Box::new(ty));
        }
        Some(otherwise) => panic!(
            "expected pointer asterisk or end of type, found {:?}",
            otherwise.as_rule()
        ),
        None => ty,
    }
}

fn parse_call_arguments<'a>(pair: Pair<'a, Rule>) -> Vec<Expression<'a>> {
    let args = pair.assert_and_unwrap(Rule::call_arguments);

    let mut arguments = Vec::new();

    for arg in args {
        arguments.push(parse_expression(arg));
    }

    arguments
}

fn parse_primary_expr<'a>(pair: Pair<'a, Rule>) -> Expression<'a> {
    match pair.as_rule() {
        Rule::string_literal => {
            let literal = pair
                .assert_and_unwrap_only_child(Rule::string_literal)
                .as_str();
            Expression::String(literal)
        }
        Rule::identifier => {
            let name = pair.as_str();
            Expression::Identifier(name)
        }
        Rule::number_literal => {
            let number = pair.as_str().parse().unwrap();
            Expression::Number(number)
        }
        Rule::expression => parse_expression(pair),
        Rule::prefixed => {
            let mut children = pair.assert_and_unwrap(Rule::prefixed);
            let op = children.next().unwrap();

            let prefix_op = match op.as_rule() {
                Rule::neg => PrefixOp::Neg,
                Rule::not => PrefixOp::Not,
                otherwise => panic!("expected prefix op, found {:?}", otherwise),
            };

            let inner = children.next().unwrap();
            let inner = parse_unary_expression(inner);

            Expression::Prefix(prefix_op, Box::new(inner))
        }
        Rule::identifier_call => {
            let mut children = pair.assert_and_unwrap(Rule::identifier_call);
            let target = children.next().unwrap();
            let identifier = parse_identifier(target);

            let args = match children.next() {
                None => Vec::new(),
                Some(args) => parse_call_arguments(args),
            };

            Expression::Call(Box::new(Expression::Identifier(identifier)), args)
        }
        Rule::vector_literal => {
            let mut children = pair.assert_and_unwrap(Rule::vector_literal);
            let x = children.next().unwrap().as_str().parse().unwrap();
            let y = children.next().unwrap().as_str().parse().unwrap();
            let z = children.next().unwrap().as_str().parse().unwrap();

            Expression::Vector(x, y, z)
        }
        otherwise => {
            panic!("expected any primary expression, found {:?}", otherwise);
        }
    }
}

fn parse_unary_expression<'a>(pair: Pair<'a, Rule>) -> Expression<'a> {
    let mut inner = pair.assert_and_unwrap(Rule::unary_expression);

    let primary = inner.next().unwrap();
    let primary = parse_primary_expr(primary);

    let mut expr = primary;

    while let Some(op) = inner.next() {
        let mut pairs = op.assert_and_unwrap(Rule::selector);

        let identifier = pairs.next().unwrap();
        let identifier = parse_identifier(identifier);

        expr = Expression::FieldAccess(Box::new(expr), identifier);

        match pairs.next() {
            Some(rule) => {
                let args = parse_call_arguments(rule);
                expr = Expression::Call(Box::new(expr), args);
            }
            None => {}
        }
    }

    expr
}

fn parse_expression<'a>(pair: Pair<'a, Rule>) -> Expression<'a> {
    let pairs = pair.assert_and_unwrap(Rule::expression);

    let infix = |lhs: Expression<'a>, op: Pair<Rule>, rhs: Expression<'a>| {
        let op = match op.as_rule() {
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

        Expression::Infix(op, Box::new((lhs, rhs)))
    };

    static CLIMBER: Lazy<PrecClimber<Rule>> = Lazy::new(|| {
        PrecClimber::new(vec![
            Operator::new(Rule::add, Assoc::Left) | Operator::new(Rule::sub, Assoc::Left),
            Operator::new(Rule::mul, Assoc::Left) | Operator::new(Rule::div, Assoc::Left),
            Operator::new(Rule::and, Assoc::Left),
            Operator::new(Rule::or, Assoc::Left),
        ])
    });

    CLIMBER.climb(pairs, parse_unary_expression, infix)
}

fn parse_statement(pair: Pair<Rule>) -> Vec<Statement> {
    let statement = pair.assert_and_unwrap_only_child(Rule::statement);
    match statement.as_rule() {
        Rule::assignment => {
            let mut inner = statement.into_inner();
            let left = inner.next().unwrap();
            let left = parse_expression(left);
            let right = inner.next().unwrap();
            let right = parse_expression(right);
            vec![Statement::Assignment {
                lvalue: left,
                rvalue: right,
            }]
        }
        Rule::declaration => parse_declaration(statement)
            .into_iter()
            .map(Statement::Decl)
            .collect(),
        Rule::expression_statement => {
            let inner = statement.into_inner().next().unwrap();
            vec![Statement::Expression(parse_expression(inner))]
        }
        Rule::newline => vec![Statement::Newline],
        otherwise => panic!("unimplemented rule: {:?}", otherwise),
    }
}

fn parse_block(pair: Pair<Rule>) -> Block {
    let inner = pair.assert_and_unwrap(Rule::block);
    let mut statements = Vec::new();

    for statement_or_newline in inner {
        match statement_or_newline.as_rule() {
            Rule::newline => {
                statements.push(Statement::Newline);
            }
            Rule::statement => {
                let mut new_statements = parse_statement(statement_or_newline);
                statements.append(&mut new_statements);
            }
            otherwise => panic!("Expected statement or newline, found {:?}", otherwise),
        };
    }

    Block(statements)
}

fn parse_binding(pair: Pair<Rule>) -> Vec<Declaration> {
    fn parse_initializer(pair: Pair<Rule>) -> BindingInitializer {
        let child = pair.assert_and_unwrap_only_child(Rule::initializer);
        match child.as_rule() {
            Rule::builtin_reference => {
                let reference = child.assert_and_unwrap_only_child(Rule::builtin_reference);
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

    fn parse_modifiers(pair: Pair<Rule>) -> Vec<BindingModifier> {
        pair.assert_and_unwrap(Rule::binding_modifiers)
            .map(|pair| match pair.as_rule() {
                Rule::binding_const => BindingModifier::Const,
                Rule::binding_var => BindingModifier::Var,
                Rule::binding_nosave => BindingModifier::Nosave,
                otherwise => panic!("Expected any binding modifier, found {:?}", otherwise),
            })
            .collect()
    }

    let mut inner = pair.assert_and_unwrap(Rule::binding);

    let modifiers = inner.next().unwrap();
    let modifiers = parse_modifiers(modifiers);

    let ty = inner.next().unwrap();
    let ty = parse_type(ty);

    let mut declarations = Vec::new();

    loop {
        let next = inner.next().expect("Unexpected end of binding list");
        match next.as_rule() {
            Rule::identifier => {
                let name = parse_identifier(next);

                let next = inner.peek().expect("Unexpected end of binding list");

                let initializer = match next.as_rule() {
                    Rule::initializer => Some(parse_initializer(inner.next().unwrap())),
                    _ => None,
                };

                declarations.push(Declaration::Binding {
                    name,
                    ty: ty.clone(),
                    initializer,
                    modifiers: modifiers.clone(),
                });
            }
            Rule::end_of_declaration => return declarations,
            otherwise => panic!("unexpected rule: {:?}", otherwise),
        }
    }
}

fn parse_declaration(pair: Pair<Rule>) -> Vec<Declaration> {
    let inner = pair.assert_and_unwrap(Rule::declaration).next().unwrap();
    match inner.as_rule() {
        // TODO
        Rule::line_comment | Rule::newline => vec![Declaration::Newline],
        Rule::field_declaration => {
            let mut inner = inner.into_inner();

            let ty = inner.next().unwrap();
            let ty = parse_type(ty);

            let name = inner.next().unwrap();
            let name = parse_identifier(name);

            vec![Declaration::Field { name, ty }]
        }
        Rule::binding => parse_binding(inner),

        otherwise => panic!("expected declaration, got {:?}", otherwise),
    }
}

pub fn parse_program(input: &str) -> Vec<Declaration> {
    match QuakeCParser::parse(Rule::main, input) {
        Ok(mut result) => {
            let program = result.next().unwrap().assert_and_unwrap(Rule::main);
            let mut declarations = Vec::new();

            for declaration_or_newline in program {
                let rule = declaration_or_newline.as_rule();

                if rule == Rule::EOI {
                    break;
                }

                if rule == Rule::newline {
                    declarations.push(Declaration::Newline);
                    continue;
                }

                let mut new_declarations = parse_declaration(declaration_or_newline);
                declarations.append(&mut new_declarations);
            }

            declarations
        }
        Err(error) => {
            println!("{}", error);
            panic!("parse error");
        }
    }
}
