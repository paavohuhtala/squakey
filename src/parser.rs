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
        println!("Unwrapping {:?} with {:?}", self, rule);
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
    match pair.as_rule() {
        Rule::function_type => {
            let mut inner = pair.into_inner();
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
        Rule::builtin_type => Type::Builtin(parse_builtin_type(pair)),
        otherwise => panic!("unexpected rule {:?}", otherwise),
    }
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
            let inner = parse_primary_expr(inner);

            Expression::Prefix(prefix_op, Box::new(inner))
        }
        otherwise => {
            panic!("expected any primary expression, found {:?}", otherwise);
        }
    }
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

    CLIMBER.climb(pairs, parse_primary_expr, infix)
}

fn parse_statement(pair: Pair<Rule>) -> Statement {
    let statement = pair.assert_and_unwrap_only_child(Rule::statement);
    match statement.as_rule() {
        Rule::assignment => {
            let mut inner = statement.into_inner();
            let left = inner.next().unwrap();
            let left = parse_expression(left);
            let right = inner.next().unwrap();
            let right = parse_expression(right);
            Statement::Assignment {
                lvalue: left,
                rvalue: right,
            }
        }
        otherwise => panic!("unimplemented rule: {:?}", otherwise),
    }
}

fn parse_declaration(pair: Pair<Rule>) -> Declaration {
    let inner = pair.assert_and_unwrap(Rule::declaration).next().unwrap();
    match inner.as_rule() {
        Rule::newline => Declaration::Newline,
        Rule::field_declaration => {
            let mut inner = inner.into_inner();

            let ty = inner.next().unwrap();
            let ty = parse_type(ty);

            let name = inner.next().unwrap();
            let name = parse_identifier(name);

            Declaration::Field { name, ty }
        }
        Rule::binding => {
            let mut inner = inner.into_inner();

            let ty = inner.next().unwrap();
            let ty = parse_type(ty);

            let name = inner.next().unwrap();
            let name = parse_identifier(name);

            let next = inner.next();

            let initializer = match next {
                Some(initializer) if initializer.as_rule() == Rule::initializer => {
                    let child = initializer.assert_and_unwrap_only_child(Rule::initializer);
                    match child.as_rule() {
                        Rule::block => {
                            let inner = child.into_inner();
                            let mut body = Vec::new();

                            for statement in inner {
                                let statement = parse_statement(statement);
                                body.push(statement);
                            }

                            Some(BindingInitializer::Block(body))
                        }
                        Rule::expression => {
                            let value = parse_expression(child);
                            Some(BindingInitializer::Expr(value))
                        }
                        otherwise => panic!("unexpected rule {:?}", otherwise),
                    }
                }
                Some(pair) if pair.as_rule() == Rule::end_of_declaration => None,
                otherwise => panic!("unexpected rule {:?}", otherwise),
            };

            Declaration::Binding {
                name,
                ty,
                initializer,
            }
        }
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

                let declaration = parse_declaration(declaration_or_newline);
                declarations.push(declaration);
            }

            declarations
        }
        Err(error) => {
            println!("{}", error);
            panic!("parse error");
        }
    }
}
