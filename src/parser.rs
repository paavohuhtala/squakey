use pest::{iterators::Pair, Parser};

use crate::ast::*;

#[derive(Parser)]
#[grammar = "quakec.pest"]
struct QuakeCParser;

fn parse_builtin_type(pair: Pair<Rule>) -> BuiltinType {
    if pair.as_rule() != Rule::builtin_type {
        panic!("expect builtin type, found {:?}", pair.as_rule());
    }

    let inner = pair.into_inner().next().unwrap();

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
    match pair.as_rule() {
        Rule::identifier => pair.as_str(),
        rule => panic!("expected identifier, found {:?}", rule),
    }
}

fn parse_argument(pair: Pair<Rule>) -> Argument {
    if pair.as_rule() != Rule::argument {
        panic!("expected argument, got {:?}", pair.as_rule())
    }

    let mut inner = pair.into_inner();
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

fn parse_declaration(pair: Pair<Rule>) -> Declaration {
    match pair.as_rule() {
        Rule::newline => Declaration::Newline,
        Rule::field_declaration => {
            let mut inner = pair.into_inner();

            let ty = inner.next().unwrap();
            let ty = parse_type(ty);

            let name = inner.next().unwrap();
            let name = parse_identifier(name);

            Declaration::Field { name, ty }
        }
        otherwise => panic!("unexpected rule {:?}", otherwise),
    }
}

pub fn parse_program(input: &str) -> Vec<Declaration> {
    match QuakeCParser::parse(Rule::main, input) {
        Ok(mut result) => {
            let only = result.next().unwrap();

            let mut declarations = Vec::new();

            match only.as_rule() {
                Rule::main => {
                    let inner = only.into_inner().next().unwrap().into_inner();
                    for declaration in inner {
                        let declaration = parse_declaration(declaration);
                        declarations.push(declaration);
                    }
                }
                otherwise => panic!("unexpected rule {:?}", otherwise),
            }

            declarations
        }
        Err(error) => {
            println!("{}", error);
            panic!("parse error");
        }
    }
}
