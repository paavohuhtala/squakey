use squakey::{parser::parse_program, serializer::format_program};

fn main() {
    let input = include_str!("../tests/data/function_declaration/input.qc");
    let program = parse_program(&input);

    println!("{:?}", program);

    let formatted = format_program(program);

    println!("{}", formatted);
}
