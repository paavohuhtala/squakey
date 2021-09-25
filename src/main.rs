use squakey::{parser::parse_program, serializer::format_program};

fn main() {
    let input = include_str!("../test_files/knight.qc");
    let program = parse_program(&input);

    let formatted = format_program(program, None);

    println!("{}", formatted);
}
