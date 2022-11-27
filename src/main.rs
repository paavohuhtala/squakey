use std::{
    env,
    fs::File,
    io::{Read, Write},
};

use squakey::{parser::parse_program, serializer::format_program};

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let mut ifile = File::open(&args[1])?;
    let mut input = String::new();
    ifile.read_to_string(&mut input)?;

    println!("Formatting {}", &args[1]);

    let program = parse_program(&input);

    let formatted = format_program(program, None);

    let mut file = File::create(&args[1])?;
    file.write_all(&formatted.into_bytes())?;

    Ok(())
}
