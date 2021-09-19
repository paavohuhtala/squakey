use std::fmt::{self};

use crate::ast::*;

struct ProgramWriter {
    buffer: String,
    indent: usize,
}

impl ProgramWriter {
    pub fn new() -> ProgramWriter {
        ProgramWriter {
            buffer: String::new(),
            indent: 0,
        }
    }

    #[allow(dead_code)]
    pub fn indent(&mut self) {
        self.indent += 1;
    }

    #[allow(dead_code)]
    pub fn dedent(&mut self) {
        self.indent -= 1;
    }

    fn apply_indent(&mut self) {
        for _ in 0..self.indent {
            self.buffer.push_str("  ");
        }
    }

    pub fn start_line(&mut self) {
        self.apply_indent();
    }

    pub fn end_line(&mut self) {
        self.buffer.push_str("\n");
    }

    pub fn write(&mut self, value: &str) {
        self.buffer.push_str(value);
    }
}

impl fmt::Write for ProgramWriter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.buffer.push_str(s);
        Ok(())
    }
}

fn builtin_type_as_str(builtin_type: BuiltinType) -> &'static str {
    match builtin_type {
        BuiltinType::String => "string",
        BuiltinType::Float => "float",
        BuiltinType::Vector => "vector",
        BuiltinType::Entity => "entity",
        BuiltinType::Void => "void",
    }
}

fn format_argument_list(writer: &mut ProgramWriter, argument_list: &[Argument]) {
    writer.write("(");

    for (i, argument) in argument_list.iter().enumerate() {
        if i > 0 {
            writer.write(", ");
        }

        format_type(writer, &argument.ty);
        writer.write(" ");
        writer.write(argument.name);
    }

    writer.write(")");
}

fn format_type(writer: &mut ProgramWriter, ty: &Type) {
    match ty {
        Type::Builtin(builtin_type) => {
            writer.write(builtin_type_as_str(*builtin_type));
        }
        Type::Function {
            return_type,
            arguments,
        } => {
            format_type(writer, &Type::Builtin(*return_type));
            format_argument_list(writer, arguments)
        }
    }
}

fn format_declaration(writer: &mut ProgramWriter, decl: &Declaration) {
    match decl {
        Declaration::Newline => {
            writer.end_line();
        }
        Declaration::Field { name, ty } => {
            writer.start_line();
            writer.write(".");
            format_type(writer, ty);
            writer.write(" ");
            writer.write(name);
            writer.write(";");
            writer.end_line();
        }
    }
}

pub fn format_program(declarations: Vec<Declaration>) -> String {
    let mut writer = ProgramWriter::new();

    let mut consecutive_newlines = 0;

    for declaration in declarations.iter() {
        println!("{:?}", declaration);
        match declaration {
            Declaration::Newline if consecutive_newlines >= 2 => {
                println!("{} newlines, skipping", consecutive_newlines);
                continue;
            }
            Declaration::Newline => {
                consecutive_newlines += 1;
            }
            Declaration::Field { .. } => {
                consecutive_newlines = 1;
            }
        }

        format_declaration(&mut writer, declaration);
    }

    writer.buffer
}
