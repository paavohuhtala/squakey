use std::fmt;

use crate::{
    ast::*,
    config::{BraceStyle, FormatSettings},
};

struct ProgramWriter {
    buffer: String,
    indent: usize,
    config: FormatSettings,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum BlockSpacing {
    None,
    SpaceBeforeOpen,
}

impl ProgramWriter {
    pub fn new(config: Option<FormatSettings>) -> ProgramWriter {
        ProgramWriter {
            buffer: String::new(),
            indent: 0,
            config: config.unwrap_or_default(),
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

    pub fn start_block(&mut self, spacing: BlockSpacing) {
        match self.config.brace {
            BraceStyle::EndOfLine => {
                match spacing {
                    BlockSpacing::None => self.write("{"),
                    BlockSpacing::SpaceBeforeOpen => self.write(" {"),
                }
                self.end_line();
                self.indent();
            }
            BraceStyle::NextLine => {
                self.end_line();
                self.start_line();
                self.write("{");
                self.end_line();
                self.indent();
            }
        }
    }

    pub fn end_block(&mut self) {
        self.dedent();
        self.start_line();
        self.write("}");
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

fn format_expression(writer: &mut ProgramWriter, expr: &Expression) {
    match expr {
        Expression::String(literal) => {
            writer.write("\"");
            writer.write(literal);
            writer.write("\"");
        }
        Expression::Number(_) => todo!(),
        Expression::Vector(_, _, _) => todo!(),
        Expression::Identifier(identifier) => {
            writer.write(identifier);
        }
    }
}

fn format_statement(writer: &mut ProgramWriter, statement: &Statement) {
    match statement {
        Statement::Block(block) => {
            writer.start_block(BlockSpacing::None);

            for statement in block {
                format_statement(writer, statement);
            }

            writer.end_block();
        }
        Statement::Expression(_) => todo!(),
        Statement::Assignment { lvalue, rvalue } => {
            writer.start_line();
            format_expression(writer, lvalue);
            writer.write(" = ");
            format_expression(writer, rvalue);
            writer.write(";");
            writer.end_line();
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
        Declaration::Function { name, ty, body } => {
            writer.start_line();
            format_type(writer, ty);
            writer.write(" ");
            writer.write(name);

            if let Some(body) = body {
                writer.write(" =");
                writer.start_block(BlockSpacing::SpaceBeforeOpen);

                for statement in body {
                    format_statement(writer, statement);
                }

                writer.end_block();
            }

            writer.write(";");
            writer.end_line();
        }
    }
}

pub fn format_program(declarations: Vec<Declaration>, config: Option<FormatSettings>) -> String {
    let mut writer = ProgramWriter::new(config);

    let mut consecutive_newlines = 0;

    for declaration in declarations.iter() {
        match declaration {
            Declaration::Newline if consecutive_newlines >= 2 => {
                continue;
            }
            Declaration::Newline => {
                consecutive_newlines += 1;
            }
            _ => {
                consecutive_newlines = 1;
            }
        }

        format_declaration(&mut writer, declaration);
    }

    writer.buffer
}
