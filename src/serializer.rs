use std::fmt::{self, Write};

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

fn format_infix<'a>(
    writer: &mut ProgramWriter,
    op: InfixOp,
    lhs: &Expression<'a>,
    rhs: &Expression<'a>,
) {
    // https://stackoverflow.com/a/14184425
    let use_parenthesis_l = match lhs {
        Expression::Infix(left_op, _) => {
            if *left_op == op && op.is_associative() {
                false
            } else if left_op.precedence() > op.precedence() {
                false
            } else if left_op.is_left_associative()
                && op.is_left_associative()
                && left_op.precedence() == op.precedence()
            {
                false
            } else {
                true
            }
        }
        _ => false,
    };

    if use_parenthesis_l {
        writer.write("(");
    }

    format_expression(writer, lhs);

    if use_parenthesis_l {
        writer.write(")");
    }

    let op_str = match op {
        InfixOp::Add => " + ",
        InfixOp::Sub => " - ",
        InfixOp::Mul => " * ",
        InfixOp::Div => " / ",
        InfixOp::And => " && ",
        InfixOp::Or => " || ",
        InfixOp::BitwiseAnd => " & ",
        InfixOp::BitwiseOr => " | ",
        InfixOp::BitwiseXor => " ^ ",
        InfixOp::Equals => " == ",
        InfixOp::NotEquals => " != ",
    };

    writer.write(op_str);

    let use_parenthesis_r = match rhs {
        Expression::Infix(right_op, _) => {
            if *right_op == op && op.is_associative() {
                false
            } else if right_op.precedence() > op.precedence() {
                false
            } else if right_op.is_right_associative()
                && right_op.is_right_associative()
                && right_op.precedence() == op.precedence()
            {
                false
            } else {
                true
            }
        }
        _ => false,
    };

    if use_parenthesis_r {
        writer.write("(");
    }

    format_expression(writer, rhs);

    if use_parenthesis_r {
        writer.write(")");
    }
}

fn format_expression(writer: &mut ProgramWriter, expr: &Expression) {
    match expr {
        Expression::String(literal) => {
            writer.write("\"");
            writer.write(literal);
            writer.write("\"");
        }
        Expression::Number(number) => {
            write!(writer, "{}", number).unwrap();
        }
        Expression::Vector(_, _, _) => todo!(),
        Expression::Identifier(identifier) => {
            writer.write(identifier);
        }
        Expression::Prefix(op, inner) => {
            // Simplify chained prefix ops
            match (op, inner.as_ref()) {
                (PrefixOp::Neg, Expression::Prefix(PrefixOp::Neg, inner))
                | (PrefixOp::Not, Expression::Prefix(PrefixOp::Not, inner)) => {
                    return format_expression(writer, inner);
                }
                (PrefixOp::Neg, Expression::Number(number)) if *number < 0.0 => {
                    return format_expression(writer, &Expression::Number(-number))
                }
                _ => {}
            }

            let op_str = match op {
                PrefixOp::Not => "!",
                PrefixOp::Neg => "-",
            };

            writer.write(op_str);

            match inner.as_ref() {
                Expression::Infix(_, _) => {
                    writer.write("(");
                    format_expression(writer, inner);
                    writer.write(")");
                }
                _ => {
                    format_expression(writer, inner);
                }
            }
        }
        Expression::Infix(op, boxed) => {
            let left = &boxed.0;
            let right = &boxed.1;

            format_infix(writer, *op, left, right);
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
        Declaration::Binding {
            name,
            ty,
            initializer,
        } => {
            writer.start_line();
            format_type(writer, ty);
            writer.write(" ");
            writer.write(name);

            match initializer {
                None => {}
                Some(BindingInitializer::Block(block)) => {
                    writer.write(" =");
                    writer.start_block(BlockSpacing::SpaceBeforeOpen);

                    for statement in block {
                        format_statement(writer, statement);
                    }

                    writer.end_block();
                }
                Some(BindingInitializer::Expr(expr)) => {
                    writer.write(" = ");
                    format_expression(writer, expr)
                }
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
