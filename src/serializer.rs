use std::{
    fmt::{self, Write},
    rc::Rc,
};

use crate::{
    ast::*,
    config::{FormatSettings, IndentStyle},
    format_utils::StatementStyle,
};

struct ProgramWriter {
    buffer: String,
    indent: usize,
    pub config: Rc<FormatSettings>,
    consecutive_empty_lines: usize,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum BlockSpacing {
    None,
    SpaceBeforeOpen,
}

impl ProgramWriter {
    pub fn new(config: Option<Rc<FormatSettings>>) -> ProgramWriter {
        ProgramWriter {
            buffer: String::new(),
            indent: 0,
            config: config.unwrap_or_default(),
            consecutive_empty_lines: 0,
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
        self.consecutive_empty_lines = 0;
    }

    pub fn write(&mut self, value: &str) {
        self.buffer.push_str(value);
    }

    pub fn start_block(&mut self, spacing: BlockSpacing) {
        match self.config.indent_style {
            IndentStyle::EndOfLine => {
                match spacing {
                    BlockSpacing::None => self.write("{"),
                    BlockSpacing::SpaceBeforeOpen => self.write(" {"),
                }
                self.end_line();
                self.indent();
            }
            IndentStyle::NextLine => {
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

    pub fn empty_line(&mut self) {
        if self.consecutive_empty_lines < 1 {
            self.consecutive_empty_lines += 1;
            self.buffer.push_str("\n");
        }
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

fn format_argument_list(writer: &mut ProgramWriter, argument_list: &[Node<Argument>]) {
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
        Type::FieldReference(inner) => {
            writer.write(".");
            format_type(writer, inner.as_ref());
        }
        Type::Function {
            return_type,
            arguments,
        } => {
            format_type(writer, &Type::Builtin(*return_type));
            format_argument_list(writer, arguments)
        }
        Type::Pointer(inner) => {
            format_type(writer, inner);
            writer.write("*");
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
        InfixOp::LessThan => " < ",
        InfixOp::LessThanOrEquals => " <= ",
        InfixOp::GreaterThan => " > ",
        InfixOp::GreaterThanOrEquals => " >= ",
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
        Expression::Vector(x, y, z) => write!(writer, "'{} {} {}'", x, y, z).unwrap(),
        Expression::Identifier(identifier) => {
            writer.write(identifier);
        }
        Expression::Prefix(op, inner) => {
            // Simplify chained prefix ops
            match (op, inner.inner()) {
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

            match inner.inner() {
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
        Expression::Call(target, args) => {
            match target.inner() {
                Expression::Infix(_, _) => {
                    writer.write("(");
                    format_expression(writer, target);
                    writer.write(")");
                }
                _ => {
                    format_expression(writer, target);
                }
            }

            writer.write("(");
            for (i, argument) in args.iter().enumerate() {
                if i > 0 {
                    writer.write(", ");
                }

                format_expression(writer, argument);
            }
            writer.write(")");
        }
        Expression::FieldAccess(target, field_name) => {
            match target.inner() {
                Expression::Infix(_, _) => {
                    writer.write("(");
                    format_expression(writer, target);
                    writer.write(")");
                }
                _ => {
                    format_expression(writer, target);
                }
            }
            writer.write(".");
            writer.write(field_name);
        }
        Expression::FrameReference(frame_name) => {
            write!(writer, "${}", frame_name).unwrap();
        }
    }
}

fn format_comment(writer: &mut ProgramWriter, comment: &Comment, is_standalone: bool) {
    match comment {
        Comment::Line(line) => {
            writer.write("//");

            let first_char = line.chars().next();

            if let Some(ch) = first_char {
                if !ch.is_ascii_whitespace() {
                    writer.write(" ");
                }
            }

            writer.write(line);
        }

        // TODO: Handle multiline comments
        Comment::Block { content, is_inline } => {
            writer.write("/*");

            // Micro-optimization: do this without allocations (iterate char-by-char)
            let with_converted_newlines = content.replace("\r\n", "\n");
            writer.write(&with_converted_newlines);

            writer.write("*/");

            if is_standalone && !is_inline {
                writer.end_line();
            }
        }
    }
}

fn format_statements(writer: &mut ProgramWriter, statements: &[Node<Statement>]) {
    let mut previous = None;

    for statement in statements {
        format_statement(writer, statement, previous, StatementStyle::AddNewline);
        previous = Some(statement);
    }
}

fn format_if_case(writer: &mut ProgramWriter, case: &Node<IfCase>) {
    writer.write("if ");

    match case.condition.inner() {
        IfCondition::IfTrue(expr) => {
            writer.write("(");
            format_expression(writer, expr);
        }
        IfCondition::IfFalse(expr) => {
            writer.write("!(");
            format_expression(writer, expr);
        }
    }

    writer.write(")");

    // TODO: This probably doesn't work properly with K&R style
    if let Some(comments) = case.condition.comments_after() {
        for comment in comments {
            writer.write(" ");
            format_comment(writer, comment, false);
        }
    }

    format_block(writer, &case.body);
}

fn format_statement(
    writer: &mut ProgramWriter,
    statement: &Node<Statement>,
    previous: Option<&Node<Statement>>,
    style: StatementStyle,
) {
    if style == StatementStyle::Inline {
        assert!(statement.can_inline(), "format_statement called with style = StatementStyle::Inline, but the statement is not inlinable");
    }

    let write_comment_after = |writer: &mut ProgramWriter| {
        // TODO: Does this really work with multiline comments?
        if let Some(comments) = statement.comments_after() {
            for comment in comments {
                writer.write(" ");
                format_comment(writer, comment, false);
            }
        }
    };

    // Oh how I wish that impl Trait worked in closures :(
    let wrap_in_whitespace =
        |writer: &mut ProgramWriter, inner: Box<dyn FnOnce(&mut ProgramWriter)>| {
            if style == StatementStyle::AddNewline {
                writer.start_line();
            }

            inner(writer);
            write_comment_after(writer);

            if style == StatementStyle::AddNewline {
                writer.end_line();
            }
        };

    if style == StatementStyle::AddNewline {}

    match statement.inner() {
        // These are the only statements that can be inlined
        Statement::Expression(expr) => {
            wrap_in_whitespace(
                writer,
                Box::new(|writer| {
                    format_expression(writer, expr);
                    writer.write(";");
                }),
            );
        }
        Statement::Return(expr) => {
            wrap_in_whitespace(
                writer,
                Box::new(|writer| {
                    writer.write("return");

                    if let Some(expr) = expr {
                        writer.write(" ");
                        format_expression(writer, expr);
                    }

                    writer.write(";");
                }),
            );
        }
        Statement::Assignment { lvalue, rvalue } => {
            wrap_in_whitespace(
                writer,
                Box::new(|writer| {
                    format_expression(writer, lvalue);
                    writer.write(" = ");
                    format_expression(writer, rvalue);
                    writer.write(";");
                }),
            );
        }

        // And these ones can not be inlined
        Statement::Comment(comment @ Comment::Line(_)) => {
            writer.start_line();
            format_comment(writer, comment, true);
            writer.end_line();
        }
        Statement::Comment(comment @ Comment::Block { .. }) => {
            format_comment(writer, comment, true);
        }
        Statement::Block(block) => {
            format_block(writer, block);
        }
        Statement::Decl(decl) => {
            // format_declaration handles comment formatting

            let previous_decl = match previous {
                Some(node) => match node.inner() {
                    Statement::Decl(declaration) => Some(declaration),
                    _ => None,
                },
                _ => None,
            };

            format_declaration(writer, decl, previous_decl);
        }
        Statement::If {
            case,
            else_if,
            else_body,
            else_keyword,
        } => {
            writer.start_line();

            format_if_case(writer, case);

            for else_if_case in else_if {
                match writer.config.indent_style {
                    IndentStyle::EndOfLine => {
                        writer.write(" else ");
                    }
                    IndentStyle::NextLine => {
                        writer.end_line();
                        writer.start_line();
                        writer.write("else ")
                    }
                }

                format_if_case(writer, else_if_case);
            }

            if let Some(else_body) = else_body {
                match writer.config.indent_style {
                    IndentStyle::EndOfLine => {
                        writer.write(" else");
                    }
                    IndentStyle::NextLine => {
                        writer.end_line();
                        writer.start_line();
                        writer.write("else")
                    }
                }

                // TODO: K&R
                if let Some(comments) = else_keyword.as_ref().unwrap().comments_after() {
                    for comment in comments {
                        writer.write(" ");
                        format_comment(writer, comment, false);
                    }
                }

                format_block(writer, else_body);
            }

            writer.end_line();
        }
        Statement::Newline => writer.empty_line(),
    }
}

fn format_block(writer: &mut ProgramWriter, block: &Block) {
    writer.start_block(BlockSpacing::SpaceBeforeOpen);

    format_statements(writer, block.0.as_slice());

    writer.end_block();
}

fn format_inline_block(writer: &mut ProgramWriter, block: &Block) {
    assert!(
        block.0.len() == 1,
        "Inline blocks must have exactly one statement"
    );
    let statement = &block.0[0];

    writer.write("{ ");

    format_statement(writer, statement, None, StatementStyle::Inline);

    writer.write(" }");
}

fn format_declaration(
    writer: &mut ProgramWriter,
    decl: &Node<Declaration>,
    previous: Option<&Node<Declaration>>,
) {
    let write_comment_after = |writer: &mut ProgramWriter| {
        if let Some(comments) = decl.comments_after() {
            for comment in comments {
                writer.write(" ");
                format_comment(writer, comment, false);
            }
        }
    };

    match decl.inner() {
        Declaration::Newline => {
            writer.empty_line();
        }
        Declaration::Comment(comment @ Comment::Line(_)) => {
            writer.start_line();
            format_comment(writer, comment, true);
            writer.end_line();
        }
        Declaration::Comment(comment @ Comment::Block { .. }) => {
            if let Some(node) = previous {
                match node.inner() {
                    Declaration::Comment(Comment::Block { is_inline, .. }) if *is_inline => {
                        writer.write(" ");
                    }
                    _ => {}
                }
            }

            format_comment(writer, comment, true);
        }
        Declaration::Field { name, ty } => {
            writer.start_line();
            writer.write(".");
            format_type(writer, ty);
            writer.write(" ");
            writer.write(name);
            writer.write(";");

            write_comment_after(writer);

            writer.end_line();
        }
        Declaration::Binding {
            modifiers,
            ty,
            names,
        } => {
            writer.start_line();

            for modifier in modifiers.iter() {
                writer.write(match modifier {
                    BindingModifier::Const => "const",
                    BindingModifier::Var => "var",
                    BindingModifier::Nosave => "nosave",
                });
                writer.write(" ");
            }

            format_type(writer, ty);
            writer.write(" ");

            for (i, node) in names.iter().enumerate() {
                let BoundName { name, initializer } = node.inner();

                if i > 0 {
                    writer.write(", ");
                }

                writer.write(name);

                match initializer {
                    None => {}
                    Some(BindingInitializer::Block(block)) => {
                        writer.write(" =");
                        format_block(writer, block);
                    }
                    Some(BindingInitializer::Expr(expr)) => {
                        writer.write(" = ");
                        format_expression(writer, expr)
                    }
                    Some(BindingInitializer::BuiltinReference(id)) => {
                        write!(writer, " = #{}", id).unwrap();
                    }
                    Some(BindingInitializer::StateFunction {
                        frame,
                        callback,
                        body,
                    }) => {
                        write!(writer, " = [{}, {}]", frame, callback).unwrap();

                        if body.inner().0.len() == 1 {
                            writer.write(" ");
                            format_inline_block(writer, body);
                        } else {
                            format_block(writer, body);
                        }
                    }
                }
            }

            writer.write(";");

            write_comment_after(writer);

            writer.end_line();
        }
    }
}

pub fn format_program(program: Program, config: Option<Rc<FormatSettings>>) -> String {
    let mut writer = ProgramWriter::new(config);

    println!("{:#?}", program);

    let mut previous = None;

    for part in program.iter() {
        match part {
            ProgramPart::Declaration(decl) => {
                format_declaration(&mut writer, decl, previous);
                previous = Some(decl);
            }
            ProgramPart::ModelGen(ModelGenCommand(cmd)) => {
                writer.start_line();
                writer.write(cmd);
                writer.end_line();
            }
        }
    }

    writer.buffer
}
