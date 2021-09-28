use crate::ast::{Node, Statement};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StatementStyle {
    Inline,
    AddNewline,
}

impl<'a> Node<'a, Statement<'a>> {
    /// Can this statement be written on a single line?
    pub fn can_inline(&self) -> bool {
        // If all comments are a single-line block comments, then we could inline
        if self.comments_after().is_some() {
            return false;
        }

        let statement = self.inner();
        match statement {
            Statement::Expression(_) => true,
            Statement::Assignment { .. } => true,
            Statement::Return(_) => true,

            Statement::Block { .. } => false,
            Statement::If { .. } => false,
            Statement::Comment(_) => false,
            Statement::Decl(_) => false,
            Statement::Newline => false,
            Statement::Switch { .. } => false,
            Statement::Break => false,
            Statement::Continue => false,
        }
    }
}
