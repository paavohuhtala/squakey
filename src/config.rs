pub enum IndentStyle {
    // K&R
    EndOfLine,
    // Allman
    NextLine,
}

pub struct FormatSettings {
    pub indent_style: IndentStyle,
}

impl Default for FormatSettings {
    fn default() -> Self {
        Self {
            indent_style: IndentStyle::NextLine,
        }
    }
}
