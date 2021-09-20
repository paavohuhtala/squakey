pub enum BraceStyle {
    // K&R
    EndOfLine,
    // Allman
    NextLine,
}

pub struct FormatSettings {
    pub brace: BraceStyle,
}

impl Default for FormatSettings {
    fn default() -> Self {
        Self {
            brace: BraceStyle::NextLine,
        }
    }
}
