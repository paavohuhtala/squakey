
# My opionated QuakeC style

## Principles

## Newlines

- Unix newlines (`\n`) are always used.
  - Motivation: Unix newlines are simpler to handle. They are supported in all respectable text editors.
- Each statement is terminated with a newline, including the last statement of a file.
  - Motivation: This makes formatting rules easier to implement by not having special casing for the last statement.
- Optionally, one empty line is allowed between declarations / statements for grouping.
  - Motivation: Empty lines should be used group code into logical blocks. More than 1 empty line is seldom required. Your judgment is trusted; the formatter will not insert them for you.

## Blocks

- Braceless statements are never used.
  - Motivation: When each statement has its own braces, code is easier to refactor and extend. Not using braces can occasionally lead to bugs.
  