//!
//! This modules handles error reporting in the interpreter

use std::fmt::Debug;

pub use span::Span;

mod span {

    #[derive(Debug, Default, Copy, Clone, PartialOrd, PartialEq, Ord, Eq, Hash)]
    pub struct Span {
        pub start: usize,
        pub end: usize,
    }

    impl Span {
        pub fn start_len(start: usize, len: usize) -> Self {
            Self {
                start,
                end: start + len,
            }
        }

        pub fn start_end(start: usize, end: usize) -> Self {
            Self { start, end }
        }

        pub fn single(start: usize) -> Self {
            Self {
                start,
                end: start + 1,
            }
        }

        pub fn dummy() -> Self {
            Self { start: 0, end: 0 }
        }

        /// Extends the span by the second one
        /// The other one has to be after the current one
        pub fn extend(&self, other: Span) -> Span {
            debug_assert!(self.start <= other.start);
            debug_assert!(self.end <= other.end);
            Span {
                start: self.start,
                end: other.end,
            }
        }

        /// Extends the span by the second one, if it exists
        /// The other one has to be after the current one, if it exists
        pub fn option_extend(&self, other: Option<Span>) -> Span {
            match other {
                None => *self,
                Some(span) => self.extend(span),
            }
        }

        pub fn len(&self) -> usize {
            self.end - self.start
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompilerError {
    pub span: Span,
    pub message: String,
    pub note: Option<String>,
}

impl CompilerError {
    pub fn new(span: Span, message: String) -> Self {
        Self {
            span,
            message,
            note: None,
        }
    }

    pub fn with_note(span: Span, message: String, note: String) -> Self {
        Self {
            span,
            message,
            note: Some(note),
        }
    }
}

pub fn display_error(source: &str, error: CompilerError) {
    let span = error.span;

    let mut chars = 0;
    let lines = source.split_inclusive('\n').enumerate();
    for (idx, line) in lines {
        if chars + line.len() > span.start {
            let offset_on_line = span.start - chars;

            println!("{}error: {}{}", RED, error.message, RESET);
            println!("      {}|{}", CYAN, RESET);
            println!("{}{:>5} |{} {}", CYAN, idx + 1, RESET, line);
            print!("      {}|{} ", CYAN, RESET);
            println!(
                "{}{}{}{}",
                " ".repeat(offset_on_line),
                RED,
                "^".repeat(span.len()),
                RESET,
            );
            if let Some(note) = error.note {
                println!("      {}|{}", CYAN, RESET);
                println!(
                    "      {}|{}   {}note: {}{}",
                    CYAN, RESET, GREEN, note, RESET
                );
            }
            break;
        }
        chars += line.len();
    }
}

macro_rules! color {
    ($name:ident: $value:literal) => {
        const $name: &str = concat!("\x1B[", $value);
    };
}

color!(RED: "0;31m");
color!(RESET: "0m");
color!(CYAN: "0;36m");
color!(GREEN: "0;32m");
