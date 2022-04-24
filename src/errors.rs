//!
//! This modules handles error reporting in the interpreter
//!
//! The `span` submodule handles Spans, which are used for tracking locations in the source code.
//!
//! There is a single type `CompilerError` that can be created from anywhere, and reported using
//! functions from here.

use std::fmt::Debug;

pub use span::Span;

mod span {
    use std::fmt::{Debug, Formatter};

    #[derive(Default, Copy, Clone, PartialOrd, PartialEq, Ord, Eq, Hash)]
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

    impl Debug for Span {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            f.debug_tuple("Span")
                .field(&(self.start..self.end))
                .finish()
        }
    }

    #[cfg(feature = "_debug")]
    impl dbg_pls::DebugPls for Span {
        fn fmt(&self, f: dbg_pls::Formatter<'_>) {
            f.debug_tuple_struct("Span")
                // todo: wait for https://github.com/conradludgate/dbg-pls/pull/1
                .field(&format!("{:?}", (self.start..self.end)))
                .finish()
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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

            eprintln!("{}error: {}{}", RED, error.message, RESET);
            eprintln!("      {}|{}", CYAN, RESET);
            eprintln!("{}{:>5} |{} {}", CYAN, idx + 1, RESET, line);
            eprint!("      {}|{} ", CYAN, RESET);
            eprintln!(
                "{}{}{}{}",
                " ".repeat(offset_on_line),
                RED,
                "^".repeat(span.len()),
                RESET,
            );
            if let Some(note) = error.note {
                eprintln!("      {}|{}", CYAN, RESET);
                eprintln!(
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
