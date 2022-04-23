#![deny(clippy::disallowed_type)]

mod ast;
mod errors;

use crate::ast::Program;
use std::io::Write;

pub use bumpalo::Bump;

#[cfg(not(feature = "fxhash"))]
#[allow(clippy::disallowed_types)]
type HashMap<K, V> = std::collections::HashMap<K, V>;

#[cfg(feature = "fxhash")]
type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;

#[cfg(not(feature = "fxhash"))]
#[allow(clippy::disallowed_types)]
type HashSet<T> = std::collections::HashSet<T>;

#[cfg(feature = "fxhash")]
type HashSet<T> = rustc_hash::FxHashSet<T>;

pub fn process_ast(program: &str, ast: &Program) {
    dbg(ast);
}

pub fn dbg(x: impl dbg_pls::DebugPls) {
    eprintln!("{}", dbg_pls::pretty(&x))
}
