#![cfg_attr(docsrs, feature(doc_cfg))]
#![warn(clippy::pedantic)]
#![forbid(unsafe_code)]

use syn::__private::{Span, TokenStream2};

mod impls;

mod debug_list;
mod debug_map;
mod debug_set;
mod debug_struct;
mod debug_tuple;
mod debug_tuple_struct;
pub use debug_list::DebugList;
pub use debug_map::DebugMap;
pub use debug_set::DebugSet;
pub use debug_struct::DebugStruct;
pub use debug_tuple::DebugTuple;
pub use debug_tuple_struct::DebugTupleStruct;

pub trait DebugPls {
    fn fmt(&self, f: Formatter<'_>);
}

pub struct Formatter<'a> {
    expr: &'a mut syn::Expr,
}

impl<'a> Formatter<'a> {
    pub(crate) fn process(value: &dyn DebugPls) -> syn::Expr {
        let mut expr = syn::Expr::Verbatim(TokenStream2::new());
        value.fmt(Formatter { expr: &mut expr });
        expr
    }

    pub fn write_expr(self, expr: impl Into<syn::Expr>) {
        *self.expr = expr.into();
    }

    #[must_use]
    pub fn debug_struct(self, name: &str) -> DebugStruct<'a> {
        DebugStruct::new(self, name)
    }

    #[must_use]
    pub fn debug_tuple(self) -> DebugTuple<'a> {
        DebugTuple::new(self)
    }

    #[must_use]
    pub fn debug_tuple_struct(self, name: &str) -> DebugTupleStruct<'a> {
        DebugTupleStruct::new(self, name)
    }

    #[must_use]
    pub fn debug_list(self) -> DebugList<'a> {
        DebugList::new(self)
    }

    #[must_use]
    pub fn debug_map(self) -> DebugMap<'a> {
        DebugMap::new(self)
    }

    #[must_use]
    pub fn debug_set(self) -> DebugSet<'a> {
        DebugSet::new(self)
    }

    pub fn debug_ident(self, name: &str) {
        let path: syn::Path = syn::Ident::new(name, Span::call_site()).into();
        self.write_expr(syn::ExprPath {
            attrs: vec![],
            qself: None,
            path,
        });
    }
}
