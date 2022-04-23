pub fn foo(parent: Parent<'_>) {
    requires_parent_fulfill_trait(parent);
}

pub fn requires_parent_fulfill_trait(_: impl dbg_pls::DebugPls) {}

#[derive(dbg_pls::DebugPls)]
pub enum Parent<'a> {
    A(&'a A<'a>),
    B(&'a B<'a>),
}


#[derive(dbg_pls::DebugPls)]
pub struct A<'a> {
    parent: Parent<'a>,
}

#[derive(dbg_pls::DebugPls)]
pub struct B<'a> {
    parent: Parent<'a>,
}
