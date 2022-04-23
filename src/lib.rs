pub fn foo(parent: Parent<'_>) {
    requires_parent_fulfill_trait(parent);
}

pub fn requires_parent_fulfill_trait(_: impl dbg_pls::DebugPls) {}
pub enum Parent<'a> {
    A(&'a A<'a>),
    B(&'a B<'a>),
}

impl<'a> dbg_pls::DebugPls for Parent<'a>
where
    &'a A<'a>: dbg_pls::DebugPls,
    &'a B<'a>: dbg_pls::DebugPls,
{
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {}
}
pub struct A<'a> {
    parent: Parent<'a>,
}

impl<'a> dbg_pls::DebugPls for A<'a>
where
    Parent<'a>: dbg_pls::DebugPls,
{
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {}
}

pub struct B<'a> {
    parent: Parent<'a>,
}

impl<'a> dbg_pls::DebugPls for B<'a>
where
    Parent<'a>: dbg_pls::DebugPls,
{
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {}
}
