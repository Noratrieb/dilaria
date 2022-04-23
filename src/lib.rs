pub trait CoolTrait {}

impl<'a, D: CoolTrait + ?Sized> CoolTrait for &'a D {}

pub fn foo(parent: Parent<'_>) {
    requires_parent_fulfill_cool_trait(parent);
}

pub fn requires_parent_fulfill_cool_trait(_: impl CoolTrait) {}

pub enum Parent<'a> {
    A(&'a A<'a>),
    B(&'a B<'a>),
}

impl<'a> CoolTrait for Parent<'a>
where
    &'a A<'a>: CoolTrait,
    &'a B<'a>: CoolTrait,
{
}

pub struct A<'a> {
    parent: Parent<'a>,
}

impl<'a> CoolTrait for A<'a> where Parent<'a>: CoolTrait {}

pub struct B<'a> {
    parent: Parent<'a>,
}

impl<'a> CoolTrait for B<'a> where Parent<'a>: CoolTrait {}
