pub trait DebugPls {
    fn fmt(&self, f: Formatter<'_>);
}

pub struct Formatter<'a> {
    _expr: &'a (),
}

impl<'a> Formatter<'a> {
}

impl<'a, D: DebugPls + ?Sized> DebugPls for &'a D {
    fn fmt(&self, _: Formatter<'_>) {
    }
}
