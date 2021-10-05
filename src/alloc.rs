use std::rc::Rc;

pub struct Alloc {
    strings: table::IStrTable,
}

pub enum Object {
    String(IStr),
}

/// Reference to an interned String
struct IStr {
    /// This will be changed to a raw pointer once a tracing GC is implemented
    data: Rc<str>,
    hash: u64,
}

mod table {
    use crate::alloc::IStr;
    use std::collections::HashMap;

    #[derive(Debug, Default)]
    pub struct IStrTable {
        map: HashMap<u64, IStr, StringHashBuilder>,
    }

    #[derive(Debug, Default)]
    struct StringHashBuilder;

    impl std::hash::BuildHasher for StringHashBuilder {
        type Hasher = ();

        fn build_hasher(&self) -> Self::Hasher {
            todo!()
        }
    }

    struct PrimitveHasher {}
}
