/// Statically asserts that the size of the type is x bytes big (on 64-bit)
macro_rules! assert_size {
    ($name:ident == $size:expr) => {
        #[cfg(target_pointer_width = "64")]
        const _: [(); $size] = [(); ::std::mem::size_of::<$name>()];
    };
}

pub(crate) use assert_size;
