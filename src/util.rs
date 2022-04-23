/// Statically asserts that the size of the type is x bytes big (on 64-bit)
macro_rules! assert_size {
    ($name:ident <= $size:expr) => {
        #[cfg(target_pointer_width = "64")]
        const _: [(); $size] = [(); ::std::mem::size_of::<$name>()];
    };
}

pub(crate) use assert_size;
use std::fmt::Display;

#[cfg(feature = "_debug")]
pub fn dbg(prefix: impl Display, x: impl dbg_pls::DebugPls) {
    eprintln!("{prefix}{}", dbg_pls::pretty(&x))
}

#[cfg(not(feature = "_debug"))]
pub fn dbg(prefix: impl Display, x: impl std::fmt::Debug) {
    eprintln!("{prefix}{x:#?}");
}
