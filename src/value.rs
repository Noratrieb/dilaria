/// imagine interning or something here
pub type Symbol = String;

#[cfg(not(feature = "fxhash"))]
#[allow(clippy::disallowed_type)]
pub type HashMap<K, V> = std::collections::HashMap<K, V>;

#[cfg(feature = "fxhash")]
pub type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;
