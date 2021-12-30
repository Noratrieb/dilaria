#![allow(dead_code)]

use std::collections::LinkedList;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::ptr::NonNull;

/// imagine interning or something here
pub type Symbol<'ast> = bumpalo::collections::String<'ast>;

/// here is the actual interning or something
pub type NewSym = Gc<str>;

#[cfg(not(feature = "fxhash"))]
#[allow(clippy::disallowed_type)]
pub type HashMap<K, V> = std::collections::HashMap<K, V>;

#[cfg(feature = "fxhash")]
pub type HashMap<K, V> = rustc_hash::FxHashMap<K, V>;

#[cfg(not(feature = "fxhash"))]
#[allow(clippy::disallowed_type)]
pub type HashSet<T> = std::collections::HashSet<T>;

#[cfg(feature = "fxhash")]
pub type HashSet<T> = rustc_hash::FxHashSet<T>;

/// A pointer to a garbage collected value. This pointer *must* always be valid, and a value
/// is only allowed to be freed once no Gc is pointing at it anymore. This is achieved through
/// tracing through all objects from a few known roots and marking every reachable value. All other
/// values will be swept.
pub struct Gc<T: ?Sized> {
    ptr: NonNull<T>,
}

impl<T: ?Sized> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // SAFETY: Gc will always point to a valid T, since T will only be freed once all Gc are gone
        // This requires tracing through *all* roots without forgetting any
        // I would guess that there will be some errors with the garbage collector, but once they are
        // all fixed this will be sound. But who knows.
        unsafe { &*self.ptr.as_ptr() }
    }
}

impl<T: Debug + ?Sized> Debug for Gc<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        T::fmt(self, f)
    }
}

impl<T: ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Self { ..*self }
    }
}

impl<T: ?Sized> Copy for Gc<T> {}

enum Object {
    String(Gc<str>),
}

pub struct RtAlloc {
    symbols: HashSet<NonNull<str>>,
    objects: LinkedList<Object>,
}

impl RtAlloc {
    pub fn alloc_str(&mut self, str: &str) -> Gc<str> {
        let ptr = Box::into_raw(str.to_owned().into_boxed_str());
        // SAFETY: Box cannot be null
        let new_nonnull = unsafe { NonNull::new_unchecked(ptr) };
        let gc = Gc { ptr: new_nonnull };
        let object = Object::String(gc.clone());

        self.objects.push_back(object);

        gc
    }

    pub fn intern_string(&mut self, str: &str) -> NewSym {
        let original_nonnull = NonNull::from(str);

        if let Some(interned) = self.symbols.get(&original_nonnull) {
            return Gc { ptr: *interned };
        }

        self.alloc_str(str)
    }
}
