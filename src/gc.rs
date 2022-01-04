//! The garbage collector for the language
//!
//! The structure of the GC might change, but for now it's simply a `LinkedList` of `Object`s.

use crate::vm::Value;
use crate::{HashMap, HashSet};
use std::collections::LinkedList;
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::ptr::NonNull;

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

/// An reference to an interned String. Hashing and Equality are O(1) and just look at the pointer address
#[derive(Clone, Copy)]
pub struct Symbol {
    gc: Gc<str>,
}

type ObjectMap = HashMap<Symbol, Value>;

/// A reference to an Object on the heap.
/// ```js
/// let x = {};
/// ```
/// This is inside the local x now.
#[derive(Clone, Copy)]
pub struct Object {
    gc: Gc<HeapObject>,
}

#[derive(Debug)]
#[repr(C)]
struct HeapObject {
    kind: HeapObjectKind,
}

#[derive(Debug)]
enum HeapObjectKind {
    String(Gc<str>),
    Object(ObjectMap),
}

#[derive(Debug)]
pub struct RtAlloc {
    symbols: HashSet<NonNullStrWrapper>,
    objects: LinkedList<HeapObject>,
}

#[derive(Debug)]
struct NonNullStrWrapper(NonNull<str>);

impl Hash for NonNullStrWrapper {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // SAFETY: Assume the ptr is valid, same rules as `Gc<T>`
        unsafe { self.0.as_ref().hash(state) }
    }
}

impl PartialEq for NonNullStrWrapper {
    fn eq(&self, other: &Self) -> bool {
        // SAFETY: Assume the ptr is valid, same rules as `Gc<T>`
        unsafe { self.0.as_ref().eq(other.0.as_ref()) }
    }
}

impl Eq for NonNullStrWrapper {}

impl RtAlloc {
    /// # Safety
    /// Promise to not forget to mark any roots and to not deref `Gc<T>` after you've dropped me 🥺
    pub unsafe fn new() -> Self {
        Self {
            symbols: HashSet::default(),
            objects: LinkedList::new(),
        }
    }

    fn alloc_str(&mut self, str: &str) -> Gc<str> {
        let ptr = Box::into_raw(str.to_owned().into_boxed_str());
        // SAFETY: Box cannot be null
        let new_nonnull = unsafe { NonNull::new_unchecked(ptr) };
        let gc = Gc { ptr: new_nonnull };
        let object = HeapObject {
            kind: HeapObjectKind::String(gc),
        };

        self.objects.push_back(object);

        gc
    }

    pub fn alloc_obj(&mut self, obj: ObjectMap) -> Object {
        self.objects.push_back(HeapObject {
            kind: HeapObjectKind::Object(obj),
        });

        let ptr = self.objects.back().unwrap();

        Object {
            gc: Gc {
                ptr: NonNull::from(ptr),
            },
        }
    }

    pub fn intern_string(&mut self, str: &str) -> Symbol {
        let original_nonnull = NonNull::from(str);

        if let Some(interned) = self.symbols.get(&NonNullStrWrapper(original_nonnull)) {
            Symbol::new(Gc { ptr: interned.0 })
        } else {
            let allocated = self.alloc_str(str);
            self.symbols.insert(NonNullStrWrapper(allocated.ptr));
            Symbol::new(allocated)
        }
    }
}

impl Drop for RtAlloc {
    fn drop(&mut self) {
        // free all interned strings
        for str in &self.symbols {
            let raw = str.0.as_ptr();
            // SAFETY: No one has free these, see `Gc<T>`
            drop(unsafe { Box::from_raw(raw) });
        }
    }
}

impl Symbol {
    pub fn new(gc: Gc<str>) -> Self {
        Self { gc }
    }

    fn address(&self) -> usize {
        self.gc.ptr.as_ptr() as *mut u8 as usize
    }

    pub fn as_str(&self) -> &str {
        &*self.gc
    }
}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.address().hash(state);
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.address() == other.address()
    }
}

impl Eq for Symbol {}

impl Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.as_str().fmt(f)
    }
}

impl Deref for Object {
    type Target = ObjectMap;

    fn deref(&self) -> &Self::Target {
        match self.gc.deref().kind {
            HeapObjectKind::Object(ref map) => map,
            _ => unreachable!(),
        }
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.gc.deref().fmt(f)
    }
}
