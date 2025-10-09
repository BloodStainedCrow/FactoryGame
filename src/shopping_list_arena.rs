use std::{marker::PhantomData, mem::MaybeUninit};

// FIXME: Huge caviat! (i.e. this is very unsound and just a sketch)
// All function here assume that the caller will always match a key/Vec with the arena is was paired with. If the caller fails to do that, UB abound!

struct Arena<T> {
    data: Vec<MaybeUninit<T>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ArenaKey {
    /// Use u32 to allow a lot of indices but only use 4 bytes
    // TODO: Maybe NonZero?
    idx: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ArenaSliceKey {
    /// Use u32 to allow a lot of indices but only use 4 bytes
    // TODO: Maybe NonZero?
    idx: u32,
    len: u32,
}

impl ArenaSliceKey {
    fn index(self, index: u32) -> ArenaSliceIndexKey {
        if index >= self.len {
            panic!(
                "Index {} is out of bounds for slice of length {}",
                index, self.len
            );
        }

        ArenaSliceIndexKey {
            idx: self.idx + index,
        }
    }

    fn subslice(self, start: u32, end: u32) -> Self {
        if start > end {
            panic!()
        }

        if end >= self.len {
            panic!()
        }

        Self {
            idx: self.idx + start,
            len: (end - start),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ArenaSliceIndexKey {
    idx: u32,
}

impl ArenaSliceIndexKey {
    // NOT pub
    fn get_raw_key(self) -> ArenaKey {
        ArenaKey { idx: self.idx }
    }
}

struct DisjointArenaSlices<'a, T> {
    slf: &'a Arena<T>,
    slice: ArenaSliceKey,
}

impl<'a, T> DisjointArenaSlices<'a, T> {
    fn index_slice(&mut self, slice_index: u32) -> &mut T {
        let index = self.slice.index(slice_index);

        // No races since no two DisjointArenaSlices are ever allowed to overlap
        unsafe { self.slf.get_from_ref(index.get_raw_key()) }
    }

    /// The caller must ensure there is no data races or overlapping mut refs to the same index!
    unsafe fn index_full_arena(&mut self, key: ArenaKey) -> &mut T {
        unsafe { self.slf.get_from_ref(key) }
    }
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        todo!()
    }

    pub fn new_with_capacity(capacity: u32) -> Self {
        todo!()
    }

    pub fn add_value(&mut self, value: T) -> ArenaKey {
        todo!()
    }

    pub fn allocate_slice(&mut self, len: u32) -> ArenaSliceKey {
        todo!()
    }

    pub fn get_value(&self, key: ArenaKey) -> &T {
        todo!()
    }

    pub fn get_slice(&self, key: ArenaSliceKey) -> &[T] {
        let start = key.idx as usize;
        let end = key.idx as usize + key.len as usize;

        todo!()
    }

    pub fn get_slices_disjoint<const N: usize>(
        &mut self,
        slices: [ArenaSliceKey; N],
    ) -> [&mut [T]; N] {
        todo!()
    }

    /// The caller must ensure there is no data races or overlapping mut refs to the same index!
    pub unsafe fn get_from_ref(&self, key: ArenaKey) -> &mut T {
        todo!()
    }
}

struct ArenaVec<T> {
    allocation: ArenaSliceKey,
    len: u32,

    phantom: PhantomData<T>,
}

impl<T> ArenaVec<T> {
    pub fn new() -> Self {
        Self {
            allocation: ArenaSliceKey {
                idx: u32::MAX,
                len: 0,
            },
            len: 0,
            phantom: PhantomData,
        }
    }

    pub fn new_with_capacity(arena: &mut Arena<T>, capacity: u32) -> Self {
        let slice = arena.allocate_slice(capacity);

        Self {
            allocation: slice,
            len: 0,
            phantom: PhantomData,
        }
    }

    pub fn as_slice<'a>(&self, arena: &'a Arena<T>) -> &'a [T] {
        let slice = self.allocation.subslice(0, self.len);

        unsafe { arena.get_slice(slice) }
    }
}
