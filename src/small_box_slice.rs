use std::{
    mem::{ManuallyDrop, MaybeUninit, offset_of},
    ops::{Index, IndexMut},
    ptr::NonNull,
};

use memoffset::offset_of_union;

use static_assertions::const_assert_eq;

#[repr(C)]
pub union SmallBoxedSlice<A: Array> {
    inline: ManuallyDrop<Inline<A>>,
    heap: ManuallyDrop<Heap<A>>,
}

#[repr(C)]
struct Inline<A> {
    low_len: u8,
    data: MaybeUninit<A>,
}

#[repr(C)]
struct Heap<A: Array> {
    low_len: u8,
    middle_low_len: u8,
    middle_len: u16,
    high_len: u32,
    data: NonNull<A::Item>,
}

const_assert_eq!(
    offset_of!(Inline<[u16; 7]>, low_len),
    offset_of!(Heap<[u16; 7]>, low_len)
);

const_assert_eq!(
    offset_of_union!(SmallBoxedSlice<[u16; 7]>, inline),
    offset_of_union!(SmallBoxedSlice<[u16; 7]>, heap)
);

enum Variant {
    Inline,
    Heap,
}

impl<A: Array> Drop for SmallBoxedSlice<A> {
    fn drop(&mut self) {
        match self.get_variant() {
            Variant::Inline => {
                let len = self.get_len();

                for i in 0..len {
                    let v = self.get_mut(i).unwrap();

                    unsafe {
                        std::ptr::drop_in_place(v);
                    }
                }
            },
            Variant::Heap => unsafe {
                let mut full_ptr =
                    std::ptr::NonNull::slice_from_raw_parts(self.heap.data, self.get_len());

                let boxed_slice = Box::from_raw(full_ptr.as_mut());

                std::mem::drop(boxed_slice);
            },
        }
    }
}

impl<A: Array> SmallBoxedSlice<A> {
    pub fn from_vec(vec: Vec<A::Item>) -> Self {
        let len = vec.len();

        match Self::get_variant_for_len(len) {
            Variant::Inline => {
                let low_len: u8 = len.try_into().unwrap();
                let mut data = MaybeUninit::<A>::uninit();

                for (i, value) in vec.into_iter().enumerate() {
                    assert!(i <= len);

                    unsafe {
                        (data.as_mut_ptr() as *mut A::Item).add(i).write(value);
                    }
                }

                Self {
                    inline: ManuallyDrop::new(Inline { low_len, data }),
                }
            },
            Variant::Heap => {
                let (low_len, middle_low_len, middle_len, high_len) = Self::part_from_len(len);

                let heap_data = vec.into_boxed_slice();

                let ptr = NonNull::new(Box::into_raw(heap_data) as *mut A::Item).unwrap();

                Self {
                    heap: ManuallyDrop::new(Heap {
                        low_len,
                        middle_low_len,
                        middle_len,
                        high_len,
                        data: ptr,
                    }),
                }
            },
        }
    }

    fn get_variant_for_len(len: usize) -> Variant {
        if usize::from(len) > usize::from(A::CAPACITY) {
            Variant::Heap
        } else {
            Variant::Inline
        }
    }

    fn get_len_from_parts(low_len: u8, rest: (u8, u16, u32)) -> usize {
        usize::from(low_len)
            + (usize::from(rest.0) * 256)
            + (usize::from(rest.1) * 256 * 256)
            + (usize::try_from(rest.2).unwrap() * 256 * 256 * 65536)
    }

    fn part_from_len(len: usize) -> (u8, u8, u16, u32) {
        let low_len = (len % 256).try_into().unwrap();
        let low_middle_len = (len % (256 * 256) / 256).try_into().unwrap();
        let middle_len = (len % (256 * 256 * 65536) / 65536).try_into().unwrap();
        let high_len = (len / (65536 * 65536)).try_into().unwrap();

        (low_len, low_middle_len, middle_len, high_len)
    }

    fn get_low_len(&self) -> u8 {
        unsafe { self.inline.low_len }
    }

    fn get_variant(&self) -> Variant {
        Self::get_variant_for_len(usize::from(self.get_low_len()))
    }

    fn get_len(&self) -> usize {
        match self.get_variant() {
            Variant::Inline => usize::from(self.get_low_len()),
            Variant::Heap => unsafe {
                Self::get_len_from_parts(
                    self.get_low_len(),
                    (
                        self.heap.middle_low_len,
                        self.heap.middle_len,
                        self.heap.high_len,
                    ),
                )
            },
        }
    }

    pub fn get(&self, index: usize) -> Option<&A::Item> {
        if index >= self.get_len() {
            return None;
        }

        match self.get_variant() {
            Variant::Inline => unsafe {
                let ptr = self.inline.data.as_ptr() as *const A::Item;
                let value = ptr.add(index);

                Some(&*value)
            },
            Variant::Heap => unsafe {
                let value = self.heap.data.add(index);

                Some(value.as_ref())
            },
        }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut A::Item> {
        if index >= self.get_len() {
            return None;
        }

        match self.get_variant() {
            Variant::Inline => unsafe {
                let ptr = self.inline.data.as_mut_ptr() as *mut A::Item;
                let value = ptr.add(index);

                Some(&mut *value)
            },
            Variant::Heap => unsafe {
                let mut value = self.heap.data.add(index);

                Some(value.as_mut())
            },
        }
    }
}

impl<A: Array> Index<usize> for SmallBoxedSlice<A> {
    type Output = A::Item;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).expect(&format!(
            "Index out of bounds: {index} >= {}",
            self.get_len()
        ))
    }
}

impl<A: Array> IndexMut<usize> for SmallBoxedSlice<A> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        let len = self.get_len();
        self.get_mut(index)
            .expect(&format!("Index out of bounds: {index} >= {}", len))
    }
}

pub trait Array: Index<usize, Output = Self::Item> + IndexMut<usize> {
    type Item;
    const CAPACITY: u8;
}

impl<const N: usize, T> Array for [T; N] {
    type Item = T;
    const CAPACITY: u8 = N as u8;
}
