use std::{cmp::min, mem};

use rayon::iter::{
    IndexedParallelIterator, ParallelIterator,
    plumbing::{Producer, bridge},
};

pub fn split_arbitrary_mut<T>(
    slice: &mut [T],
    sizes: impl IntoIterator<Item = usize>,
) -> impl Iterator<Item = &mut [T]> {
    SplitArbitraryMut {
        slice,
        sizes: sizes.into_iter(),
    }
}

struct SplitArbitraryMut<'a, T, C: Iterator<Item = usize>> {
    slice: &'a mut [T],
    sizes: C,
}

impl<'a, T, C: Iterator<Item = usize>> Iterator for SplitArbitraryMut<'a, T, C> {
    type Item = &'a mut [T];

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.sizes.next();

        match next {
            Some(size) => {
                let slice = mem::take(&mut self.slice);
                let split = slice.split_at_mut(size);
                self.slice = split.1;
                Some(split.0)
            },
            None => None,
        }
    }
}

pub fn split_arbitrary_mut_slice<'a, 'b, T: Send>(
    slice: &'a mut [T],
    sizes: &'b [usize],
) -> impl IndexedParallelIterator<Item = &'a mut [T]> + use<'a, 'b, T> {
    SplitArbitraryMutSlice { slice, sizes }
}

struct SplitArbitraryMutSlice<'a, 'b, T> {
    slice: &'a mut [T],
    sizes: &'b [usize],
}

impl<'a, 'b, T> Iterator for SplitArbitraryMutSlice<'a, 'b, T> {
    type Item = &'a mut [T];

    fn next(&mut self) -> Option<Self::Item> {
        if self.sizes.is_empty() {
            return None;
        }

        let (next, rest) = self.sizes.split_at(1);
        self.sizes = rest;

        let &[next] = next else { unreachable!() };

        let slice = mem::take(&mut self.slice);
        let split = slice.split_at_mut(next);
        self.slice = split.1;
        Some(split.0)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.sizes.len(), Some(self.sizes.len()))
    }
}

impl<'a, 'b, T> ExactSizeIterator for SplitArbitraryMutSlice<'a, 'b, T> {}

impl<'a, 'b, T> DoubleEndedIterator for SplitArbitraryMutSlice<'a, 'b, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.sizes.is_empty() {
            return None;
        }

        let (rest, next_back) = self.sizes.split_at(self.sizes.len() - 1);
        self.sizes = rest;

        let &[next_back] = next_back else {
            unreachable!()
        };

        let slice = mem::take(&mut self.slice);
        let split = slice.split_at_mut(slice.len() - next_back);
        self.slice = split.0;
        Some(split.1)
    }
}

impl<'a, 'b, T: Send> ParallelIterator for SplitArbitraryMutSlice<'a, 'b, T> {
    type Item = &'a mut [T];

    fn drive_unindexed<C>(self, consumer: C) -> C::Result
    where
        C: rayon::iter::plumbing::UnindexedConsumer<Self::Item>,
    {
        bridge(self, consumer)
    }
}

impl<'a, 'b, T: Send> IndexedParallelIterator for SplitArbitraryMutSlice<'a, 'b, T> {
    fn len(&self) -> usize {
        self.sizes.len()
    }

    fn drive<C: rayon::iter::plumbing::Consumer<Self::Item>>(self, consumer: C) -> C::Result {
        bridge(self, consumer)
    }

    fn with_producer<CB: rayon::iter::plumbing::ProducerCallback<Self::Item>>(
        self,
        callback: CB,
    ) -> CB::Output {
        let producer = SplitArbitraryMutSlice {
            slice: self.slice,
            sizes: self.sizes,
        };
        callback.callback(producer)
    }
}

impl<'a, 'b, T: Send> Producer for SplitArbitraryMutSlice<'a, 'b, T> {
    type Item = &'a mut [T];
    type IntoIter = SplitArbitraryMutSlice<'a, 'b, T>;

    fn into_iter(self) -> Self::IntoIter {
        self
    }

    fn split_at(self, index: usize) -> (Self, Self) {
        let (left_size, right_size) = self.sizes.split_at(index);
        let left_sum = left_size.iter().sum();
        let (left, right) = self.slice.split_at_mut(min(left_sum, self.slice.len()));
        (
            Self {
                slice: left,
                sizes: left_size,
            },
            Self {
                slice: right,
                sizes: right_size,
            },
        )
    }
}
