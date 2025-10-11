use std::{cmp::max, marker::PhantomData};

use bitvec::{field::BitField, order::Lsb0, slice::BitSlice, vec::BitVec, view::BitViewSized};

use crate::item::ITEMCOUNTTYPE;
#[cfg(feature = "client")]
use egui_show_info::{EguiDisplayable, InfoExtractor};
#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size::GetSize;

use super::{FakeUnionStorage, storage_storage_with_buckets::InserterId};

#[derive(Debug, Clone, Copy, PartialEq, serde::Deserialize, serde::Serialize)]
pub(super) struct EncodedUpdatingInserter {
    pub(super) storage_id_in: FakeUnionStorage,
    pub(super) storage_id_out: FakeUnionStorage,
    pub(super) max_hand_size: ITEMCOUNTTYPE,
    pub(super) current_hand: ITEMCOUNTTYPE,
    pub(super) id: InserterId,
}

impl FakeUnionStorage {
    fn into_bits(
        self,
        bits_for_index: u32,
        bits_for_grid_or_static: u32,
        bits_for_recipe: u32,
    ) -> impl Iterator<Item = bool> {
        let index = self.index;
        let index = index
            .into_bitarray::<Lsb0>()
            .into_iter()
            .take(bits_for_index.try_into().unwrap());
        let grid = self.grid_or_static_flag;
        let grid = grid
            .into_bitarray::<Lsb0>()
            .into_iter()
            .take(bits_for_grid_or_static.try_into().unwrap());
        let recipe = self.recipe_idx_with_this_item;
        let recipe = recipe
            .into_bitarray::<Lsb0>()
            .into_iter()
            .take(bits_for_recipe.try_into().unwrap());

        index.chain(grid).chain(recipe)
    }

    fn from_bits(
        data: &BitSlice,
        bits_for_index: u32,
        bits_for_grid_or_static: u32,
        bits_for_recipe: u32,
    ) -> Self {
        let (index, data) = data.split_at(bits_for_index.try_into().unwrap());
        let (grid, data) = data.split_at(bits_for_grid_or_static.try_into().unwrap());
        let (recipe, data) = data.split_at(bits_for_recipe.try_into().unwrap());

        let index = index.load();
        let grid = grid.load();
        let recipe = recipe.load();

        debug_assert!(data.is_empty());

        Self {
            index,
            grid_or_static_flag: grid,
            recipe_idx_with_this_item: recipe,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, serde::Deserialize, serde::Serialize)]
pub struct Info {
    bits_for_inserter_id: u32,
    bits_for_hands: u32,
    bits_for_index: u32,
    bits_for_grid_or_static: u32,
    bits_for_recipe: u32,
}

impl BitCompressable for EncodedUpdatingInserter {
    type Info = Info;
    fn get_num_bits(
        Info {
            bits_for_inserter_id,
            bits_for_hands,
            bits_for_index,
            bits_for_grid_or_static,
            bits_for_recipe,
        }: Info,
    ) -> usize {
        (bits_for_inserter_id
            + 2 * bits_for_hands
            + 2 * (bits_for_index + bits_for_grid_or_static + bits_for_recipe))
            .try_into()
            .unwrap()
    }

    fn into_bits(
        self,
        Info {
            bits_for_inserter_id,
            bits_for_hands,
            bits_for_index,
            bits_for_grid_or_static,
            bits_for_recipe,
        }: Info,
    ) -> impl Iterator<Item = bool> {
        let Self {
            storage_id_in,
            storage_id_out,
            max_hand_size,
            current_hand,
            id,
        } = self;

        let sid_in =
            storage_id_in.into_bits(bits_for_index, bits_for_grid_or_static, bits_for_recipe);
        let sid_out =
            storage_id_out.into_bits(bits_for_index, bits_for_grid_or_static, bits_for_recipe);

        let max = max_hand_size
            .into_bitarray::<Lsb0>()
            .into_iter()
            .take(bits_for_hands.try_into().unwrap());
        let current = current_hand
            .into_bitarray::<Lsb0>()
            .into_iter()
            .take(bits_for_hands.try_into().unwrap());
        let id =
            id.0.into_bitarray::<Lsb0>()
                .into_iter()
                .take(bits_for_inserter_id.try_into().unwrap());

        sid_in.chain(sid_out).chain(max).chain(current).chain(id)
    }

    fn from_bits(
        data: &BitSlice,
        Info {
            bits_for_inserter_id,
            bits_for_hands,
            bits_for_index,
            bits_for_grid_or_static,
            bits_for_recipe,
        }: Info,
    ) -> Self {
        debug_assert_eq!(
            data.len() as u32,
            (bits_for_inserter_id
                + 2 * bits_for_hands
                + 2 * (bits_for_index + bits_for_grid_or_static + bits_for_recipe))
        );

        let (sid_in, data) = data.split_at(
            (bits_for_index + bits_for_grid_or_static + bits_for_recipe)
                .try_into()
                .unwrap(),
        );
        let (sid_out, data) = data.split_at(
            (bits_for_index + bits_for_grid_or_static + bits_for_recipe)
                .try_into()
                .unwrap(),
        );

        let (max, data) = data.split_at(bits_for_hands.try_into().unwrap());
        let (current, data) = data.split_at(bits_for_hands.try_into().unwrap());
        let (id, data) = data.split_at(bits_for_inserter_id.try_into().unwrap());

        debug_assert!(data.is_empty());

        let sid_in = FakeUnionStorage::from_bits(
            sid_in,
            bits_for_index,
            bits_for_grid_or_static,
            bits_for_recipe,
        );
        let sid_out = FakeUnionStorage::from_bits(
            sid_out,
            bits_for_index,
            bits_for_grid_or_static,
            bits_for_recipe,
        );

        let max = max.load();
        let current = current.load();
        let id = InserterId(id.load());

        Self {
            storage_id_in: sid_in,
            storage_id_out: sid_out,
            max_hand_size: max,
            current_hand: current,
            id,
        }
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub(super) struct BitInserterStorageInfo {
    pub(super) max_inserter_id: u8,
    pub(super) max_max_hand_size: u8,

    pub(super) max_index: u32,
    pub(super) max_grid: u16,
    pub(super) max_recipe: u16,
}

impl From<BitInserterStorageInfo> for Info {
    fn from(info: BitInserterStorageInfo) -> Self {
        let bits_for_inserter_id = max(u8::BITS - info.max_inserter_id.leading_zeros(), 1);
        let bits_for_hands = max(u8::BITS - info.max_max_hand_size.leading_zeros(), 1);

        let bits_for_index = max(u32::BITS - info.max_index.leading_zeros(), 1);
        let bits_for_grid = max(u16::BITS - info.max_grid.leading_zeros(), 1);
        let bits_for_recipe = max(u16::BITS - info.max_recipe.leading_zeros(), 1);
        Self {
            bits_for_inserter_id,
            bits_for_hands,
            bits_for_index,
            bits_for_grid_or_static: bits_for_grid,
            bits_for_recipe,
        }
    }
}

impl<T: BitCompressable> Extend<T> for FixedSizeCompressedVec<T> {
    fn extend<Iter: IntoIterator<Item = T>>(&mut self, new: Iter) {
        self.data
            .extend(new.into_iter().flat_map(|value| value.into_bits(self.info)))
    }
}

impl<T: BitCompressable> FixedSizeCompressedVec<T> {
    pub fn new(info: impl Into<T::Info>) -> Self {
        Self {
            data: BitVec::new(),

            info: info.into(),

            phantom: PhantomData,
        }
    }

    pub fn clear(&mut self) {
        self.data.clear()
    }

    pub fn change_info(&mut self, new_info: impl Into<T::Info>) {
        let new_info = new_info.into();
        if self.info == new_info {
            return;
        }

        let values: Vec<_> = self.iter().collect();
        self.clear();

        assert!(self.data.is_empty());
        assert_eq!(self.len(), 0);

        self.info = new_info;
        self.extend(values);
    }

    pub fn push(&mut self, value: T) {
        self.data.extend(value.into_bits(self.info));
    }

    pub fn get_info(&self) -> &T::Info {
        &self.info
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        let size = T::get_num_bits(self.info);
        self.data.len() / size
    }

    #[inline(always)]
    pub fn get(&self, index: usize) -> T {
        let size = T::get_num_bits(self.info);
        let slice = &self.data[(size * index)..(size * (index + 1))];
        T::from_bits(slice, self.info)
    }

    #[inline(always)]
    pub fn set(&mut self, index: usize, value: T) {
        let size = T::get_num_bits(self.info);
        let slice = &mut self.data[(size * index)..(size * (index + 1))];

        let mut res = value.into_bits(self.info);
        slice.fill_with(|_| res.next().unwrap());
    }

    pub fn iter(&self) -> impl Iterator<Item = T> + ExactSizeIterator + DoubleEndedIterator {
        self.data
            .chunks(T::get_num_bits(self.info))
            .into_iter()
            .map(move |chunk| T::from_bits(chunk, self.info))
    }

    pub fn iter_mut(&mut self, mut f: impl FnMut(&mut T)) {
        let len = self.len();
        for index in 0..len {
            let mut value = self.get(index);
            f(&mut value);
            self.set(index, value);
        }
    }

    pub fn iter_mut_dedup(&mut self, mut f: impl FnMut(&mut T) -> bool) {
        let len = self.len();
        for index in 0..len {
            let mut value = self.get(index);
            if f(&mut value) {
                self.set(index, value);
            }
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.len() > 0 {
            let ret = self.get(self.len() - 1);

            let size = T::get_num_bits(self.info);

            self.data.resize(self.data.len() - size, false);

            Some(ret)
        } else {
            None
        }
    }

    pub fn swap(&mut self, first: usize, second: usize) {
        let first_val = self.get(first);
        let second_val = self.get(second);

        self.set(first, second_val);
        self.set(second, first_val);
    }

    pub fn swap_remove(&mut self, index: usize) -> T {
        self.swap(index, self.len() - 1);

        self.pop().unwrap()
    }

    #[inline(never)]
    pub fn extract_if(&mut self, mut f: impl FnMut(&mut T) -> bool) -> impl Iterator<Item = T>
    // FIXME: I need unsafe to remove this
    where
        T: Clone,
    {
        let size = T::get_num_bits(self.info);
        assert!(size <= usize::BITS as usize);
        let len = self.len();
        // FIXME: This can totally be improved using unsafe
        let mut extracted = vec![];
        let mut extracted_indices = BitVec::<usize>::with_capacity(len);
        self.iter_mut_dedup(|value| {
            let extract = f(value);

            if extract {
                extracted.push(value.clone());
            }

            extracted_indices.push(extract);

            !extract
        });

        self.data
            .retain(|index, _| !extracted_indices[index / size]);

        extracted.into_iter()
    }
}

pub(super) trait BitCompressable: Copy + 'static {
    type Info: Copy + PartialEq;

    fn get_num_bits(info: Self::Info) -> usize;

    fn into_bits(self, info: Self::Info) -> impl Iterator<Item = bool>;
    fn from_bits(data: &BitSlice, info: Self::Info) -> Self;
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub(super) struct FixedSizeCompressedVec<T: BitCompressable> {
    data: BitVec,
    info: T::Info,

    phantom: PhantomData<T>,
}

#[cfg(feature = "client")]
impl<T: BitCompressable> get_size::GetSize for FixedSizeCompressedVec<T> {
    fn get_heap_size(&self) -> usize {
        self.data.len() / 8
    }
}

#[cfg(feature = "client")]
impl<T: BitCompressable, E: InfoExtractor<Self, Info>, Info: EguiDisplayable>
    egui_show_info::ShowInfo<E, Info> for FixedSizeCompressedVec<T>
{
    fn show_fields<C: egui_show_info::Cache<String, Info>>(
        &self,
        extractor: &mut E,
        ui: &mut egui::Ui,
        path: String,
        cache: &mut C,
    ) {
    }
}

#[cfg(test)]
mod test {
    use std::cmp::max;

    use super::*;
    use ::test::Bencher;
    use ::test::black_box;
    use itertools::Itertools;
    use proptest::prop_assert_eq;
    use proptest::prop_assume;
    use proptest::proptest;
    use rayon::iter::IndexedParallelIterator;
    use rayon::iter::IntoParallelRefMutIterator;
    use rayon::iter::ParallelIterator;

    use crate::inserter::FakeUnionStorage;
    use crate::inserter::bucket_bit_compressed::InserterId;

    proptest! {
        #[test]
        fn test_to_bit_from_bit_round_trip((index_in, index_out) in (0..1_000_000u32,0..1_000_000u32), (grid_in, grid_out) in (0..200u16,0..200u16), (recipe_in, recipe_out) in (0..200u16,0..200u16), id in 0..255u8, current_hand in 0..12u8, max_hand_size in 0..12u8) {
            prop_assume!(current_hand <= max_hand_size);
            let ins = EncodedUpdatingInserter {
                storage_id_in: FakeUnionStorage { index: index_in, grid_or_static_flag: grid_in, recipe_idx_with_this_item: recipe_in },
                storage_id_out: FakeUnionStorage {index: index_out, grid_or_static_flag: grid_out, recipe_idx_with_this_item: recipe_out },
                max_hand_size,
                current_hand,
                id: InserterId(id),
            };

            let info = BitInserterStorageInfo { max_inserter_id: id, max_max_hand_size: max_hand_size, max_index: max(index_in, index_out), max_grid: max(grid_in, grid_out), max_recipe: max(recipe_in, recipe_out) }.into();

            let bits: BitVec = ins.into_bits(info).collect();

            let after = EncodedUpdatingInserter::from_bits(&bits, info);
            prop_assert_eq!(ins, after);
        }

        #[test]
        fn test_bit_vector_round_trip((index_in, index_out) in (0..1_000_000u32,0..1_000_000u32), (grid_in, grid_out) in (0..200u16,0..200u16), (recipe_in, recipe_out) in (0..200u16,0..200u16), id in 0..255u8, current_hand in 0..12u8, max_hand_size in 0..12u8) {
            prop_assume!(current_hand <= max_hand_size);
            let ins = EncodedUpdatingInserter {
                storage_id_in: FakeUnionStorage { index: index_in, grid_or_static_flag: grid_in, recipe_idx_with_this_item: recipe_in },
                storage_id_out: FakeUnionStorage {index: index_out, grid_or_static_flag: grid_out, recipe_idx_with_this_item: recipe_out },
                max_hand_size,
                current_hand,
                id: InserterId(id),
            };

            let info: Info = BitInserterStorageInfo { max_inserter_id: id, max_max_hand_size: max_hand_size, max_index: max(index_in, index_out), max_grid: max(grid_in, grid_out), max_recipe: max(recipe_in, recipe_out) }.into();

            let mut vec = FixedSizeCompressedVec::new(info);

            vec.extend(std::iter::once(ins));
            vec.extend(std::iter::once(ins));
            vec.extend(std::iter::once(ins));
            vec.extend(std::iter::once(ins));

            prop_assert_eq!(vec.get(0), ins);
        }

        #[test]
        fn test_bit_vector_extend_multiple((index_in, index_out) in (0..1_000_000u32,0..1_000_000u32), (grid_in, grid_out) in (0..200u16,0..200u16), (recipe_in, recipe_out) in (0..200u16,0..200u16), id in 0..255u8, current_hand in 0..12u8, max_hand_size in 0..12u8) {
            prop_assume!(current_hand <= max_hand_size);
            let ins = EncodedUpdatingInserter {
                storage_id_in: FakeUnionStorage { index: index_in, grid_or_static_flag: grid_in, recipe_idx_with_this_item: recipe_in },
                storage_id_out: FakeUnionStorage {index: index_out, grid_or_static_flag: grid_out, recipe_idx_with_this_item: recipe_out },
                max_hand_size,
                current_hand,
                id: InserterId(id),
            };

            let info: Info = BitInserterStorageInfo { max_inserter_id: id, max_max_hand_size: max_hand_size, max_index: max(index_in, index_out), max_grid: max(grid_in, grid_out), max_recipe: max(recipe_in, recipe_out) }.into();

            let mut vec = FixedSizeCompressedVec::new(info);

            vec.extend(std::iter::repeat(ins).take(100));

            for i in 0..100 {
                prop_assert_eq!(vec.get(i), ins);
            }
        }

        #[test]
        fn test_bit_vector_iter_mut((index_in, index_out) in (0..1_000_000u32,0..1_000_000u32), (grid_in, grid_out) in (0..200u16,0..200u16), (recipe_in, recipe_out) in (0..200u16,0..200u16), id in 0..255u8, current_hand in 0..12u8, max_hand_size in 0..12u8) {
            prop_assume!(current_hand <= max_hand_size);
            let ins = EncodedUpdatingInserter {
                storage_id_in: FakeUnionStorage { index: index_in, grid_or_static_flag: grid_in, recipe_idx_with_this_item: recipe_in },
                storage_id_out: FakeUnionStorage {index: index_out, grid_or_static_flag: grid_out, recipe_idx_with_this_item: recipe_out },
                max_hand_size,
                current_hand,
                id: InserterId(id),
            };

            let info: Info = BitInserterStorageInfo { max_inserter_id: id, max_max_hand_size: max_hand_size, max_index: max(index_in, index_out), max_grid: max(grid_in, grid_out), max_recipe: max(recipe_in, recipe_out) }.into();

            let mut vec = FixedSizeCompressedVec::new(info);

            vec.extend(std::iter::once(ins));

            vec.iter_mut(|ins| {
                ins.current_hand = ins.max_hand_size;
            });

            prop_assert_eq!(vec.get(0), EncodedUpdatingInserter { storage_id_in: ins.storage_id_in, storage_id_out: ins.storage_id_out, max_hand_size: ins.max_hand_size, current_hand: ins.max_hand_size, id: ins.id });
        }


        #[test]
        fn test_bit_vector_extract_if_not_extracted((index_in, index_out) in (0..1_000_000u32,0..1_000_000u32), (grid_in, grid_out) in (0..200u16,0..200u16), (recipe_in, recipe_out) in (0..200u16,0..200u16), id in 0..255u8, current_hand in 0..12u8, max_hand_size in 0..12u8) {
            prop_assume!(current_hand <= max_hand_size);
            let ins = EncodedUpdatingInserter {
                storage_id_in: FakeUnionStorage { index: index_in, grid_or_static_flag: grid_in, recipe_idx_with_this_item: recipe_in },
                storage_id_out: FakeUnionStorage {index: index_out, grid_or_static_flag: grid_out, recipe_idx_with_this_item: recipe_out },
                max_hand_size,
                current_hand,
                id: InserterId(id),
            };

            let info: Info = BitInserterStorageInfo { max_inserter_id: id, max_max_hand_size: max_hand_size, max_index: max(index_in, index_out), max_grid: max(grid_in, grid_out), max_recipe: max(recipe_in, recipe_out) }.into();

            let mut vec = FixedSizeCompressedVec::new(info);

            vec.extend(std::iter::once(ins));

            let extracted = vec.extract_if(|ins| {
                ins.current_hand = ins.max_hand_size;
                false
            });

            prop_assert_eq!(extracted.collect_vec(), vec![]);
            prop_assert_eq!(vec.get(0), EncodedUpdatingInserter { storage_id_in: ins.storage_id_in, storage_id_out: ins.storage_id_out, max_hand_size: ins.max_hand_size, current_hand: ins.max_hand_size, id: ins.id });
        }

        #[test]
        fn test_bit_vector_extract_if((index_in, index_out) in (0..1_000_000u32,0..1_000_000u32), (grid_in, grid_out) in (0..200u16,0..200u16), (recipe_in, recipe_out) in (0..200u16,0..200u16), id in 0..255u8, current_hand in 0..12u8, max_hand_size in 0..12u8) {
            prop_assume!(current_hand <= max_hand_size);
            let ins = EncodedUpdatingInserter {
                storage_id_in: FakeUnionStorage { index: index_in, grid_or_static_flag: grid_in, recipe_idx_with_this_item: recipe_in },
                storage_id_out: FakeUnionStorage {index: index_out, grid_or_static_flag: grid_out, recipe_idx_with_this_item: recipe_out },
                max_hand_size,
                current_hand,
                id: InserterId(id),
            };

            let info: Info = BitInserterStorageInfo { max_inserter_id: id, max_max_hand_size: max_hand_size, max_index: max(index_in, index_out), max_grid: max(grid_in, grid_out), max_recipe: max(recipe_in, recipe_out) }.into();

            let mut vec = FixedSizeCompressedVec::new(info);

            vec.extend(std::iter::once(ins));

            let extracted = vec.extract_if( |ins| {
                ins.current_hand = ins.max_hand_size;
                true
            });

            prop_assert_eq!(extracted.collect_vec(), vec![EncodedUpdatingInserter { storage_id_in: ins.storage_id_in, storage_id_out: ins.storage_id_out, max_hand_size: ins.max_hand_size, current_hand: ins.max_hand_size, id: ins.id }]);
            prop_assert_eq!(vec.len(), 0);
        }
    }

    const NUM: usize = 10_000;

    #[bench]
    fn bench_extract_if_low_extraction_rate(b: &mut Bencher) {
        let info: Info = BitInserterStorageInfo {
            max_inserter_id: 5,
            max_max_hand_size: 12,
            max_index: 80_000,
            max_grid: 3,
            max_recipe: 10,
        }
        .into();

        let ins = EncodedUpdatingInserter {
            storage_id_in: FakeUnionStorage {
                index: 11_322,
                grid_or_static_flag: 1,
                recipe_idx_with_this_item: 2,
            },
            storage_id_out: FakeUnionStorage {
                index: 75_111,
                grid_or_static_flag: 1,
                recipe_idx_with_this_item: 8,
            },
            max_hand_size: 12,
            current_hand: 5,
            id: InserterId(3),
        };

        let mut source = FixedSizeCompressedVec::new(info);
        source.extend(std::iter::repeat(ins).take(NUM));
        let mut dest = FixedSizeCompressedVec::new(info);

        let mut num_iterations = 0;
        b.iter(|| {
            let extracted = source.extract_if(|_| rand::random::<u8>() < 20);

            dest.extend(extracted);

            source.extend(dest.extract_if(|_| true));

            num_iterations += 1;
        });

        dbg!(source.len());
        dbg!(dest.len());

        dbg!(num_iterations);
    }

    #[bench]
    fn bench_extract_if_high_extraction_rate(b: &mut Bencher) {
        let info: Info = BitInserterStorageInfo {
            max_inserter_id: 5,
            max_max_hand_size: 12,
            max_index: 80_000,
            max_grid: 3,
            max_recipe: 10,
        }
        .into();

        let ins = EncodedUpdatingInserter {
            storage_id_in: FakeUnionStorage {
                index: 11_322,
                grid_or_static_flag: 1,
                recipe_idx_with_this_item: 2,
            },
            storage_id_out: FakeUnionStorage {
                index: 75_111,
                grid_or_static_flag: 1,
                recipe_idx_with_this_item: 8,
            },
            max_hand_size: 12,
            current_hand: 5,
            id: InserterId(3),
        };

        let mut source = FixedSizeCompressedVec::new(info);
        source.extend(std::iter::repeat(ins).take(NUM));
        let mut dest = FixedSizeCompressedVec::new(info);

        let mut num_iterations = 0;
        b.iter(|| {
            let extracted = source.extract_if(|_| rand::random::<u8>() >= 20);

            dest.extend(extracted);

            source.extend(dest.extract_if(|_| true));

            num_iterations += 1;
        });

        dbg!(source.len());
        dbg!(dest.len());

        dbg!(num_iterations);
    }

    #[bench]
    fn bench_extract_if_low_extraction_rate_multithreaded(b: &mut Bencher) {
        let info: Info = BitInserterStorageInfo {
            max_inserter_id: 5,
            max_max_hand_size: 12,
            max_index: 80_000,
            max_grid: 3,
            max_recipe: 10,
        }
        .into();

        let ins = EncodedUpdatingInserter {
            storage_id_in: FakeUnionStorage {
                index: 11_322,
                grid_or_static_flag: 1,
                recipe_idx_with_this_item: 2,
            },
            storage_id_out: FakeUnionStorage {
                index: 75_111,
                grid_or_static_flag: 1,
                recipe_idx_with_this_item: 8,
            },
            max_hand_size: 12,
            current_hand: 5,
            id: InserterId(3),
        };

        let mut sources = vec![FixedSizeCompressedVec::new(info); 12];
        for source in &mut sources {
            source.extend(std::iter::repeat(ins).take(NUM));
        }
        let mut dests = vec![FixedSizeCompressedVec::new(info); 12];

        let mut num_iterations = 0;
        b.iter(|| {
            sources
                .par_iter_mut()
                .zip(dests.par_iter_mut())
                .for_each(|(source, dest)| {
                    let extracted = source.extract_if(|_| rand::random::<u8>() < 20);

                    dest.extend(extracted);

                    source.extend(dest.extract_if(|_| true));
                });

            num_iterations += 1;
        });

        dbg!(sources[0].len());
        dbg!(dests[0].len());

        dbg!(num_iterations);
    }

    const BITCOUNT: usize = 80_000_000;
    #[bench]
    fn bench_extract_bitpacked(b: &mut Bencher) {
        let mut data: BitVec<usize, Lsb0> = BitVec::new();

        data.extend(std::iter::from_fn(|| Some(rand::random::<bool>())).take(BITCOUNT));

        b.iter(|| {
            data.chunks(8)
                .map(|slice| {
                    let data: u32 = slice.load();

                    data
                })
                .for_each(|value| {
                    black_box(value);
                });
        });

        const MBPS: usize = 1_000_000_000 * BITCOUNT / 44_000_000 / 8 / 1_000_000;
    }
}
