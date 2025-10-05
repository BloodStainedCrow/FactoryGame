use std::{array, marker::PhantomData, simd::Simd, u8};

use crate::{
    data::DataStore,
    frontend::world::{Position, tile::AssemblerID},
    item::{ITEMCOUNTTYPE, IdxTrait, Indexable, Item, Recipe, WeakIdxTrait},
    power::{
        Watt,
        power_grid::{IndexUpdateInfo, PowerGridIdentifier},
    },
};
use itertools::Itertools;
use std::cmp::max;

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size::GetSize;

pub mod bucketed;
pub mod simd;

pub type Simdtype = Simd<u8, 32>;

pub type TIMERTYPE = u16;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct FullAssemblerStore<
    RecipeIdxType: WeakIdxTrait,
    Store0_1: MultiAssemblerStore<RecipeIdxType, 0, 1>,
    Store1_1: MultiAssemblerStore<RecipeIdxType, 1, 1>,
    Store2_1: MultiAssemblerStore<RecipeIdxType, 2, 1>,
    Store2_2: MultiAssemblerStore<RecipeIdxType, 2, 2>,
    Store2_3: MultiAssemblerStore<RecipeIdxType, 2, 3>,
    Store3_1: MultiAssemblerStore<RecipeIdxType, 3, 1>,
    Store4_1: MultiAssemblerStore<RecipeIdxType, 4, 1>,
    Store5_1: MultiAssemblerStore<RecipeIdxType, 5, 1>,
    Store6_1: MultiAssemblerStore<RecipeIdxType, 6, 1>,
> {
    pub assemblers_0_1: Box<[Store0_1]>,
    pub assemblers_1_1: Box<[Store1_1]>,
    pub assemblers_2_1: Box<[Store2_1]>,
    pub assemblers_2_2: Box<[Store2_2]>,
    pub assemblers_2_3: Box<[Store2_3]>,
    pub assemblers_3_1: Box<[Store3_1]>,
    pub assemblers_4_1: Box<[Store4_1]>,
    pub assemblers_5_1: Box<[Store5_1]>,
    pub assemblers_6_1: Box<[Store6_1]>,

    recipe: PhantomData<RecipeIdxType>,
}

#[derive(Debug, Clone)]
pub struct AssemblerOnclickInfo<ItemIdxType: WeakIdxTrait> {
    pub inputs: Vec<(Item<ItemIdxType>, ITEMCOUNTTYPE)>,
    pub outputs: Vec<(Item<ItemIdxType>, ITEMCOUNTTYPE)>,
    pub timer_percentage: f32,
    pub prod_timer_percentage: f32,
    pub base_speed: f32,
    pub speed_mod: f32,
    pub prod_mod: f32,
    pub power_consumption_mod: f32,
    pub base_power_consumption: Watt,
}

impl<
    RecipeIdxType: IdxTrait,
    Store0_1: MultiAssemblerStore<RecipeIdxType, 0, 1>,
    Store1_1: MultiAssemblerStore<RecipeIdxType, 1, 1>,
    Store2_1: MultiAssemblerStore<RecipeIdxType, 2, 1>,
    Store2_2: MultiAssemblerStore<RecipeIdxType, 2, 2>,
    Store2_3: MultiAssemblerStore<RecipeIdxType, 2, 3>,
    Store3_1: MultiAssemblerStore<RecipeIdxType, 3, 1>,
    Store4_1: MultiAssemblerStore<RecipeIdxType, 4, 1>,
    Store5_1: MultiAssemblerStore<RecipeIdxType, 5, 1>,
    Store6_1: MultiAssemblerStore<RecipeIdxType, 6, 1>,
>
    FullAssemblerStore<
        RecipeIdxType,
        Store0_1,
        Store1_1,
        Store2_1,
        Store2_2,
        Store2_3,
        Store3_1,
        Store4_1,
        Store5_1,
        Store6_1,
    >
{
    #[must_use]
    pub fn new<ItemIdxType: IdxTrait>(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        let assemblers_0_1 = data_store
            .ing_out_num_to_recipe
            .get(&(0, 1))
            .unwrap()
            .iter()
            .map(|r| MultiAssemblerStore::new(*r, data_store))
            .collect();
        let assemblers_1_1 = data_store
            .ing_out_num_to_recipe
            .get(&(1, 1))
            .unwrap()
            .iter()
            .map(|r| MultiAssemblerStore::new(*r, data_store))
            .collect();
        let assemblers_2_1 = data_store
            .ing_out_num_to_recipe
            .get(&(2, 1))
            .unwrap()
            .iter()
            .map(|r| MultiAssemblerStore::new(*r, data_store))
            .collect();
        let assemblers_2_2 = data_store
            .ing_out_num_to_recipe
            .get(&(2, 2))
            .unwrap()
            .iter()
            .map(|r| MultiAssemblerStore::new(*r, data_store))
            .collect();
        let assemblers_2_3 = data_store
            .ing_out_num_to_recipe
            .get(&(2, 3))
            .unwrap()
            .iter()
            .map(|r| MultiAssemblerStore::new(*r, data_store))
            .collect();
        let assemblers_3_1 = data_store
            .ing_out_num_to_recipe
            .get(&(3, 1))
            .unwrap()
            .iter()
            .map(|r| MultiAssemblerStore::new(*r, data_store))
            .collect();
        let assemblers_4_1 = data_store
            .ing_out_num_to_recipe
            .get(&(4, 1))
            .unwrap()
            .iter()
            .map(|r| MultiAssemblerStore::new(*r, data_store))
            .collect();
        let assemblers_5_1 = data_store
            .ing_out_num_to_recipe
            .get(&(5, 1))
            .unwrap()
            .iter()
            .map(|r| MultiAssemblerStore::new(*r, data_store))
            .collect();
        let assemblers_6_1 = data_store
            .ing_out_num_to_recipe
            .get(&(6, 1))
            .unwrap()
            .iter()
            .map(|r| MultiAssemblerStore::new(*r, data_store))
            .collect();

        Self {
            assemblers_0_1,
            assemblers_1_1,
            assemblers_2_1,
            assemblers_2_2,
            assemblers_2_3,
            assemblers_3_1,
            assemblers_4_1,
            assemblers_5_1,
            assemblers_6_1,

            recipe: PhantomData,
        }
    }

    #[must_use]
    pub fn join<ItemIdxType: IdxTrait>(
        self,
        other: Self,
        new_grid_id: PowerGridIdentifier,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        Self,
        impl IntoIterator<Item = IndexUpdateInfo<ItemIdxType, RecipeIdxType>>,
    ) {
        // TODO: This just works with box::into_iter in edition 2024
        let (assemblers_0_1, assemblers_0_1_updates): (Vec<_>, Vec<_>) = self
            .assemblers_0_1
            .into_vec()
            .into_iter()
            .zip(other.assemblers_0_1.into_vec())
            .map(|(a, b)| a.join(b, new_grid_id, data_store))
            .unzip();

        let (assemblers_1_1, assemblers_1_1_updates): (Vec<_>, Vec<_>) = self
            .assemblers_1_1
            .into_vec()
            .into_iter()
            .zip(other.assemblers_1_1.into_vec())
            .map(|(a, b)| a.join(b, new_grid_id, data_store))
            .unzip();

        let (assemblers_2_1, assemblers_2_1_updates): (Vec<_>, Vec<_>) = self
            .assemblers_2_1
            .into_vec()
            .into_iter()
            .zip(other.assemblers_2_1.into_vec())
            .map(|(a, b)| a.join(b, new_grid_id, data_store))
            .unzip();
        let (assemblers_2_2, assemblers_2_2_updates): (Vec<_>, Vec<_>) = self
            .assemblers_2_2
            .into_vec()
            .into_iter()
            .zip(other.assemblers_2_2.into_vec())
            .map(|(a, b)| a.join(b, new_grid_id, data_store))
            .unzip();
        let (assemblers_2_3, assemblers_2_3_updates): (Vec<_>, Vec<_>) = self
            .assemblers_2_3
            .into_vec()
            .into_iter()
            .zip(other.assemblers_2_3.into_vec())
            .map(|(a, b)| a.join(b, new_grid_id, data_store))
            .unzip();

        let (assemblers_3_1, assemblers_3_1_updates): (Vec<_>, Vec<_>) = self
            .assemblers_3_1
            .into_vec()
            .into_iter()
            .zip(other.assemblers_3_1.into_vec())
            .map(|(a, b)| a.join(b, new_grid_id, data_store))
            .unzip();

        let (assemblers_4_1, assemblers_4_1_updates): (Vec<_>, Vec<_>) = self
            .assemblers_4_1
            .into_vec()
            .into_iter()
            .zip(other.assemblers_4_1.into_vec())
            .map(|(a, b)| a.join(b, new_grid_id, data_store))
            .unzip();

        let (assemblers_5_1, assemblers_5_1_updates): (Vec<_>, Vec<_>) = self
            .assemblers_5_1
            .into_vec()
            .into_iter()
            .zip(other.assemblers_5_1.into_vec())
            .map(|(a, b)| a.join(b, new_grid_id, data_store))
            .unzip();

        let (assemblers_6_1, assemblers_6_1_updates): (Vec<_>, Vec<_>) = self
            .assemblers_6_1
            .into_vec()
            .into_iter()
            .zip(other.assemblers_6_1.into_vec())
            .map(|(a, b)| a.join(b, new_grid_id, data_store))
            .unzip();

        let ret = Self {
            assemblers_0_1: assemblers_0_1.into_boxed_slice(),
            assemblers_1_1: assemblers_1_1.into_boxed_slice(),
            assemblers_2_1: assemblers_2_1.into_boxed_slice(),
            assemblers_2_2: assemblers_2_2.into_boxed_slice(),
            assemblers_2_3: assemblers_2_3.into_boxed_slice(),
            assemblers_3_1: assemblers_3_1.into_boxed_slice(),
            assemblers_4_1: assemblers_4_1.into_boxed_slice(),
            assemblers_5_1: assemblers_5_1.into_boxed_slice(),
            assemblers_6_1: assemblers_6_1.into_boxed_slice(),

            recipe: PhantomData,
        };

        (
            ret,
            assemblers_0_1_updates
                .into_iter()
                .flatten()
                .chain(assemblers_1_1_updates.into_iter().flatten())
                .chain(assemblers_2_1_updates.into_iter().flatten())
                .chain(assemblers_2_2_updates.into_iter().flatten())
                .chain(assemblers_2_3_updates.into_iter().flatten())
                .chain(assemblers_3_1_updates.into_iter().flatten())
                .chain(assemblers_4_1_updates.into_iter().flatten())
                .chain(assemblers_5_1_updates.into_iter().flatten())
                .chain(assemblers_6_1_updates.into_iter().flatten()),
        )
    }

    pub fn get_info<ItemIdxType: IdxTrait>(
        &self,
        assembler_id: AssemblerID<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerOnclickInfo<ItemIdxType> {
        let recipe_id = assembler_id.recipe.id.into();

        match (
            data_store.recipe_num_ing_lookup[recipe_id],
            data_store.recipe_num_out_lookup[recipe_id],
        ) {
            (0, 1) => {
                assert_eq!(
                    assembler_id.recipe,
                    self.assemblers_0_1[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                        .get_recipe()
                );

                self.assemblers_0_1[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                    .get_info(assembler_id.assembler_index, data_store)
            },
            (1, 1) => {
                assert_eq!(
                    assembler_id.recipe,
                    self.assemblers_1_1[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                        .get_recipe()
                );

                self.assemblers_1_1[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                    .get_info(assembler_id.assembler_index, data_store)
            },
            (2, 1) => {
                assert_eq!(
                    assembler_id.recipe,
                    self.assemblers_2_1[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                        .get_recipe()
                );

                self.assemblers_2_1[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                    .get_info(assembler_id.assembler_index, data_store)
            },

            (2, 2) => {
                assert_eq!(
                    assembler_id.recipe,
                    self.assemblers_2_2[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                        .get_recipe()
                );

                self.assemblers_2_2[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                    .get_info(assembler_id.assembler_index, data_store)
            },

            (2, 3) => {
                assert_eq!(
                    assembler_id.recipe,
                    self.assemblers_2_3[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                        .get_recipe()
                );

                self.assemblers_2_3[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                    .get_info(assembler_id.assembler_index, data_store)
            },

            (3, 1) => {
                assert_eq!(
                    assembler_id.recipe,
                    self.assemblers_3_1[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                        .get_recipe()
                );

                self.assemblers_3_1[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                    .get_info(assembler_id.assembler_index, data_store)
            },

            (4, 1) => {
                assert_eq!(
                    assembler_id.recipe,
                    self.assemblers_4_1[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                        .get_recipe()
                );

                self.assemblers_4_1[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                    .get_info(assembler_id.assembler_index, data_store)
            },

            (5, 1) => {
                assert_eq!(
                    assembler_id.recipe,
                    self.assemblers_5_1[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                        .get_recipe()
                );

                self.assemblers_5_1[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                    .get_info(assembler_id.assembler_index, data_store)
            },

            (6, 1) => {
                assert_eq!(
                    assembler_id.recipe,
                    self.assemblers_6_1[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                        .get_recipe()
                );

                self.assemblers_6_1[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
                    .get_info(assembler_id.assembler_index, data_store)
            },

            _ => unreachable!(),
        }
    }
}

// FIXME:
// fn get_slice_for_item<'a, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
//     item: Item<ITEMCOUNTTYPE>,
//     assemblers: &'a mut FullAssemblerStore<RecipeIdxType>,
//     data_store: &DataStore<ItemIdxType, RecipeIdxType>,
// ) -> SingleItemSlice<'a, 'a> {
//     let res: Vec<&mut Vec<u8>> = assemblers
//         .iter_mut()
//         .flat_map(|store| {
//             let idx = recipe_item_idx(recipe, item);
//             idx.map(|idx| &mut store.ings[idx])
//         })
//         .collect();

//     // lifetime!
//     // We can´t return this out of a function
//     &mut res
// }

// ALTERNATIVE
// This has the alternative of having the lookup for inserters ready
// struct AssemblerItemStores {
//     stores: [(
//         [Vec<ITEMCOUNTTYPE>; NUM_RECIPES_ITEM_INGS],
//         [Vec<ITEMCOUNTTYPE>; NUM_RECIPES_ITEM_OUTPUT],
//     ); NUM_ITEMS],
// }

// struct MultiAssemblerTest {
//     recipe: (),
//     timers: Vec<TIMERTYPE>,
// }

pub struct AssemblerRemovalInfo {
    pub ings: Vec<ITEMCOUNTTYPE>,
    pub outputs: Vec<ITEMCOUNTTYPE>,
}

pub struct ZipArray<T, const N: usize> {
    pub(crate) array: [T; N],
}

pub fn zip_array<T: Iterator, const N: usize>(array: [T; N]) -> ZipArray<T, N> {
    ZipArray { array }
}

impl<T: Iterator, const N: usize> Iterator for ZipArray<T, N> {
    type Item = [T::Item; N];

    fn next(&mut self) -> Option<Self::Item> {
        self.array.each_mut().try_map(std::iter::Iterator::next)
    }
}

pub enum PowerUsageInfo {
    ByType(Vec<Watt>),
    Combined(Watt),
}

impl From<PowerUsageInfo> for Watt {
    fn from(value: PowerUsageInfo) -> Self {
        match value {
            PowerUsageInfo::ByType(v) => v.into_iter().sum(),
            PowerUsageInfo::Combined(value) => value,
        }
    }
}

pub trait MultiAssemblerStore<
    RecipeIdxType: WeakIdxTrait,
    const NUM_INGS: usize,
    const NUM_OUTPUTS: usize,
>: Sized + 'static
{
    fn new<ItemIdxType: IdxTrait>(
        recipe: Recipe<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self;
    fn get_recipe(&self) -> Recipe<RecipeIdxType>;
    fn get_info<ItemIdxType: IdxTrait>(
        &self,
        index: u32,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerOnclickInfo<ItemIdxType>;
    fn join<ItemIdxType: IdxTrait>(
        self,
        other: Self,
        new_grid_id: PowerGridIdentifier,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        Self,
        impl IntoIterator<Item = IndexUpdateInfo<ItemIdxType, RecipeIdxType>>,
    );

    fn do_single_tick_update<ItemIdxType: IdxTrait>(
        &mut self,
        power_mult: u8,
        recipe_lookup: &[(usize, usize)],
        recipe_ings: &[[ITEMCOUNTTYPE; NUM_INGS]],
        recipe_outputs: &[[ITEMCOUNTTYPE; NUM_OUTPUTS]],
        times: &[TIMERTYPE],
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (PowerUsageInfo, u32, u32)
    where
        RecipeIdxType: IdxTrait;

    fn get_all_mut(
        &mut self,
    ) -> (
        (
            [&[ITEMCOUNTTYPE]; NUM_INGS],
            [&mut [ITEMCOUNTTYPE]; NUM_INGS],
        ),
        [&mut [ITEMCOUNTTYPE]; NUM_OUTPUTS],
    );

    fn modify_modifiers<ItemIdxType: IdxTrait>(
        &mut self,
        index: u32,
        speed: i16,
        prod: i16,
        power: i16,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    );

    fn add_assembler_with_data<ItemIdxType: IdxTrait>(
        &mut self,
        ings_max_insert: [ITEMCOUNTTYPE; NUM_INGS],
        ings: [ITEMCOUNTTYPE; NUM_INGS],
        out: [ITEMCOUNTTYPE; NUM_OUTPUTS],
        timer: TIMERTYPE,
        prod_timer: TIMERTYPE,
        power: Watt,
        power_consumption_modifier: i16,
        bonus_productiviy: i16,
        base_speed: u8,
        speed_mod: i16,
        ty: u8,
        position: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> u32;

    fn remove_assembler_data<ItemIdxType: IdxTrait>(
        &mut self,
        index: u32,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        [ITEMCOUNTTYPE; NUM_INGS],
        [ITEMCOUNTTYPE; NUM_INGS],
        [ITEMCOUNTTYPE; NUM_OUTPUTS],
        TIMERTYPE,
        TIMERTYPE,
        Watt,
        i16,
        i16,
        u8,
        i16,
        u8,
        Position,
    );

    fn remove_assembler_data_non_generic<ItemIdxType: IdxTrait>(
        &mut self,
        index: u32,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        Vec<ITEMCOUNTTYPE>,
        Vec<ITEMCOUNTTYPE>,
        Vec<ITEMCOUNTTYPE>,
        TIMERTYPE,
        TIMERTYPE,
        Watt,
        i16,
        i16,
        u8,
        i16,
        u8,
        Position,
    ) {
        let data = self.remove_assembler_data(index, data_store);

        (
            data.0.into(),
            data.1.into(),
            data.2.into(),
            data.3.into(),
            data.4.into(),
            data.5.into(),
            data.6.into(),
            data.7.into(),
            data.8.into(),
            data.9.into(),
            data.10.into(),
            data.11.into(),
        )
    }

    fn move_assembler<ItemIdxType: IdxTrait>(
        &mut self,
        index: u32,
        dest: &mut Self,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> u32 {
        let data = self.remove_assembler_data(index, data_store);

        dest.add_assembler_with_data(
            data.0, data.1, data.2, data.3, data.4, data.5, data.6, data.7, data.8, data.9,
            data.10, data.11, data_store,
        )
    }

    fn add_assembler<ItemIdxType: IdxTrait>(
        &mut self,
        ty: u8,
        modules: &[Option<usize>],
        position: Position,
        recipe_lookup: &[(usize, usize)],
        recipe_ings: &[[ITEMCOUNTTYPE; NUM_INGS]],
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> u32 {
        assert_eq!(
            modules.len(),
            data_store.assembler_info[usize::from(ty)].num_module_slots as usize
        );

        let base_speed = data_store.assembler_info[usize::from(ty)].base_speed;
        let bonus_productivity_of_machine = data_store.assembler_info[usize::from(ty)].base_prod;
        let base_power_consumption: Watt =
            data_store.assembler_info[usize::from(ty)].base_power_consumption;

        let speed_mod = modules
            .iter()
            .copied()
            .flatten()
            .map(|module| i16::from(data_store.module_info[module].speed_mod))
            .sum();

        let prod = i16::from(bonus_productivity_of_machine)
            + modules
                .iter()
                .copied()
                .flatten()
                .map(|module| i16::from(data_store.module_info[module].prod_mod))
                .sum::<i16>();

        let power_mod = modules
            .iter()
            .copied()
            .flatten()
            .map(|module| i16::from(data_store.module_info[module].power_mod))
            .sum();

        let (ing_idx, out_idx) = recipe_lookup[self.get_recipe().into_usize()];

        let our_ings: &[ITEMCOUNTTYPE; NUM_INGS] = &recipe_ings[ing_idx];

        self.add_assembler_with_data(
            // TODO: Make the automatic insertion limit dependent on the speed of the machine and recipe
            array::from_fn(|ing| max(12, our_ings[ing].saturating_mul(3).saturating_add(12))),
            array::from_fn(|_| 0),
            array::from_fn(|_| 0),
            0,
            0,
            base_power_consumption,
            power_mod,
            prod,
            base_speed,
            speed_mod,
            ty,
            position,
            data_store,
        )
    }

    /// The caller must make sure, that this index is not used in any other machine, since it will either crash/work on a nonexistant Assembler or be reused for another machine!
    fn remove_assembler<ItemIdxType: IdxTrait>(
        &mut self,
        index: u32,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerRemovalInfo {
        let (ings, outputs, ..) = self.remove_assembler_data(index, data_store);

        let ret = AssemblerRemovalInfo {
            ings: ings.into_iter().collect(),
            outputs: outputs.into_iter().collect(),
        };

        ret
    }
}

pub mod arrays {
    use std::{convert::TryInto, marker::PhantomData};

    use serde::{
        Deserialize, Deserializer, Serialize, Serializer,
        de::{SeqAccess, Visitor},
        ser::SerializeTuple,
    };
    pub fn serialize<S: Serializer, T: Serialize, const N: usize>(
        data: &[T; N],
        ser: S,
    ) -> Result<S::Ok, S::Error> {
        let mut s = ser.serialize_tuple(N)?;
        for item in data {
            s.serialize_element(item)?;
        }
        s.end()
    }

    struct ArrayVisitor<T, const N: usize>(PhantomData<T>);

    impl<'de, T, const N: usize> Visitor<'de> for ArrayVisitor<T, N>
    where
        T: Deserialize<'de>,
    {
        type Value = [T; N];

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str(&format!("an array of length {N}"))
        }

        #[inline]
        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            // can be optimized using MaybeUninit
            let mut data = Vec::with_capacity(N);
            for _ in 0..N {
                match (seq.next_element())? {
                    Some(val) => data.push(val),
                    None => return Err(serde::de::Error::invalid_length(N, &self)),
                }
            }
            data.try_into().map_or_else(|_| unreachable!(), Ok)
        }
    }
    pub fn deserialize<'de, D, T, const N: usize>(deserializer: D) -> Result<[T; N], D::Error>
    where
        D: Deserializer<'de>,
        T: Deserialize<'de>,
    {
        deserializer.deserialize_tuple(N, ArrayVisitor::<T, N>(PhantomData))
    }
}
