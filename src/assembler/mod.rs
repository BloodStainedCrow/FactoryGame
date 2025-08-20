use std::{array, collections::BTreeMap, i32, marker::PhantomData, simd::Simd, u8};

use crate::{
    data::{DataStore, ItemRecipeDir},
    frontend::world::{Position, tile::AssemblerID},
    inserter::HAND_SIZE,
    item::{ITEMCOUNTTYPE, IdxTrait, Indexable, Item, Recipe, WeakIdxTrait},
    power::{
        Watt,
        power_grid::{IndexUpdateInfo, MAX_POWER_MULT, PowerGridEntity, PowerGridIdentifier},
    },
};
use itertools::Itertools;
use std::cmp::max;
use std::fmt::Debug;

#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size::GetSize;

pub mod bucketed;
pub mod simd;

pub type Simdtype = Simd<u8, 32>;

pub type TIMERTYPE = u16;

type CurrentlyUsedMultiAssemblerStore<RecipeIdxType> = simd::MultiAssemblerStore<RecipeIdxType>;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
pub struct FullAssemblerStore<RecipeIdxType: WeakIdxTrait> {
    pub assemblers: BTreeMap<(usize, usize), Vec<CurrentlyUsedMultiAssemblerStore<RecipeIdxType>>>,

    phantom_data: PhantomData<RecipeIdxType>,
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

impl<RecipeIdxType: IdxTrait> FullAssemblerStore<RecipeIdxType> {
    #[must_use]
    pub fn new<ItemIdxType: IdxTrait>(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        let mut assemblers = BTreeMap::new();

        for (nums, recipes) in data_store.ing_out_num_to_recipe.iter() {
            assert!(
                assemblers
                    .insert(
                        *nums,
                        recipes
                            .iter()
                            .copied()
                            .map(|r| simd::MultiAssemblerStore::new(r, data_store))
                            .collect(),
                    )
                    .is_none()
            );
        }

        Self {
            assemblers,
            phantom_data: PhantomData,
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
        let (assemblers, updates): (BTreeMap<_, _>, Vec<_>) = self
            .assemblers
            .into_iter()
            .zip(other.assemblers.into_iter())
            .map(|((self_num, self_list), (other_num, other_list))| {
                assert_eq!(self_num, other_num);

                let (list, updates) = self_list
                    .into_iter()
                    .zip(other_list.into_iter())
                    .map(|(a, b)| a.join(b, new_grid_id, data_store))
                    .unzip();
                ((self_num, list), updates)
            })
            .unzip();

        let ret = Self {
            assemblers,
            phantom_data: PhantomData,
        };

        (ret, updates.into_iter().flatten())
    }

    pub fn get_info<ItemIdxType: IdxTrait>(
        &self,
        assembler_id: AssemblerID<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerOnclickInfo<ItemIdxType> {
        let recipe_id = assembler_id.recipe.id.into();
        let nums = (
            data_store.recipe_num_ing_lookup[recipe_id],
            data_store.recipe_num_out_lookup[recipe_id],
        );
        self.assemblers.get(&nums).unwrap()[data_store.recipe_to_ing_out_combo_idx[recipe_id]]
            .get_info(assembler_id.assembler_index, data_store)
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

pub trait MultiAssemblerStore<RecipeIdxType: WeakIdxTrait>:
    Debug + Clone + for<'de> serde::Deserialize<'de> + serde::Serialize + 'static
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
        recipe_ings: &[&[ITEMCOUNTTYPE]],
        recipe_outputs: &[&[ITEMCOUNTTYPE]],
        times: &[TIMERTYPE],
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (PowerUsageInfo, u32, u32)
    where
        RecipeIdxType: IdxTrait;

    fn get_all_mut(
        &mut self,
    ) -> (
        (Box<[&[ITEMCOUNTTYPE]]>, Box<[&mut [ITEMCOUNTTYPE]]>),
        Box<[&mut [ITEMCOUNTTYPE]]>,
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
        ings_max_insert: &[ITEMCOUNTTYPE],
        ings: &[ITEMCOUNTTYPE],
        out: &[ITEMCOUNTTYPE],
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
        Box<[ITEMCOUNTTYPE]>,
        Box<[ITEMCOUNTTYPE]>,
        Box<[ITEMCOUNTTYPE]>,
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
            &data.0, &data.1, &data.2, data.3, data.4, data.5, data.6, data.7, data.8, data.9,
            data.10, data.11, data_store,
        )
    }

    fn add_assembler<ItemIdxType: IdxTrait>(
        &mut self,
        ty: u8,
        modules: &[Option<u8>],
        position: Position,
        recipe_lookup: &[(usize, usize)],
        recipe_ings: &[&[ITEMCOUNTTYPE]],
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
            .map(|module| i16::from(data_store.module_info[module as usize].speed_mod))
            .sum();

        let prod = i16::from(bonus_productivity_of_machine)
            + modules
                .iter()
                .copied()
                .flatten()
                .map(|module| i16::from(data_store.module_info[module as usize].prod_mod))
                .sum::<i16>();

        let power_mod = modules
            .iter()
            .copied()
            .flatten()
            .map(|module| i16::from(data_store.module_info[module as usize].power_mod))
            .sum();

        let (ing_idx, out_idx) = recipe_lookup[self.get_recipe().into_usize()];

        let our_ings: &[ITEMCOUNTTYPE] = &recipe_ings[ing_idx];

        self.add_assembler_with_data(
            // TODO: Make the automatic insertion limit dependent on the speed of the machine and recipe
            &array::from_fn(|ing| {
                max(
                    HAND_SIZE,
                    our_ings[ing].saturating_mul(3).saturating_add(12),
                )
            }),
            &array::from_fn(|_| 0),
            &array::from_fn(|_| 0),
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
