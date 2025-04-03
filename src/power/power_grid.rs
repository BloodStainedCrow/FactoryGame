use std::{
    cmp::{max, min},
    iter::{self},
};

use rayon::iter::{IndexedParallelIterator, IntoParallelRefMutIterator, ParallelIterator};

use crate::{
    assembler::{AssemblerOnclickInfo, AssemblerRemovalInfo, FullAssemblerStore},
    data::{DataStore, LazyPowerMachineInfo},
    frontend::world::{tile::AssemblerID, Position},
    inserter::{Storage, StorageID},
    item::{IdxTrait, Item, Recipe, WeakIdxTrait, ITEMCOUNTTYPE},
    lab::MultiLabStore,
    power::Joule,
    research::{ResearchProgress, TechState},
    statistics::{
        production::ProductionInfo,
        recipe::{RecipeTickInfo, RecipeTickInfoParts, SingleRecipeTickInfo},
    },
};

use super::{grid_graph::GridGraph, Watt};

pub const MAX_POWER_MULT: u8 = 64;

pub const MAX_BURNER_RATE: Watt = Watt(1_800_000);

const MAX_ACCUMULATOR_CHARGE_RATE: Watt = Watt(300_000);
const MAX_ACCUMULATOR_DISCHARGE_RATE: Watt = Watt(300_000);

const MAX_ACCUMULATOR_CHARGE: Joule = Joule(5_000_000);

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct PowerGrid<RecipeIdxType: WeakIdxTrait> {
    pub stores: FullAssemblerStore<RecipeIdxType>,
    pub lab_stores: MultiLabStore,
    pub(super) grid_graph: GridGraph<RecipeIdxType>,
    steam_power_producers: SteamPowerProducerStore,
    num_solar_panels: u64,
    main_accumulator_count: u64,
    main_accumulator_charge: Joule,
    // unique_accumulators: Vec<UniqueAccumulator>,
    use_burnable_fuel_to_charge_accumulators: Option<BurnableFuelForAccumulators>,

    last_power_consumption: Watt,

    pub last_power_mult: u8,
    // power_history: Timeline<()>
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, serde::Deserialize, serde::Serialize)]
enum BurnableFuelForAccumulators {
    Yes,
    #[default]
    No,
}

impl From<BurnableFuelForAccumulators> for bool {
    fn from(val: BurnableFuelForAccumulators) -> Self {
        match val {
            BurnableFuelForAccumulators::Yes => true,
            BurnableFuelForAccumulators::No => false,
        }
    }
}

pub struct IndexUpdateInfo<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub old: (Item<ItemIdxType>, Storage<RecipeIdxType>),
    pub new: (Item<ItemIdxType>, Storage<RecipeIdxType>),
}

pub struct PowerPoleUpdateInfo {
    pub position: Position,
    pub new_grid_id: PowerGridIdentifier,
}

pub fn all_storages<'a, RecipeIdxType: IdxTrait>(
    grids: impl IntoIterator<Item = &'a mut PowerGrid<RecipeIdxType>>,
) -> impl IntoIterator<Item = (usize, impl IntoIterator<Item = &'a mut [u8]>)> {
    // NOTE: This has to be assembled in the same way the lookup is generated in DataStore
    //       Currently this means assemblers -> labs -> TODO
    // TODO: This is very fragile :/
    grids.into_iter().enumerate().map(|(grid_id, grid)| {
        (
            grid_id,
            grid.stores
                .assemblers_0_1
                .iter_mut()
                .flat_map(|store| iter::once(store.get_outputs_mut(0)))
                // TODO: Chain the other storages here
                .chain(
                    grid.lab_stores
                        .sciences
                        .iter_mut()
                        .map(std::vec::Vec::as_mut_slice),
                ),
        )
    })
}

impl<RecipeIdxType: IdxTrait> PowerGrid<RecipeIdxType> {
    #[must_use]
    pub fn new<ItemIdxType: IdxTrait>(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        Self {
            stores: FullAssemblerStore::new(data_store),
            lab_stores: MultiLabStore::new(&data_store.science_bottle_items),
            grid_graph: GridGraph::new(),
            steam_power_producers: SteamPowerProducerStore {
                all_producers: data_store
                    .lazy_power_machine_infos
                    .iter()
                    .map(|info| MultiLazyPowerProducer::new(info))
                    .collect(),
            },
            num_solar_panels: 100,
            main_accumulator_count: 0,
            main_accumulator_charge: Joule(0),
            use_burnable_fuel_to_charge_accumulators: None,

            last_power_consumption: Watt(0),

            last_power_mult: MAX_POWER_MULT,
        }
    }

    #[must_use]
    pub fn join<ItemIdxType: IdxTrait>(
        self,
        other: Self,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
        self_grid: PowerGridIdentifier,
        other_grid: PowerGridIdentifier,
        new_pole_pos: Position,
        new_pole_connections: impl IntoIterator<Item = Position>,
    ) -> (
        Self,
        impl IntoIterator<Item = IndexUpdateInfo<ItemIdxType, RecipeIdxType>>,
    ) {
        let mut update_vec = vec![];

        let (new_stores, updates) =
            self.stores
                .join(other.stores, data_store, self_grid, other_grid);
        update_vec.extend(updates);

        let (new_labs, updates) =
            self.lab_stores
                .join(other.lab_stores, self_grid, other_grid, data_store);
        update_vec.extend(updates);

        let new_grid_graph =
            self.grid_graph
                .join(other.grid_graph, new_pole_pos, new_pole_connections);

        let ret = Self {
            stores: new_stores,
            lab_stores: new_labs,
            grid_graph: new_grid_graph,
            // TODO:
            steam_power_producers: self.steam_power_producers,
            num_solar_panels: self.num_solar_panels + other.num_solar_panels,
            main_accumulator_count: self.main_accumulator_count + other.main_accumulator_count,
            main_accumulator_charge: if self.main_accumulator_count + other.main_accumulator_count
                > 0
            {
                (self.main_accumulator_charge * self.main_accumulator_count
                    + other.main_accumulator_charge * other.main_accumulator_count)
                    / (self.main_accumulator_count + other.main_accumulator_count)
            } else {
                Joule(0)
            },
            use_burnable_fuel_to_charge_accumulators: match (
                self.use_burnable_fuel_to_charge_accumulators,
                other.use_burnable_fuel_to_charge_accumulators,
            ) {
                (None, None) => None,
                (None, Some(v)) | (Some(v), None) => Some(v),
                (Some(s), Some(o)) => {
                    if s == o {
                        Some(s)
                    } else {
                        todo!("You specified using Fuel on one power network, and not using it on another, how do I want to handle this...")
                    }
                },
            },

            last_power_consumption: self.last_power_consumption + other.last_power_consumption,
            last_power_mult: {
                // TODO: Is this exploitable? Maybe? But even if it is, it is only exploitable in an annoying and relatively useless way I think
                if self.last_power_consumption >= other.last_power_consumption {
                    self.last_power_mult
                } else {
                    other.last_power_mult
                }
            },
        };

        (ret, update_vec)
    }

    pub fn get_assembler_info<ItemIdxType: IdxTrait>(
        &self,
        assembler_id: AssemblerID<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerOnclickInfo<ItemIdxType> {
        self.stores.get_info(assembler_id, data_store)
    }

    pub fn add_pole(
        &mut self,
        pole_pos: Position,
        pole_connections: impl IntoIterator<Item = Position>,
    ) {
        self.grid_graph.add_pole(pole_pos, pole_connections);
    }

    pub fn add_assembler<ItemIdxType: IdxTrait>(
        &mut self,
        grid_id: PowerGridIdentifier,
        recipe: Recipe<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerID<RecipeIdxType> {
        let new_idx = match (
            data_store.recipe_num_ing_lookup[recipe.id.into()],
            data_store.recipe_num_out_lookup[recipe.id.into()],
        ) {
            (0, 1) => self.stores.assemblers_0_1
                [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]]
            .add_assembler(),

            _ => unreachable!(),
        };

        AssemblerID {
            recipe,
            grid: grid_id,
            assembler_index: new_idx.try_into().expect("More than u16::MAX assemblers"),
        }
    }

    pub fn remove_assembler<ItemIdxType: IdxTrait>(
        &mut self,
        assembler_id: AssemblerID<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerRemovalInfo {
        match (
            data_store.recipe_num_ing_lookup[assembler_id.recipe.id.into()],
            data_store.recipe_num_out_lookup[assembler_id.recipe.id.into()],
        ) {
            (0, 1) => self.stores.assemblers_0_1
                [data_store.recipe_to_ing_out_combo_idx[assembler_id.recipe.id.into()]]
            .remove_assembler(assembler_id.assembler_index as usize),

            _ => unreachable!(),
        }
    }

    // TODO: Currently impossible because of Ing Generics
    // fn do_for_assembler<T, ItemIdxType: IdxTrait>(
    //     &mut self,
    //     recipe: Recipe<RecipeIdxType>,
    //     data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    //     f: impl Fn(&mut MultiAssemblerStore<RecipeIdxType>) -> T,
    // ) -> T {
    //     match (
    //         data_store.recipe_num_ing_lookup[recipe.id.into()],
    //         data_store.recipe_num_out_lookup[recipe.id.into()],
    //     ) {
    //         (0, 1) => f(&mut self.stores.assemblers_0_1
    //             [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]]),

    //         _ => unreachable!(),
    //     }
    // }

    // FIXME: This is a huge, high branching function.
    // Make it simpler and more readable, and reduce repetition
    fn extract_power<ItemIdxType: IdxTrait>(
        &mut self,
        goal_amount: Joule,
        solar_panel_production_amount: Watt,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> u8 {
        let solar_power = (solar_panel_production_amount * self.num_solar_panels).joules_per_tick();

        let max_charge_amount: Joule = max(
            MAX_ACCUMULATOR_CHARGE_RATE.joules_per_tick(),
            MAX_ACCUMULATOR_CHARGE - self.main_accumulator_charge,
        ) * self.main_accumulator_count;
        // + self
        //     .unique_accumulators
        //     .iter()
        //     .map(|acc| {
        //         max(
        //             MAX_ACCUMULATOR_CHARGE_RATE.joules_per_tick(),
        //             MAX_ACCUMULATOR_CHARGE - acc.charge,
        //         )
        //     })
        //     .sum();

        if goal_amount <= solar_power {
            // We have power to charge accumulators
            let remaining_solar = solar_power - goal_amount; // This cannot underflow

            if remaining_solar >= max_charge_amount {
                // We already have enough power, without using burnables.
                self.charge_by(max_charge_amount);
            } else if self
                .use_burnable_fuel_to_charge_accumulators
                .unwrap_or_default()
                .into()
            {
                let still_needed = max_charge_amount - remaining_solar;

                let actually_extracted = self
                    .steam_power_producers
                    .extract_from_burners(still_needed, data_store);

                assert!(actually_extracted <= still_needed);
                assert!(actually_extracted + remaining_solar <= max_charge_amount);

                self.charge_by(actually_extracted + remaining_solar);
            } else {
                // We only charge using solar
                self.charge_by(remaining_solar);
            }
            MAX_POWER_MULT
        } else {
            // Not enough solar

            if self
                .use_burnable_fuel_to_charge_accumulators
                .unwrap_or_default()
                .into()
            {
                let still_needed = goal_amount + max_charge_amount - solar_power;

                let actually_extracted = self
                    .steam_power_producers
                    .extract_from_burners(still_needed, data_store);

                assert!(actually_extracted <= still_needed);
                assert!(actually_extracted - goal_amount <= max_charge_amount);

                match actually_extracted.cmp(&goal_amount) {
                    std::cmp::Ordering::Less => {
                        // Not enough power!
                        let power_missing_to_discharge_from_accs = goal_amount - actually_extracted;

                        let actually_discharged: Joule =
                            self.extract_from_accumulators(power_missing_to_discharge_from_accs);

                        assert!(actually_discharged + actually_extracted <= goal_amount);

                        let power_mult = ((actually_discharged + actually_extracted)
                            * MAX_POWER_MULT.into())
                            / goal_amount;

                        assert!(power_mult < MAX_POWER_MULT.into());

                        return u8::try_from(power_mult).expect("We already asserted");
                    },
                    std::cmp::Ordering::Equal => return MAX_POWER_MULT,
                    std::cmp::Ordering::Greater => {
                        // Use remaining power for charging
                        let charge_amount = goal_amount - actually_extracted; // This will never underflow

                        self.charge_by(charge_amount);

                        return MAX_POWER_MULT;
                    },
                }
            }
            // Not enough solar and do not charge using burners
            let still_needed = goal_amount - solar_power;

            let actually_extracted = self
                .steam_power_producers
                .extract_from_burners(still_needed, data_store);

            assert!(actually_extracted <= still_needed);
            assert!(actually_extracted <= goal_amount);

            match actually_extracted.cmp(&still_needed) {
                std::cmp::Ordering::Less => {
                    // Not enough power!
                    let power_missing_to_discharge_from_accs = still_needed - actually_extracted;

                    let actually_discharged =
                        self.extract_from_accumulators(power_missing_to_discharge_from_accs);

                    assert!(actually_discharged + actually_extracted + solar_power <= goal_amount);

                    let power_mult = ((actually_discharged + actually_extracted + solar_power)
                        * MAX_POWER_MULT.into())
                        / goal_amount;

                    assert!(power_mult < MAX_POWER_MULT.into());

                    u8::try_from(power_mult).expect("We already asserted")
                },
                std::cmp::Ordering::Equal => MAX_POWER_MULT,
                std::cmp::Ordering::Greater => {
                    unreachable!(
                        "We extracted more power than needed from burners, while disallowing charging?!"
                    );
                },
            }
        }
    }

    fn charge_by(&mut self, amount: Joule) {
        assert!(
            (MAX_ACCUMULATOR_CHARGE_RATE * self.main_accumulator_count).joules_per_tick() >= amount
        );

        self.main_accumulator_charge = min(
            self.main_accumulator_charge + amount,
            MAX_ACCUMULATOR_CHARGE,
        );

        // This is an algorithm for (kindof) handling accumulators with different charge from the "main pack"
        // while amount > MegaJoule(0) {
        //     // FIXME: This is integer division, so we lose some power here.
        //     let amount_per_accumulator = amount
        //         / (self.main_accumulator_count
        //             + u32::try_from(self.unique_accumulators.len())
        //                 .expect("Maximum u32::MAX accumulators allowed"));

        //     assert!(amount_per_accumulator > MAX_ACCUMULATOR_CHARGE_RATE.joules_per_tick());

        //     let used_charge_per = max(
        //         MAX_ACCUMULATOR_CHARGE - self.main_accumulator_charge,
        //         amount_per_accumulator,
        //     );

        //     self.main_accumulator_charge = self.main_accumulator_charge + used_charge_per;

        //     amount = amount - used_charge_per * self.main_accumulator_count;

        //     // Distribute the rest
        //     let amount_per_accumulator = amount
        //         / u32::try_from(self.unique_accumulators.len())
        //             .expect("Maximum u32::MAX accumulators allowed");
        //     for acc in &mut self.unique_accumulators {
        //         let to_charge = max(amount_per_accumulator, MAX_ACCUMULATOR_CHARGE - acc.charge);

        //         acc.charge = acc.charge + to_charge;
        //         amount = amount - to_charge;
        //     }

        //     if self.main_accumulator_charge == MAX_ACCUMULATOR_CHARGE {
        //         let old_len = self.unique_accumulators.len();
        //         self.unique_accumulators
        //             .retain(|acc| acc.charge < MAX_ACCUMULATOR_CHARGE);

        //         // Move all accumulators which are now full into main_accumulator_charge
        //         self.main_accumulator_count +=
        //             u32::try_from(old_len - self.unique_accumulators.len())
        //                 .expect("Maximum u32::MAX accumulators allowed");
        //     }
        // }
    }

    // TODO: currently all accumulators have the same power level. This means, adding accumulators will immediatly charge them to some degree/ tranfer power from others
    //       is that something I want?
    fn extract_from_accumulators(&mut self, power_needed: Joule) -> Joule {
        // only extract at most MAX_ACCUMULATOR_DISCHARGE_RATE
        let to_extract = min(
            power_needed,
            (MAX_ACCUMULATOR_DISCHARGE_RATE * self.main_accumulator_count).joules_per_tick(),
        );
        assert!(
            (MAX_ACCUMULATOR_DISCHARGE_RATE * self.main_accumulator_count).joules_per_tick()
                >= to_extract
        );

        let old = self.main_accumulator_charge;
        self.main_accumulator_charge =
            Joule(self.main_accumulator_charge.0.saturating_sub(to_extract.0));

        min(old, to_extract)
    }

    pub fn update<ItemIdxType: IdxTrait>(
        &mut self,
        solar_panel_production_amount: Watt,
        tech_state: &TechState,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (ResearchProgress, RecipeTickInfo) {
        let (
            ((power_used_0_1, infos_0_1), (power_used_1_1, infos_1_1)),
            (lab_power_used, times_labs_used_science, tech_progress),
        ) = rayon::join(
            || {
                rayon::join(
                    || {
                        self.stores
                    .assemblers_0_1
                    .par_iter_mut()
                    .map(|s| {
                        s.update_branchless::<RecipeIdxType>(
                            self.last_power_mult,
                            &data_store.recipe_index_lookups,
                            &data_store.recipe_ings.ing0,
                            &data_store.recipe_outputs.out1,
                            &data_store.recipe_timers,
                        )
                    })
                    .map(|(power_used, times_ings_used, crafts_finished)| {
                        (power_used, SingleRecipeTickInfo {
                            full_crafts: times_ings_used as u64,
                            prod_crafts: crafts_finished.checked_sub(times_ings_used).expect("More ingredients used than crafts finished?!? Negative productivity?") as u64,
                        })
                    })
                    .fold_with((Joule(0), vec![]), |(acc_power, mut infos), (rhs_power, info)| {
                        infos.push(info);

                        (acc_power + rhs_power, infos)
                    }).reduce(|| (Joule(0), vec![]), |(acc_power, mut infos), (rhs_power, info)| {
                        infos.extend_from_slice(&info);

                        (acc_power + rhs_power, infos)
                    })
                    },
                    || {
                        self.stores
                        .assemblers_1_1
                        .par_iter_mut()
                        .map(|s| {
                            s.update_branchless::<RecipeIdxType>(
                                self.last_power_mult,
                                &data_store.recipe_index_lookups,
                                &data_store.recipe_ings.ing1,
                                &data_store.recipe_outputs.out1,
                                &data_store.recipe_timers,
                            )
                        })
                        .map(|(power_used, times_ings_used, crafts_finished)| {
                            (power_used, SingleRecipeTickInfo {
                                full_crafts: times_ings_used as u64,
                                prod_crafts: crafts_finished.checked_sub(times_ings_used).expect("More ingredients used than crafts finished?!? Negative productivity?") as u64,
                            })
                        })
                        .fold_with((Joule(0), vec![]), |(acc_power, mut infos), (rhs_power, info)| {
                            infos.push(info);

                            (acc_power + rhs_power, infos)
                        }).reduce(|| (Joule(0), vec![]), |(acc_power, mut infos), (rhs_power, info)| {
                            infos.extend_from_slice(&info);

                            (acc_power + rhs_power, infos)
                        })
                    },
                )
            },
            || {
                self.lab_stores
                    .update(self.last_power_mult, &tech_state.current_technology)
            },
        );

        let power_used = power_used_0_1 + power_used_1_1;

        self.last_power_consumption = power_used.watt_from_tick();

        let next_power_mult = self.extract_power(
            power_used + lab_power_used,
            solar_panel_production_amount,
            data_store,
        );

        // TODO:
        self.last_power_mult = next_power_mult;
        // self.last_power_mult = MAX_POWER_MULT;

        let parts = RecipeTickInfoParts {
            recipes_0_1: infos_0_1,
            recipes_1_1: infos_1_1,
        };

        (tech_progress, RecipeTickInfo::from_parts(parts, data_store))
    }
}

struct UniqueAccumulator {
    charge: Joule,
}

#[derive(Debug, Default, Clone, serde::Deserialize, serde::Serialize)]
pub struct SteamPowerProducerStore {
    all_producers: Box<[MultiLazyPowerProducer]>,
}

impl SteamPowerProducerStore {
    // TODO: Maybe find a better algorithm for this. If only like one engine is running we constantly recheck all of them
    fn extract_from_burners<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        power_needed: Joule,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Joule {
        if power_needed == Joule(0) {
            // No need to do any calculations for 0 power need
            return power_needed;
        }

        let mut to_extract = power_needed;
        // TODO: etc
        let num_machines: usize = self
            .all_producers
            .iter()
            .map(MultiLazyPowerProducer::count)
            .sum();

        if num_machines == 0 {
            // Without machines, there is no power
            return Joule(0);
        }

        // TODO: Float?
        let mut power_per_machine = min(
            to_extract / u64::try_from(num_machines).expect("More than u64::MAX machines"),
            MAX_BURNER_RATE.joules_per_tick(),
        );

        let mut already_extracted_per_machine = Joule(0);

        loop {
            let (successful_count, extracted) =
                self.extract_from_all(power_per_machine, data_store);
            to_extract = to_extract - extracted;

            if successful_count == 0 {
                // We are done. All burners are empty
                break power_needed - to_extract;
            }

            // Try another round, extracting more from the machines that worked.
            // TODO: Float?
            already_extracted_per_machine = already_extracted_per_machine + power_per_machine;
            power_per_machine = min(
                to_extract / u64::try_from(successful_count).expect("More than u64::MAX machines")
                    + already_extracted_per_machine,
                MAX_BURNER_RATE.joules_per_tick(),
            ) - already_extracted_per_machine;
            // power_per_machine now holds how much power we want/can still extract from the successfull machines

            // This happens either if we extracted all we need, or if we reached MAX_BURNER_RATE
            if power_per_machine == Joule(0) {
                // No more power to be extracted.
                // We are done
                break power_needed - to_extract;
            }
        }
    }

    fn extract_from_all<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        power_needed_per_machine: Joule,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (usize, Joule) {
        let (successful_count, power_cumulative) = self
            .all_producers
            .iter_mut()
            .zip(data_store.lazy_power_machine_infos.iter())
            .map(|(prod, info)| {
                prod.extract(
                    info.power_per_item,
                    min(info.max_power_per_tick, power_needed_per_machine),
                )
            })
            .fold((0, Joule(0)), |acc, v| (acc.0 + v.0, acc.1 + v.1));

        (successful_count, power_cumulative)
    }
}

#[derive(Debug, Default, Clone, serde::Deserialize, serde::Serialize)]
struct MultiLazyPowerProducer {
    // TODO: For now turbines can only have a single input and no outputs
    ingredient: Vec<ITEMCOUNTTYPE>,
    stored_power: Vec<Joule>,

    holes: Vec<usize>,
    len: usize,
}

impl MultiLazyPowerProducer {
    fn new<ItemIdxType: IdxTrait>(info: &LazyPowerMachineInfo<ItemIdxType>) -> Self {
        Self {
            ingredient: vec![],
            stored_power: vec![],
            holes: vec![],
            len: 0,
        }
    }

    fn join<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        self,
        other: Self,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        Self,
        impl IntoIterator<Item = IndexUpdateInfo<ItemIdxType, RecipeIdxType>>,
    ) {
        (todo!(), [])
    }

    fn count(&self) -> usize {
        assert_eq!(self.ingredient.len(), self.stored_power.len());
        self.ingredient.len()
    }

    /// the caller must ensure, that power_needed_per_machine does not exceed the maximin possible power generation for the machine type!
    fn extract(
        &mut self,
        power_per_fuel_item: Joule,
        power_needed_per_machine: Joule,
    ) -> (usize, Joule) {
        debug_assert!(
            power_needed_per_machine <= power_per_fuel_item,
            "When extracting more than a single item can provide, it require looping over the power producers multiple times!"
        );

        let mut successful_count = 0;
        let mut extracted = Joule(0);

        for (ingredient, stored_power) in
            self.ingredient.iter_mut().zip(self.stored_power.iter_mut())
        {
            if *stored_power > power_needed_per_machine {
                *stored_power = *stored_power - power_needed_per_machine;

                extracted = extracted + power_needed_per_machine;
                successful_count += 1;
                continue;
            }

            // Not enough stored power
            if *ingredient >= 1 {
                // Create some more
                *ingredient -= 1;
                *stored_power = *stored_power + power_per_fuel_item;

                extracted = extracted + power_needed_per_machine;
                successful_count += 1;
            } else {
                // No fuel to recharge
                extracted = extracted + *stored_power;

                *stored_power = Joule(0);
            }
        }

        (successful_count, extracted)
    }
}

pub type PowerGridIdentifier = u8;

//        The current problem is that writing power usage to a slice will stop any vectorizations I might want to do
//       - A possible solution is chucking each MultiStore in it's connected power grid. That would complicate the inserter logic even more, though
//       - Another option is to limit the game to only ONE power grid (everything must be connected to the ship or something)
//       - Or maybe just not worrying about it is possible if it turns out, that even non-vectorized code can reach the memory bandwidth limits we are constrained by

// I decided on putting the MultiStores into the PowerGrid

#[cfg(test)]
mod test {
    use proptest::{prop_assert_eq, prop_assume, proptest};

    use crate::data::get_raw_data_test;

    use super::*;

    proptest! {

        #[test]
        fn test_power_grid_solar_only(num_solar in 0..1_000u64) {
            const SOLAR_POWER: Watt = Watt(1_000);

            let data_store = get_raw_data_test().process().assume_simple();

            let mut power_grid = PowerGrid::<u8> {
                stores: FullAssemblerStore::default(),
                lab_stores: MultiLabStore::new::<u8>(&[]),
                grid_graph: GridGraph::new(),
                steam_power_producers: SteamPowerProducerStore::default(),
                num_solar_panels: num_solar,
                main_accumulator_count: 0,
                main_accumulator_charge: Joule(0),
                use_burnable_fuel_to_charge_accumulators: Some(BurnableFuelForAccumulators::No),
                last_power_consumption: Watt(0),

                last_power_mult: 64
            };

            prop_assert_eq!(power_grid.extract_power(SOLAR_POWER.joules_per_tick() * num_solar, SOLAR_POWER, &data_store), MAX_POWER_MULT);
        }

        #[test]
        fn test_power_grid_always_satisfies_0(num_solar in 0..1_000u64, solar_power in 0..10_000u64) {
            let solar_power: Watt = Watt(solar_power);

            let data_store = get_raw_data_test().process().assume_simple();

            let mut power_grid = PowerGrid::<u8>  {
                stores: FullAssemblerStore::default(),
                lab_stores: MultiLabStore::new::<u8>(&[]),
                grid_graph: GridGraph::new(),
                steam_power_producers: SteamPowerProducerStore::default(),
                num_solar_panels: num_solar,
                main_accumulator_count: 0,
                main_accumulator_charge: Joule(0),
                use_burnable_fuel_to_charge_accumulators: Some(BurnableFuelForAccumulators::No),
                last_power_consumption: Watt(0),

                last_power_mult: 64
            };

            prop_assert_eq!(power_grid.extract_power(Joule(0), solar_power, &data_store), MAX_POWER_MULT);
        }

        #[test]
        fn test_power_grid_always_off_at_night_only_solar(num_solar in 0..1_000u64, power_needed in 0..1_000u64) {
            prop_assume!(power_needed > 0);

            let needed = Joule(power_needed);

            let data_store = get_raw_data_test().process().assume_simple();

            let mut power_grid = PowerGrid::<u8>  {
                stores: FullAssemblerStore::default(),
                lab_stores: MultiLabStore::new::<u8>(&[]),
                grid_graph: GridGraph::new(),
                steam_power_producers: SteamPowerProducerStore::default(),
                num_solar_panels: num_solar,
                main_accumulator_count: 0,
                main_accumulator_charge: Joule(0),
                use_burnable_fuel_to_charge_accumulators: Some(BurnableFuelForAccumulators::No),
                last_power_consumption: Watt(0),

                last_power_mult: 64
            };

            prop_assert_eq!(power_grid.extract_power(needed, Watt(0), &data_store), 0);
        }
    }
}
