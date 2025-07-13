use std::{cmp::min, collections::HashMap, mem};

use itertools::Itertools;
use rayon::iter::{IndexedParallelIterator, IntoParallelRefMutIterator, ParallelIterator};

use crate::{
    assembler::{AssemblerOnclickInfo, AssemblerRemovalInfo, FullAssemblerStore},
    data::{DataStore, LazyPowerMachineInfo},
    frontend::world::{tile::AssemblerID, Position},
    item::{usize_from, IdxTrait, Indexable, Item, Recipe, WeakIdxTrait, ITEMCOUNTTYPE},
    lab::MultiLabStore,
    network_graph::{Network, WeakIndex},
    power::Joule,
    research::{ResearchProgress, TechState},
    statistics::{
        recipe::{RecipeTickInfo, RecipeTickInfoParts, SingleRecipeTickInfo},
        Timeline,
    },
};

use super::Watt;

pub const MAX_POWER_MULT: u8 = 64;
pub const MIN_BEACON_POWER_MULT: u8 = MAX_POWER_MULT / 2;

pub const MAX_BURNER_RATE: Watt = Watt(1_800_000);

const MAX_ACCUMULATOR_CHARGE_RATE: Watt = Watt(300_000);
const MAX_ACCUMULATOR_DISCHARGE_RATE: Watt = Watt(300_000);

const MAX_ACCUMULATOR_CHARGE: Joule = Joule(5_000_000);

pub enum PowerGridEntityFIXME {
    Assembler {
        ty: u8,
        recipe: Recipe<u8>,
        index: u32,
    },
    Lab {
        ty: u8,
        index: u32,
    },
    LazyPowerProducer {
        item: Item<u8>,
        index: u32,
    },
    SolarPanel {
        ty: u8,
    },
    Accumulator {
        ty: u8,
    },
    Beacon {
        ty: u8,
        speed: i16,
        prod: i16,
        power: i16,
        pos: Position,
    },
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub enum PowerGridEntity<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    Assembler {
        ty: u8,
        recipe: Recipe<RecipeIdxType>,
        index: u32,
    },
    Lab {
        ty: u8,
        index: u32,
    },
    LazyPowerProducer {
        item: Item<ItemIdxType>,
        index: u32,
    },
    SolarPanel {
        ty: u8,
    },
    Accumulator {
        ty: u8,
    },
    Beacon {
        ty: u8,
        modules: Box<[Option<usize>]>,
        affected_entities: Vec<BeaconAffectedEntity<RecipeIdxType>>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
pub enum BeaconAffectedEntity<RecipeIdxType: WeakIdxTrait> {
    Assembler {
        id: AssemblerID<RecipeIdxType>,
    },
    Lab {
        grid: PowerGridIdentifier,
        index: u32,
    },
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct PowerGrid<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub stores: FullAssemblerStore<RecipeIdxType>,
    pub lab_stores: MultiLabStore,
    pub(super) grid_graph:
        Network<Position, (), (Position, PowerGridEntity<ItemIdxType, RecipeIdxType>)>,
    steam_power_producers: SteamPowerProducerStore,

    // TODO: Currently there can only be a single type of solar panel and accumulator
    pub num_solar_panels_of_type: Box<[u64]>,
    pub main_accumulator_count: Box<[u64]>,
    pub main_accumulator_charge: Box<[Joule]>,
    // unique_accumulators: Vec<UniqueAccumulator>,
    use_burnable_fuel_to_charge_accumulators: Option<BurnableFuelForAccumulators>,

    pub last_power_consumption: Watt,
    pub last_produced_power: Watt,

    pub last_ticks_max_power_production: Watt,

    max_lazy_power: Watt,

    pub last_power_mult: u8,
    pub power_mult_history: Timeline<u32>,
    // FIXME: Not actually storing where the power consumption/production originates is not very useful :/
    // pub power_consumption_history: Timeline<Watt>,
    // pub power_production_history: Timeline<Watt>,
    pub is_placeholder: bool,

    pub num_assemblers_of_type: Box<[usize]>,
    pub num_labs_of_type: Box<[usize]>,
    pub num_beacons_of_type: Box<[usize]>,

    pub beacon_affected_entities: HashMap<BeaconAffectedEntity<RecipeIdxType>, (i16, i16, i16)>,
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

#[derive(Debug)]
pub struct IndexUpdateInfo<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub position: Position,
    pub old_pg_entity: PowerGridEntity<ItemIdxType, RecipeIdxType>,
    pub new_pg_entity: PowerGridEntity<ItemIdxType, RecipeIdxType>,
    pub new_grid: PowerGridIdentifier,
    // IMPORTANTLY the WeakIdx always stays the same, since it is just a measure of how many machine are connected to a single pole,
    // and we only move poles over to new networks so that stays
}

pub struct PowerPoleUpdateInfo {
    pub position: Position,
    pub new_grid_id: PowerGridIdentifier,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> PowerGrid<ItemIdxType, RecipeIdxType> {
    #[must_use]
    pub fn new(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
        first_pole_pos: Position,
    ) -> Self {
        let network = Network::new((), first_pole_pos);

        Self {
            stores: FullAssemblerStore::new(data_store),
            lab_stores: MultiLabStore::new(&data_store.science_bottle_items),
            grid_graph: network,
            steam_power_producers: SteamPowerProducerStore {
                all_producers: data_store
                    .lazy_power_machine_infos
                    .iter()
                    .map(|info| MultiLazyPowerProducer::new(info))
                    .collect(),
            },
            num_solar_panels_of_type: vec![0; data_store.solar_panel_info.len()].into_boxed_slice(),
            main_accumulator_count: vec![0; data_store.accumulator_info.len()].into_boxed_slice(),
            main_accumulator_charge: vec![Joule(0); data_store.accumulator_info.len()]
                .into_boxed_slice(),
            use_burnable_fuel_to_charge_accumulators: None,

            last_power_consumption: Watt(0),
            last_produced_power: Watt(0),
            max_lazy_power: Watt(0),

            last_ticks_max_power_production: Watt(0),

            last_power_mult: MAX_POWER_MULT,

            power_mult_history: Timeline::new(false, data_store),

            is_placeholder: false,

            num_assemblers_of_type: vec![0; data_store.assembler_info.len()].into_boxed_slice(),
            num_labs_of_type: vec![0; data_store.lab_info.len()].into_boxed_slice(),
            num_beacons_of_type: vec![0; data_store.beacon_info.len()].into_boxed_slice(),

            beacon_affected_entities: HashMap::default(),
        }
    }

    #[must_use]
    pub fn new_placeholder(data_store: &DataStore<ItemIdxType, RecipeIdxType>) -> Self {
        let network = Network::new((), Position { x: 0, y: 0 });

        Self {
            stores: FullAssemblerStore::new(data_store),
            lab_stores: MultiLabStore::new(&data_store.science_bottle_items),
            grid_graph: network,
            steam_power_producers: SteamPowerProducerStore {
                all_producers: data_store
                    .lazy_power_machine_infos
                    .iter()
                    .map(|info| MultiLazyPowerProducer::new(info))
                    .collect(),
            },
            num_solar_panels_of_type: vec![0; data_store.solar_panel_info.len()].into_boxed_slice(),
            main_accumulator_count: vec![0; data_store.accumulator_info.len()].into_boxed_slice(),
            main_accumulator_charge: vec![Joule(0); data_store.accumulator_info.len()]
                .into_boxed_slice(),
            use_burnable_fuel_to_charge_accumulators: None,

            last_power_consumption: Watt(0),
            last_produced_power: Watt(0),
            max_lazy_power: Watt(0),

            last_ticks_max_power_production: Watt(0),

            last_power_mult: MAX_POWER_MULT,

            power_mult_history: Timeline::new(false, data_store),

            is_placeholder: true,

            num_assemblers_of_type: vec![].into_boxed_slice(),
            num_labs_of_type: vec![].into_boxed_slice(),
            num_beacons_of_type: vec![].into_boxed_slice(),

            beacon_affected_entities: HashMap::default(),
        }
    }

    fn new_from_graph(
        graph: Network<Position, (), (Position, PowerGridEntity<ItemIdxType, RecipeIdxType>)>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            grid_graph: graph,
            // TODO: If adding a power pole has addition side effect besides changing the graph, this is not correct
            ..Self::new(data_store, Position { x: 0, y: 0 })
        }
    }

    #[must_use]
    #[profiling::function]
    pub fn join(
        mut self,
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
        assert!(!self.is_placeholder);
        assert!(!other.is_placeholder);

        let (new_stores, assembler_updates) = self.stores.join(other.stores, self_grid, data_store);

        let (new_labs, lab_updates) = self
            .lab_stores
            .join(other.lab_stores, self_grid, data_store);

        let mut new_grid_graph = self.grid_graph;
        let mut new_pole_connections = new_pole_connections.into_iter();
        new_grid_graph.add_node_merging(
            (),
            new_pole_pos,
            (new_pole_connections.next().unwrap(), new_pole_connections),
            other.grid_graph,
        );

        self.num_assemblers_of_type
            .iter_mut()
            .zip(other.num_assemblers_of_type)
            .for_each(|(s, o)| {
                *s += o;
            });

        self.num_labs_of_type
            .iter_mut()
            .zip(other.num_labs_of_type)
            .for_each(|(s, o)| {
                *s += o;
            });

        self.num_beacons_of_type
            .iter_mut()
            .zip(other.num_beacons_of_type)
            .for_each(|(s, o)| {
                *s += o;
            });

        for affected in other.beacon_affected_entities {
            let entry = self.beacon_affected_entities.entry(affected.0);

            match entry {
                std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                    let old = occupied_entry.get_mut();
                    old.0 += affected.1 .0;
                    old.1 += affected.1 .1;
                    old.2 += affected.1 .2;
                },
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(affected.1);
                },
            }
        }

        for (((self_charge, other_charge), self_count), other_count) in self
            .main_accumulator_charge
            .iter_mut()
            .zip(other.main_accumulator_charge)
            .zip(self.main_accumulator_count.iter().copied())
            .zip(other.main_accumulator_count.iter().copied())
        {
            if self_count + other_count == 0 {
                assert_eq!(*self_charge, Joule(0));
                assert_eq!(other_charge, Joule(0));
            } else {
                *self_charge = (*self_charge * self_count + other_charge * other_count)
                    / (self_count / other_count);
            }
        }

        for (self_count, other_count) in self
            .num_solar_panels_of_type
            .iter_mut()
            .zip(other.num_solar_panels_of_type)
        {
            *self_count += other_count;
        }

        for (self_count, other_count) in self
            .main_accumulator_count
            .iter_mut()
            .zip(other.main_accumulator_count)
        {
            *self_count += other_count;
        }

        let ret = Self {
            stores: new_stores,
            lab_stores: new_labs,
            grid_graph: new_grid_graph,
            // TODO:
            steam_power_producers: self.steam_power_producers,
            num_solar_panels_of_type: self.num_solar_panels_of_type,
            main_accumulator_count: self.main_accumulator_count,
            main_accumulator_charge: self.main_accumulator_charge,
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
                        todo!("You specified using Fuel to charge accumulators on one power network, and not using it on another, how do I want to handle this...")
                    }
                },
            },

            last_power_consumption: self.last_power_consumption + other.last_power_consumption,
            last_produced_power: self.last_produced_power + other.last_produced_power,
            last_ticks_max_power_production: self.last_ticks_max_power_production
                + other.last_ticks_max_power_production,
            max_lazy_power: self.max_lazy_power + other.max_lazy_power,
            last_power_mult: {
                // TODO: Is this exploitable? Maybe? But even if it is, it is only exploitable in an annoying and relatively useless way I think
                if self.last_power_consumption >= other.last_power_consumption {
                    self.last_power_mult
                } else {
                    other.last_power_mult
                }
            },
            power_mult_history: {
                if self.last_power_consumption >= other.last_power_consumption {
                    self.power_mult_history
                } else {
                    other.power_mult_history
                }
            },
            is_placeholder: false,

            num_assemblers_of_type: self.num_assemblers_of_type,
            num_labs_of_type: self.num_labs_of_type,
            num_beacons_of_type: self.num_beacons_of_type,

            beacon_affected_entities: self.beacon_affected_entities,
        };

        (ret, assembler_updates.into_iter().chain(lab_updates))
    }

    pub fn get_assembler_info(
        &self,
        assembler_id: AssemblerID<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerOnclickInfo<ItemIdxType> {
        self.stores.get_info(assembler_id, data_store)
    }

    pub fn add_solar_panel(
        &mut self,
        panel_position: Position,
        ty: u8,
        pole_connection: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> WeakIndex {
        assert!(!self.is_placeholder);

        self.num_solar_panels_of_type[usize::from(ty)] += 1;

        self.grid_graph.add_weak_element(
            pole_connection,
            (panel_position, PowerGridEntity::SolarPanel { ty }),
        )
    }

    pub fn remove_solar_panel(
        &mut self,
        pole_connection: Position,
        weak_idx: WeakIndex,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        assert!(!self.is_placeholder);

        let (_, PowerGridEntity::SolarPanel { ty }) = self
            .grid_graph
            .remove_weak_element(pole_connection, weak_idx)
        else {
            unreachable!()
        };

        self.num_solar_panels_of_type[usize::from(ty)] -= 1;
    }

    pub fn add_lab(
        &mut self,
        lab_position: Position,
        ty: u8,
        modules: &[Option<usize>],
        pole_connection: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (WeakIndex, u32) {
        assert!(!self.is_placeholder);

        self.num_labs_of_type[usize::from(ty)] += 1;

        let index = self
            .lab_stores
            .add_lab(ty, lab_position, modules, data_store);

        let weak_idx = self.grid_graph.add_weak_element(
            pole_connection,
            (lab_position, PowerGridEntity::Lab { ty, index }),
        );

        (weak_idx, index)
    }

    pub fn remove_lab(
        &mut self,
        pole_connection: Position,
        weak_idx: WeakIndex,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Vec<(Item<ItemIdxType>, ITEMCOUNTTYPE)> {
        assert!(!self.is_placeholder);

        let (_lab_pos, PowerGridEntity::Lab { ty, index }) = self
            .grid_graph
            .remove_weak_element(pole_connection, weak_idx)
        else {
            unreachable!()
        };

        self.num_labs_of_type[usize::from(ty)] -= 1;

        let residual_items = self.lab_stores.remove_lab(index);

        data_store
            .science_bottle_items
            .iter()
            .copied()
            .zip(residual_items)
            .collect()
    }

    pub fn add_pole(
        &mut self,
        new_pole_pos: Position,
        pole_connections: impl IntoIterator<Item = Position>,
    ) {
        assert!(!self.is_placeholder);

        let mut pole_connections = pole_connections.into_iter();
        self.grid_graph.add_node(
            (),
            new_pole_pos,
            (pole_connections.next().unwrap(), pole_connections),
        );
    }

    #[profiling::function]
    pub fn remove_pole<'a>(
        &mut self,
        pole_pos: Position,
        data_store: &'a DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        impl IntoIterator<
            Item = (
                Self,
                impl IntoIterator<
                    Item = (
                        Position,
                        PowerGridEntity<ItemIdxType, RecipeIdxType>,
                        PowerGridEntity<ItemIdxType, RecipeIdxType>,
                    ),
                >,
            ),
        >,
        bool, // This tells the storage to delete us
        impl IntoIterator<Item = Position>,
        impl IntoIterator<Item = (BeaconAffectedEntity<RecipeIdxType>, (i16, i16, i16))>
            + use<'a, ItemIdxType, RecipeIdxType>,
    ) {
        let ((), no_longer_connected_entities, new_electric_networks) =
            self.grid_graph.remove_node(pole_pos);

        let (no_longer_connected_entities_pos, no_longer_connected_entities): (Vec<_>, Vec<_>) =
            no_longer_connected_entities.into_iter().unzip();

        // This is needed to make sure both paths have the same type (since closures have different types even if identical)
        let pos_closure = |v: &(Position, PowerGridEntity<ItemIdxType, RecipeIdxType>)| v.0;
        let beacon_mod_closure = |v: PowerGridEntity<ItemIdxType, RecipeIdxType>| match &v {
            PowerGridEntity::Beacon {
                ty,
                modules,
                affected_entities,
            } => {
                let effect: (i16, i16, i16) = modules
                    .iter()
                    .flatten()
                    .map(|module_ty| {
                        (
                            data_store.module_info[*module_ty].speed_mod.into(),
                            data_store.module_info[*module_ty].prod_mod.into(),
                            data_store.module_info[*module_ty].power_mod.into(),
                        )
                    })
                    .reduce(|acc, v| (acc.0 + v.0, acc.1 + v.1, acc.2 + v.2))
                    .unwrap_or((0, 0, 0));

                let effect = (
                    effect.0 * data_store.beacon_info[usize::from(*ty)].effectiveness.0 as i16
                        / data_store.beacon_info[usize::from(*ty)].effectiveness.1 as i16,
                    effect.1 * data_store.beacon_info[usize::from(*ty)].effectiveness.0 as i16
                        / data_store.beacon_info[usize::from(*ty)].effectiveness.1 as i16,
                    effect.2 * data_store.beacon_info[usize::from(*ty)].effectiveness.0 as i16
                        / data_store.beacon_info[usize::from(*ty)].effectiveness.1 as i16,
                );

                if effect == (0, 0, 0) {
                    // No effect, do not emit any updates
                    vec![]
                } else {
                    affected_entities
                        .into_iter()
                        .map(|e| (*e, (-effect.0, -effect.1, -effect.2)))
                        .collect()
                }
            },
            _ => vec![],
        };

        if new_electric_networks.is_none() {
            // We no longer exist

            // No new networks
            let new_networks: Vec<(_, Vec<_>)> = vec![];

            mem::drop(new_electric_networks);

            assert!(self.grid_graph.keys().into_iter().count() == 0);
            return (
                new_networks,
                true,
                no_longer_connected_entities_pos,
                no_longer_connected_entities
                    .into_iter()
                    .flat_map(beacon_mod_closure),
            );
        }

        let new_electric_networks: Vec<_> = new_electric_networks.into_iter().flatten().collect();

        let new_electric_networks: Vec<_> = new_electric_networks
            .into_iter()
            .map(|(network, pole_position_in_new_network)| {
                let mut new_network: PowerGrid<ItemIdxType, RecipeIdxType> =
                    Self::new_from_graph(network, data_store);

                let storage_updates = self
                    .move_connected_entities(&mut new_network, data_store)
                    .into_iter()
                    .collect();

                (new_network, storage_updates)
            })
            .collect();

        for no_longer_connected_entity in no_longer_connected_entities.iter() {
            self.remove_connected_entity(&no_longer_connected_entity, data_store);
        }

        // TODO: Do I want to return the no_longer_connected_entities positions?

        (
            new_electric_networks,
            false,
            no_longer_connected_entities_pos,
            no_longer_connected_entities
                .into_iter()
                .flat_map(beacon_mod_closure),
        )
    }

    fn remove_connected_entity(
        &mut self,
        connected_entity: &PowerGridEntity<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        match connected_entity {
            PowerGridEntity::Assembler { ty, recipe, index } => {
                self.remove_assembler_raw(
                    AssemblerID {
                        recipe: *recipe,
                        grid: 0, // Does not matter
                        assembler_index: *index,
                    },
                    data_store,
                );
            },
            PowerGridEntity::Lab { index, ty: _ty } => {
                let residual_items = self.lab_stores.remove_lab(*index);
            },
            PowerGridEntity::LazyPowerProducer { item, index } => {
                todo!("Remove LazyPowerProducer (Steam Engine)")
            },
            PowerGridEntity::SolarPanel { ty } => {
                self.num_solar_panels_of_type[usize::from(*ty)] -= 1;
            },
            PowerGridEntity::Accumulator { ty } => {
                self.main_accumulator_count[usize::from(*ty)] -= 1;
            },
            PowerGridEntity::Beacon {
                ty,
                modules,
                affected_entities,
            } => {
                let raw_effect: (i16, i16, i16) = modules
                    .iter()
                    .flatten()
                    .map(|module_ty| {
                        (
                            data_store.module_info[*module_ty].speed_mod.into(),
                            data_store.module_info[*module_ty].prod_mod.into(),
                            data_store.module_info[*module_ty].power_mod.into(),
                        )
                    })
                    .reduce(|acc, v| (acc.0 + v.0, acc.1 + v.1, acc.2 + v.2))
                    .unwrap_or((0, 0, 0));

                let raw_effect = (
                    raw_effect.0 * data_store.beacon_info[usize::from(*ty)].effectiveness.0 as i16
                        / data_store.beacon_info[usize::from(*ty)].effectiveness.1 as i16,
                    raw_effect.1 * data_store.beacon_info[usize::from(*ty)].effectiveness.0 as i16
                        / data_store.beacon_info[usize::from(*ty)].effectiveness.1 as i16,
                    raw_effect.2 * data_store.beacon_info[usize::from(*ty)].effectiveness.0 as i16
                        / data_store.beacon_info[usize::from(*ty)].effectiveness.1 as i16,
                );

                let effect = if self.last_power_mult >= MIN_BEACON_POWER_MULT {
                    raw_effect
                } else {
                    (0, 0, raw_effect.2)
                };

                if effect.0 > 0 || effect.1 > 0 || effect.2 > 0 {
                    for effected_entity in affected_entities {
                        let old = self.beacon_affected_entities[effected_entity];

                        *self.beacon_affected_entities.get_mut(effected_entity).expect("Beacon affected entities list did not include a beacons affected entity") = (old.0 - effect.0, old.1 - effect.1, old.2 - effect.2);

                        // The effect on the other grids is already handled in  remove_pole
                    }
                }

                self.num_beacons_of_type[usize::from(*ty)] -= 1;
            },
        }
    }

    // The caller is responsible that the connected Entity is removed from self's graph
    #[profiling::function]
    fn move_connected_entities<'a, 'b, 'c>(
        &'a mut self,
        other: &'b mut Self,
        data_store: &'c DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<
        Item = (
            Position,
            PowerGridEntity<ItemIdxType, RecipeIdxType>,
            PowerGridEntity<ItemIdxType, RecipeIdxType>,
        ),
    > + use<'a, 'b, 'c, ItemIdxType, RecipeIdxType> {
        other
            .grid_graph
            .weak_components_mut()
            .into_iter()
            .map(|connected_entity| match &mut connected_entity.1 {
                PowerGridEntity::Assembler { ty, recipe, index } => {
                    let new_idx =
                        self.move_assembler(&mut other.stores, *recipe, *index, data_store);

                    self.num_assemblers_of_type[usize::from(*ty)] -= 1;
                    other.num_assemblers_of_type[usize::from(*ty)] += 1;

                    let ret = (
                        connected_entity.0,
                        PowerGridEntity::Assembler {
                            ty: *ty,
                            recipe: *recipe,
                            index: (*index).try_into().unwrap(),
                        },
                        PowerGridEntity::Assembler {
                            ty: *ty,
                            recipe: *recipe,
                            index: new_idx.try_into().unwrap(),
                        },
                    );

                    *index = new_idx.try_into().unwrap();

                    ret
                },
                PowerGridEntity::Lab { index, ty } => {
                    let new_idx = self.move_lab(&mut other.lab_stores, *index, data_store);

                    let ret = (
                        connected_entity.0,
                        PowerGridEntity::Lab {
                            ty: *ty,
                            index: *index,
                        },
                        PowerGridEntity::Lab {
                            ty: *ty,
                            index: new_idx,
                        },
                    );

                    *index = new_idx.try_into().unwrap();

                    ret
                },
                PowerGridEntity::LazyPowerProducer { item, index } => {
                    todo!("Move LazyPowerProducer (Steam Engine)")
                },
                PowerGridEntity::SolarPanel { ty } => {
                    // FIXME: Respect ty
                    self.num_solar_panels_of_type[usize::from(*ty)] -= 1;
                    other.num_solar_panels_of_type[usize::from(*ty)] += 1;

                    (
                        connected_entity.0,
                        PowerGridEntity::SolarPanel { ty: *ty },
                        PowerGridEntity::SolarPanel { ty: *ty },
                    )
                },
                PowerGridEntity::Accumulator { ty } => {
                    // FIXME: Respect ty
                    self.main_accumulator_count[usize::from(*ty)] -= 1;
                    other.main_accumulator_count[usize::from(*ty)] += 1;

                    (
                        connected_entity.0,
                        PowerGridEntity::Accumulator { ty: *ty },
                        PowerGridEntity::Accumulator { ty: *ty },
                    )
                },
                PowerGridEntity::Beacon {
                    ty,
                    modules,
                    affected_entities,
                } => {
                    self.num_beacons_of_type[usize::from(*ty)] -= 1;
                    other.num_beacons_of_type[usize::from(*ty)] += 1;

                    let raw_effect: (i16, i16, i16) = modules
                        .iter()
                        .flatten()
                        .map(|module_ty| {
                            (
                                data_store.module_info[*module_ty].speed_mod.into(),
                                data_store.module_info[*module_ty].prod_mod.into(),
                                data_store.module_info[*module_ty].power_mod.into(),
                            )
                        })
                        .reduce(|acc, v| (acc.0 + v.0, acc.1 + v.1, acc.2 + v.2))
                        .unwrap_or((0, 0, 0));

                    let raw_effect = (
                        raw_effect.0
                            * data_store.beacon_info[usize::from(*ty)].effectiveness.0 as i16
                            / data_store.beacon_info[usize::from(*ty)].effectiveness.1 as i16,
                        raw_effect.1
                            * data_store.beacon_info[usize::from(*ty)].effectiveness.0 as i16
                            / data_store.beacon_info[usize::from(*ty)].effectiveness.1 as i16,
                        raw_effect.2
                            * data_store.beacon_info[usize::from(*ty)].effectiveness.0 as i16
                            / data_store.beacon_info[usize::from(*ty)].effectiveness.1 as i16,
                    );

                    for affected_entity in affected_entities {
                        let entry = self
                            .beacon_affected_entities
                            .get_mut(&affected_entity)
                            .unwrap();

                        entry.0 -= raw_effect.0;
                        entry.1 -= raw_effect.1;
                        entry.2 -= raw_effect.2;

                        let entry = other
                            .beacon_affected_entities
                            .entry(*affected_entity).or_insert((0, 0, 0));

                        entry.0 += raw_effect.0;
                        entry.1 += raw_effect.1;
                        entry.2 += raw_effect.2;
                    }

                    assert_eq!(self.last_power_mult, other.last_power_mult, "If the destination power grid has a different power mult, then we would have to change the modifiers on the assemblers");

                    // FIXME: Cloning here sucks
                    (connected_entity.0, connected_entity.1.clone(), connected_entity.1.clone())
                },
            })
    }

    pub fn add_module_to_assembler(
        &mut self,
        id: AssemblerID<RecipeIdxType>,
        module_kind: usize,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let speed_mod = data_store.module_info[module_kind].speed_mod;
        let prod_mod = data_store.module_info[module_kind].prod_mod;
        let power_mod = data_store.module_info[module_kind].power_mod;

        assert!(!self.is_placeholder);

        match (
            data_store.recipe_num_ing_lookup[id.recipe.id.into()],
            data_store.recipe_num_out_lookup[id.recipe.id.into()],
        ) {
            (0, 1) => self.stores.assemblers_0_1
                [data_store.recipe_to_ing_out_combo_idx[id.recipe.id.into()]]
            .modify_modifiers(
                id.assembler_index,
                speed_mod.into(),
                prod_mod.into(),
                power_mod.into(),
                data_store,
            ),
            (1, 1) => self.stores.assemblers_1_1
                [data_store.recipe_to_ing_out_combo_idx[id.recipe.id.into()]]
            .modify_modifiers(
                id.assembler_index,
                speed_mod.into(),
                prod_mod.into(),
                power_mod.into(),
                data_store,
            ),

            (2, 1) => self.stores.assemblers_2_1
                [data_store.recipe_to_ing_out_combo_idx[id.recipe.id.into()]]
            .modify_modifiers(
                id.assembler_index,
                speed_mod.into(),
                prod_mod.into(),
                power_mod.into(),
                data_store,
            ),

            (3, 1) => self.stores.assemblers_3_1
                [data_store.recipe_to_ing_out_combo_idx[id.recipe.id.into()]]
            .modify_modifiers(
                id.assembler_index,
                speed_mod.into(),
                prod_mod.into(),
                power_mod.into(),
                data_store,
            ),

            (4, 1) => self.stores.assemblers_4_1
                [data_store.recipe_to_ing_out_combo_idx[id.recipe.id.into()]]
            .modify_modifiers(
                id.assembler_index,
                speed_mod.into(),
                prod_mod.into(),
                power_mod.into(),
                data_store,
            ),

            _ => unreachable!(),
        };
    }

    pub fn remove_module_from_assembler(
        &mut self,
        id: AssemblerID<RecipeIdxType>,
        module_kind: usize,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        // TODO: This will crash if someone uses i8::MIN but oh well
        let speed_mod = data_store.module_info[module_kind]
            .speed_mod
            .checked_neg()
            .expect("Negation failed");
        let prod_mod = data_store.module_info[module_kind]
            .prod_mod
            .checked_neg()
            .expect("Negation failed");
        let power_mod = data_store.module_info[module_kind]
            .power_mod
            .checked_neg()
            .expect("Negation failed");

        assert!(!self.is_placeholder);

        match (
            data_store.recipe_num_ing_lookup[id.recipe.id.into()],
            data_store.recipe_num_out_lookup[id.recipe.id.into()],
        ) {
            (0, 1) => self.stores.assemblers_0_1
                [data_store.recipe_to_ing_out_combo_idx[id.recipe.id.into()]]
            .modify_modifiers(
                id.assembler_index,
                speed_mod.into(),
                prod_mod.into(),
                power_mod.into(),
                data_store,
            ),
            (1, 1) => self.stores.assemblers_1_1
                [data_store.recipe_to_ing_out_combo_idx[id.recipe.id.into()]]
            .modify_modifiers(
                id.assembler_index,
                speed_mod.into(),
                prod_mod.into(),
                power_mod.into(),
                data_store,
            ),

            (2, 1) => self.stores.assemblers_2_1
                [data_store.recipe_to_ing_out_combo_idx[id.recipe.id.into()]]
            .modify_modifiers(
                id.assembler_index,
                speed_mod.into(),
                prod_mod.into(),
                power_mod.into(),
                data_store,
            ),

            (3, 1) => self.stores.assemblers_3_1
                [data_store.recipe_to_ing_out_combo_idx[id.recipe.id.into()]]
            .modify_modifiers(
                id.assembler_index,
                speed_mod.into(),
                prod_mod.into(),
                power_mod.into(),
                data_store,
            ),

            (4, 1) => self.stores.assemblers_4_1
                [data_store.recipe_to_ing_out_combo_idx[id.recipe.id.into()]]
            .modify_modifiers(
                id.assembler_index,
                speed_mod.into(),
                prod_mod.into(),
                power_mod.into(),
                data_store,
            ),

            _ => unreachable!(),
        };
    }

    pub fn add_module_to_lab(
        &mut self,
        index: u32,
        module_kind: usize,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let speed_mod = data_store.module_info[module_kind].speed_mod;
        let prod_mod = data_store.module_info[module_kind].prod_mod;
        let power_mod = data_store.module_info[module_kind].power_mod;

        assert!(!self.is_placeholder);

        self.lab_stores.modify_modifiers(
            index,
            speed_mod.into(),
            prod_mod.into(),
            power_mod.into(),
            data_store,
        );
    }

    pub fn remove_module_from_lab(
        &mut self,
        index: u32,
        module_kind: usize,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        // TODO: This will crash if someone uses i8::MIN but oh well
        let speed_mod = data_store.module_info[module_kind]
            .speed_mod
            .checked_neg()
            .expect("Negation failed");
        let prod_mod = data_store.module_info[module_kind]
            .prod_mod
            .checked_neg()
            .expect("Negation failed");
        let power_mod = data_store.module_info[module_kind]
            .power_mod
            .checked_neg()
            .expect("Negation failed");

        assert!(!self.is_placeholder);

        self.lab_stores.modify_modifiers(
            index,
            speed_mod.into(),
            prod_mod.into(),
            power_mod.into(),
            data_store,
        );
    }

    pub(super) fn change_assembler_module_modifiers(
        &mut self,
        id: AssemblerID<RecipeIdxType>,
        modifiers: (i16, i16, i16),
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let speed_mod = modifiers.0;
        let prod_mod = modifiers.1;
        let power_mod = modifiers.2;

        assert!(!self.is_placeholder);

        match (
            data_store.recipe_num_ing_lookup[id.recipe.id.into()],
            data_store.recipe_num_out_lookup[id.recipe.id.into()],
        ) {
            (0, 1) => self.stores.assemblers_0_1
                [data_store.recipe_to_ing_out_combo_idx[id.recipe.id.into()]]
            .modify_modifiers(
                id.assembler_index,
                speed_mod.into(),
                prod_mod.into(),
                power_mod.into(),
                data_store,
            ),
            (1, 1) => self.stores.assemblers_1_1
                [data_store.recipe_to_ing_out_combo_idx[id.recipe.id.into()]]
            .modify_modifiers(
                id.assembler_index,
                speed_mod.into(),
                prod_mod.into(),
                power_mod.into(),
                data_store,
            ),

            (2, 1) => self.stores.assemblers_2_1
                [data_store.recipe_to_ing_out_combo_idx[id.recipe.id.into()]]
            .modify_modifiers(
                id.assembler_index,
                speed_mod.into(),
                prod_mod.into(),
                power_mod.into(),
                data_store,
            ),
            (3, 1) => self.stores.assemblers_3_1
                [data_store.recipe_to_ing_out_combo_idx[id.recipe.id.into()]]
            .modify_modifiers(
                id.assembler_index,
                speed_mod.into(),
                prod_mod.into(),
                power_mod.into(),
                data_store,
            ),
            (4, 1) => self.stores.assemblers_4_1
                [data_store.recipe_to_ing_out_combo_idx[id.recipe.id.into()]]
            .modify_modifiers(
                id.assembler_index,
                speed_mod.into(),
                prod_mod.into(),
                power_mod.into(),
                data_store,
            ),

            _ => unreachable!(),
        };
    }

    pub fn change_lab_module_modifiers(
        &mut self,
        index: u32,
        modifiers: (i16, i16, i16),
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let speed_mod = modifiers.0;
        let prod_mod = modifiers.1;
        let power_mod = modifiers.2;

        assert!(!self.is_placeholder);

        self.lab_stores.modify_modifiers(
            index,
            speed_mod.into(),
            prod_mod.into(),
            power_mod.into(),
            data_store,
        );
    }

    pub fn add_assembler(
        &mut self,
        ty: u8,
        grid_id: PowerGridIdentifier,
        recipe: Recipe<RecipeIdxType>,
        modules: &[Option<usize>],
        connected_power_pole_position: Position,
        assembler_position: Position,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (AssemblerID<RecipeIdxType>, WeakIndex) {
        assert!(!self.is_placeholder);

        self.num_assemblers_of_type[usize::from(ty)] += 1;

        let new_idx = match (
            data_store.recipe_num_ing_lookup[recipe.id.into()],
            data_store.recipe_num_out_lookup[recipe.id.into()],
        ) {
            (0, 1) => self.stores.assemblers_0_1
                [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]]
            .add_assembler(ty, modules, assembler_position, data_store),

            (1, 1) => self.stores.assemblers_1_1
                [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]]
            .add_assembler(ty, modules, assembler_position, data_store),

            (2, 1) => self.stores.assemblers_2_1
                [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]]
            .add_assembler(ty, modules, assembler_position, data_store),

            (3, 1) => self.stores.assemblers_3_1
                [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]]
            .add_assembler(ty, modules, assembler_position, data_store),
            (4, 1) => self.stores.assemblers_4_1
                [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]]
            .add_assembler(ty, modules, assembler_position, data_store),

            _ => unreachable!(),
        };

        let weak_index = self.grid_graph.add_weak_element(
            connected_power_pole_position,
            (
                assembler_position,
                PowerGridEntity::Assembler {
                    ty,
                    recipe,
                    index: new_idx.try_into().unwrap(),
                },
            ),
        );

        (
            AssemblerID {
                recipe,
                grid: grid_id,
                assembler_index: new_idx.try_into().expect("More than u16::MAX assemblers"),
            },
            weak_index,
        )
    }

    #[must_use]
    pub fn change_assembler_recipe(
        &mut self,
        old_assembler_id: AssemblerID<RecipeIdxType>,
        pole_pos: Position,
        weak_idx: WeakIndex,
        new_recipe: Recipe<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (AssemblerRemovalInfo, AssemblerID<RecipeIdxType>) {
        let (
            _max_insert,
            ings,
            outputs,
            _timer,
            _prod_timer,
            base_power,
            raw_power_mod,
            raw_prod_mod,
            base_speed,
            raw_speed_mod,
            ty,
            pos,
        ) = match (
            data_store.recipe_num_ing_lookup[old_assembler_id.recipe.id.into()],
            data_store.recipe_num_out_lookup[old_assembler_id.recipe.id.into()],
        ) {
            (0, 1) => self.stores.assemblers_0_1
                [data_store.recipe_to_ing_out_combo_idx[old_assembler_id.recipe.id.into()]]
            .remove_assembler_data(old_assembler_id.assembler_index),
            (1, 1) => self.stores.assemblers_1_1
                [data_store.recipe_to_ing_out_combo_idx[old_assembler_id.recipe.id.into()]]
            .remove_assembler_data(old_assembler_id.assembler_index),
            (2, 1) => self.stores.assemblers_2_1
                [data_store.recipe_to_ing_out_combo_idx[old_assembler_id.recipe.id.into()]]
            .remove_assembler_data(old_assembler_id.assembler_index),
            (3, 1) => self.stores.assemblers_3_1
                [data_store.recipe_to_ing_out_combo_idx[old_assembler_id.recipe.id.into()]]
            .remove_assembler_data(old_assembler_id.assembler_index),
            (4, 1) => self.stores.assemblers_4_1
                [data_store.recipe_to_ing_out_combo_idx[old_assembler_id.recipe.id.into()]]
            .remove_assembler_data(old_assembler_id.assembler_index),

            _ => unreachable!(),
        };

        let new_id = match (
            data_store.recipe_num_ing_lookup[new_recipe.id.into()],
            data_store.recipe_num_out_lookup[new_recipe.id.into()],
        ) {
            (0, 1) => self.stores.assemblers_0_1
                [data_store.recipe_to_ing_out_combo_idx[new_recipe.id.into()]]
            .add_assembler_with_data(
                [10; 0],
                [0; 0],
                [0; 1],
                0,
                0,
                base_power,
                raw_power_mod,
                raw_prod_mod,
                base_speed,
                raw_speed_mod,
                ty,
                pos,
                data_store,
            ),
            (1, 1) => self.stores.assemblers_1_1
                [data_store.recipe_to_ing_out_combo_idx[new_recipe.id.into()]]
            .add_assembler_with_data(
                [10; 1],
                [0; 1],
                [0; 1],
                0,
                0,
                base_power,
                raw_power_mod,
                raw_prod_mod,
                base_speed,
                raw_speed_mod,
                ty,
                pos,
                data_store,
            ),
            (2, 1) => self.stores.assemblers_2_1
                [data_store.recipe_to_ing_out_combo_idx[new_recipe.id.into()]]
            .add_assembler_with_data(
                [10; 2],
                [0; 2],
                [0; 1],
                0,
                0,
                base_power,
                raw_power_mod,
                raw_prod_mod,
                base_speed,
                raw_speed_mod,
                ty,
                pos,
                data_store,
            ),
            (3, 1) => self.stores.assemblers_3_1
                [data_store.recipe_to_ing_out_combo_idx[new_recipe.id.into()]]
            .add_assembler_with_data(
                [10; 3],
                [0; 3],
                [0; 1],
                0,
                0,
                base_power,
                raw_power_mod,
                raw_prod_mod,
                base_speed,
                raw_speed_mod,
                ty,
                pos,
                data_store,
            ),
            (4, 1) => self.stores.assemblers_4_1
                [data_store.recipe_to_ing_out_combo_idx[new_recipe.id.into()]]
            .add_assembler_with_data(
                [10; 4],
                [0; 4],
                [0; 1],
                0,
                0,
                base_power,
                raw_power_mod,
                raw_prod_mod,
                base_speed,
                raw_speed_mod,
                ty,
                pos,
                data_store,
            ),

            _ => unreachable!(),
        };

        let (
            _pos,
            PowerGridEntity::Assembler {
                ty: _,
                recipe,
                index,
            },
        ) = self.grid_graph.modify_weak_component(pole_pos, weak_idx)
        else {
            unreachable!()
        };

        *recipe = new_recipe;
        *index = new_id.try_into().unwrap();

        (
            AssemblerRemovalInfo { ings, outputs },
            AssemblerID {
                recipe: new_recipe,
                grid: old_assembler_id.grid,
                assembler_index: new_id.try_into().unwrap(),
            },
        )
    }

    pub fn remove_assembler(
        &mut self,
        assembler_id: AssemblerID<RecipeIdxType>,
        pole_pos: Position,
        weak_idx: WeakIndex,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerRemovalInfo {
        let removal_info = self.remove_assembler_raw(assembler_id, data_store);

        let (pos, PowerGridEntity::Assembler { ty, recipe, index }) =
            self.grid_graph.remove_weak_element(pole_pos, weak_idx)
        else {
            unreachable!()
        };

        self.num_assemblers_of_type[usize::from(ty)] -= 1;

        removal_info
    }

    fn remove_assembler_raw(
        &mut self,
        assembler_id: AssemblerID<RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> AssemblerRemovalInfo {
        let removal_info = match (
            data_store.recipe_num_ing_lookup[assembler_id.recipe.id.into()],
            data_store.recipe_num_out_lookup[assembler_id.recipe.id.into()],
        ) {
            (0, 1) => self.stores.assemblers_0_1
                [data_store.recipe_to_ing_out_combo_idx[assembler_id.recipe.id.into()]]
            .remove_assembler(assembler_id.assembler_index),
            (1, 1) => self.stores.assemblers_1_1
                [data_store.recipe_to_ing_out_combo_idx[assembler_id.recipe.id.into()]]
            .remove_assembler(assembler_id.assembler_index),
            (2, 1) => self.stores.assemblers_2_1
                [data_store.recipe_to_ing_out_combo_idx[assembler_id.recipe.id.into()]]
            .remove_assembler(assembler_id.assembler_index),
            (3, 1) => self.stores.assemblers_3_1
                [data_store.recipe_to_ing_out_combo_idx[assembler_id.recipe.id.into()]]
            .remove_assembler(assembler_id.assembler_index),
            (4, 1) => self.stores.assemblers_4_1
                [data_store.recipe_to_ing_out_combo_idx[assembler_id.recipe.id.into()]]
            .remove_assembler(assembler_id.assembler_index),

            _ => unreachable!(),
        };

        removal_info
    }

    fn move_assembler(
        &mut self,
        other_stores: &mut FullAssemblerStore<RecipeIdxType>,
        recipe: Recipe<RecipeIdxType>,
        index: u32,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> u32 {
        match (
            data_store.recipe_num_ing_lookup[recipe.id.into()],
            data_store.recipe_num_out_lookup[recipe.id.into()],
        ) {
            (0, 1) => self.stores.assemblers_0_1
                [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]]
            .move_assembler(
                index,
                &mut other_stores.assemblers_0_1
                    [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]],
                data_store,
            ),
            (1, 1) => self.stores.assemblers_1_1
                [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]]
            .move_assembler(
                index,
                &mut other_stores.assemblers_1_1
                    [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]],
                data_store,
            ),
            (2, 1) => self.stores.assemblers_2_1
                [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]]
            .move_assembler(
                index,
                &mut other_stores.assemblers_2_1
                    [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]],
                data_store,
            ),
            (3, 1) => self.stores.assemblers_3_1
                [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]]
            .move_assembler(
                index,
                &mut other_stores.assemblers_3_1
                    [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]],
                data_store,
            ),
            (4, 1) => self.stores.assemblers_4_1
                [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]]
            .move_assembler(
                index,
                &mut other_stores.assemblers_4_1
                    [data_store.recipe_to_ing_out_combo_idx[recipe.id.into()]],
                data_store,
            ),

            _ => unreachable!(),
        }
    }

    fn move_lab(
        &mut self,
        other_stores: &mut MultiLabStore,
        index: u32,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> u32 {
        self.lab_stores.move_lab(index, other_stores)
    }

    // FIXME: This is a huge, high branching function.
    // Make it simpler and more readable, and reduce repetition
    #[profiling::function]
    fn extract_power(
        &mut self,
        goal_amount: Joule,
        solar_panel_production_amounts: &[Watt],
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Joule {
        assert_eq!(
            solar_panel_production_amounts.len(),
            self.num_solar_panels_of_type.len()
        );
        let solar_power = self
            .num_solar_panels_of_type
            .iter()
            .zip(solar_panel_production_amounts)
            .map(|(a, b)| *b * *a)
            .sum::<Watt>()
            .joules_per_tick();

        self.last_ticks_max_power_production = solar_power.watt_from_tick() + self.max_lazy_power;

        if goal_amount == Joule(0) {
            return Joule(0);
        }

        let max_charge_amount_per: Box<[Joule]> = self
            .main_accumulator_count
            .iter()
            .zip(self.main_accumulator_charge.iter().copied())
            .zip(
                data_store
                    .accumulator_info
                    .iter()
                    .map(|info| info.max_charge_rate),
            )
            .zip(
                data_store
                    .accumulator_info
                    .iter()
                    .map(|info| info.max_charge),
            )
            .map(
                |(((count, charge), max_charge_rate), max_charge): (
                    ((&u64, Joule), Watt),
                    Joule,
                )| {
                    min(max_charge_rate.joules_per_tick(), max_charge - charge) * *count
                },
            )
            .collect();
        let max_charge_amount = max_charge_amount_per.iter().copied().sum();

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
                self.charge_by(max_charge_amount, &max_charge_amount_per);
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

                self.charge_by(actually_extracted + remaining_solar, &max_charge_amount_per);
            } else {
                // We only charge using solar
                self.charge_by(remaining_solar, &max_charge_amount_per);
            }
            goal_amount
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

                        let actually_discharged: Joule = self.extract_from_accumulators(
                            power_missing_to_discharge_from_accs,
                            data_store,
                        );

                        assert!(actually_discharged + actually_extracted <= goal_amount);

                        return actually_discharged + actually_extracted;
                    },
                    std::cmp::Ordering::Equal => return goal_amount,
                    std::cmp::Ordering::Greater => {
                        // Use remaining power for charging
                        let charge_amount = goal_amount - actually_extracted; // This will never underflow

                        self.charge_by(charge_amount, &max_charge_amount_per);

                        return goal_amount;
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

                    let actually_discharged = self.extract_from_accumulators(
                        power_missing_to_discharge_from_accs,
                        data_store,
                    );

                    assert!(actually_discharged + actually_extracted + solar_power <= goal_amount);

                    actually_discharged + actually_extracted + solar_power
                },
                std::cmp::Ordering::Equal => goal_amount,
                std::cmp::Ordering::Greater => {
                    unreachable!(
                        "We extracted more power than needed from burners, while disallowing charging?!"
                    );
                },
            }
        }
    }

    fn charge_by(&mut self, mut amount: Joule, max_charge_amount_per: &[Joule]) {
        assert!(
            max_charge_amount_per.iter().copied().sum::<Joule>() >= amount,
            "Tried to charge the accumulators more than max amount"
        );

        debug_assert!(max_charge_amount_per.is_sorted());

        // Since we sort, we can simply go through the list sequentially
        for ((charge, max_rate), num_left) in self
            .main_accumulator_charge
            .iter_mut()
            .zip(max_charge_amount_per)
            .zip((1..=max_charge_amount_per.len()).rev())
            .sorted_by_key(|((_charge, max_rate), _num_left)| *max_rate)
        {
            // FIXME: This is integer division, so we lose some power here.
            let charge_here: Joule = min(*max_rate, amount / u64::try_from(num_left).unwrap());
            *charge = *charge + charge_here;
            amount = amount - charge_here;
        }

        // Due to the integer div, this assert could fail
        assert!(amount == Joule(0));

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
    fn extract_from_accumulators(
        &mut self,
        mut power_needed: Joule,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Joule {
        let originally_needed = power_needed;
        let discharge_amount_per: Box<[Joule]> = self
            .main_accumulator_count
            .iter()
            .zip(self.main_accumulator_charge.iter().copied())
            .zip(
                data_store
                    .accumulator_info
                    .iter()
                    .map(|info| info.max_discharge_rate),
            )
            .map(
                |((count, charge), max_discharge_rate): ((&u64, Joule), Watt)| {
                    min(max_discharge_rate.joules_per_tick(), charge) * *count
                },
            )
            .collect();

        // Since we sort, we can simply go through the list sequentially
        for ((charge, max_rate), num_left) in self
            .main_accumulator_charge
            .iter_mut()
            .zip(discharge_amount_per)
            .zip((1..=data_store.accumulator_info.len()).rev())
            .sorted_by_key(|((charge, max_rate), num_left)| *max_rate)
        {
            // FIXME: This is integer division, so we lose some power here.
            let discharge_from_here: Joule =
                min(max_rate, power_needed / u64::try_from(num_left).unwrap());
            assert!(*charge >= discharge_from_here);
            *charge = *charge - discharge_from_here;
            power_needed = power_needed - discharge_from_here;
        }

        originally_needed - power_needed
    }

    #[profiling::function]
    pub fn update(
        &mut self,
        solar_panel_production_amounts: &[Watt],
        tech_state: &TechState,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> (
        ResearchProgress,
        RecipeTickInfo,
        u64,
        Vec<(BeaconAffectedEntity<RecipeIdxType>, (i16, i16, i16))>,
    ) {
        let active_recipes = tech_state.get_active_recipes();

        let (
            (
                (power_used_0_1, infos_0_1),
                (
                    (power_used_1_1, infos_1_1),
                    (
                        (power_used_2_1, infos_2_1),
                        ((power_used_3_1, infos_3_1), (power_used_4_1, infos_4_1)),
                    ),
                ),
            ),
            (lab_power_used, times_labs_used_science, tech_progress),
        ) = rayon::join(
            || {
                rayon::join(
                    || {
                        profiling::scope!("assemblers_0_1 updates");
                        self.stores
                            .assemblers_0_1
                            .par_iter_mut()
                            .map(|s| {
                                profiling::scope!("Assembler Update", format!("Recipe: {}", data_store.recipe_names[usize_from(s.recipe.id)]).as_str());
                                if active_recipes[s.recipe.into_usize()] {
                                    s.update_branchless::<RecipeIdxType>(
                                        self.last_power_mult,
                                        &data_store.recipe_index_lookups,
                                        &data_store.recipe_ings.ing0,
                                        &data_store.recipe_outputs.out1,
                                        &data_store.recipe_timers,
                                    )
                                } else {
                                    (Watt(0), 0, 0)
                                }
                            })
                            .map(|(power_used, times_ings_used, crafts_finished)| {
                                (power_used, SingleRecipeTickInfo {
                                    full_crafts: times_ings_used as u64,
                                    prod_crafts: crafts_finished.checked_sub(times_ings_used).expect("More ingredients used than crafts finished?!? Negative productivity?") as u64,
                                })
                            })
                            .fold_with((Watt(0), vec![]), |(acc_power, mut infos), (rhs_power, info)| {
                                infos.push(info);

                                (acc_power + rhs_power, infos)
                            }).reduce(|| (Watt(0), vec![]), |(acc_power, mut infos), (rhs_power, info)| {
                                infos.extend_from_slice(&info);

                                (acc_power + rhs_power, infos)
                            })
                    },
                    || {
                        rayon::join(
                            || {
                                profiling::scope!("assemblers_1_1 updates");
                                self.stores
                        .assemblers_1_1
                        .par_iter_mut()
                        .map(|s| {
                            profiling::scope!("Assembler Update", format!("Recipe: {}", data_store.recipe_names[usize_from(s.recipe.id)]).as_str());
                            if active_recipes[s.recipe.into_usize()] {
                                s.update_branchless::<RecipeIdxType>(
                                    self.last_power_mult,
                                    &data_store.recipe_index_lookups,
                                    &data_store.recipe_ings.ing1,
                                    &data_store.recipe_outputs.out1,
                                    &data_store.recipe_timers,
                                )
                            } else {
                                (Watt(0), 0, 0)
                            }
                        })
                        .map(|(power_used, times_ings_used, crafts_finished)| {
                            (power_used, SingleRecipeTickInfo {
                                full_crafts: times_ings_used as u64,
                                prod_crafts: crafts_finished.checked_sub(times_ings_used).expect("More ingredients used than crafts finished?!? Negative productivity?") as u64,
                            })
                        })
                        .fold_with((Watt(0), vec![]), |(acc_power, mut infos), (rhs_power, info)| {
                            infos.push(info);

                            (acc_power + rhs_power, infos)
                        }).reduce(|| (Watt(0), vec![]), |(acc_power, mut infos), (rhs_power, info)| {
                            infos.extend_from_slice(&info);

                            (acc_power + rhs_power, infos)
                        })
                            },
                            || {
                                rayon::join(
                                    || {
                                        profiling::scope!("assemblers_2_1 updates");
                                        self.stores
                                    .assemblers_2_1
                                    .par_iter_mut()
                                    .map(|s| {
                                        profiling::scope!("Assembler Update", format!("Recipe: {}", data_store.recipe_names[usize_from(s.recipe.id)]).as_str());
                                        if active_recipes[s.recipe.into_usize()] {
                                            s.update_branchless::<RecipeIdxType>(
                                                self.last_power_mult,
                                                &data_store.recipe_index_lookups,
                                                &data_store.recipe_ings.ing2,
                                                &data_store.recipe_outputs.out1,
                                                &data_store.recipe_timers,
                                            )
                                        } else {
                                            (Watt(0), 0, 0)
                                        }
                                    })
                                    .map(|(power_used, times_ings_used, crafts_finished)| {
                                        (power_used, SingleRecipeTickInfo {
                                            full_crafts: times_ings_used as u64,
                                            prod_crafts: crafts_finished.checked_sub(times_ings_used).expect("More ingredients used than crafts finished?!? Negative productivity?") as u64,
                                        })
                                    })
                                    .fold_with((Watt(0), vec![]), |(acc_power, mut infos), (rhs_power, info)| {
                                        infos.push(info);

                                        (acc_power + rhs_power, infos)
                                    }).reduce(|| (Watt(0), vec![]), |(acc_power, mut infos), (rhs_power, info)| {
                                        infos.extend_from_slice(&info);

                                        (acc_power + rhs_power, infos)
                                    })
                                    },
                                    || {
                                        rayon::join(
                                            || {
                                                profiling::scope!("assemblers_3_1 updates");
                                                self.stores
                                        .assemblers_3_1
                                        .par_iter_mut()
                                        .map(|s| {
                                            profiling::scope!("Assembler Update", format!("Recipe: {}", data_store.recipe_names[usize_from(s.recipe.id)]).as_str());
                                            if active_recipes[s.recipe.into_usize()] {
                                                s.update_branchless::<RecipeIdxType>(
                                                    self.last_power_mult,
                                                    &data_store.recipe_index_lookups,
                                                    &data_store.recipe_ings.ing3,
                                                    &data_store.recipe_outputs.out1,
                                                    &data_store.recipe_timers,
                                                )
                                            } else {
                                                (Watt(0), 0, 0)
                                            }
                                        })
                                        .map(|(power_used, times_ings_used, crafts_finished)| {
                                            (power_used, SingleRecipeTickInfo {
                                                full_crafts: times_ings_used as u64,
                                                prod_crafts: crafts_finished.checked_sub(times_ings_used).expect("More ingredients used than crafts finished?!? Negative productivity?") as u64,
                                            })
                                        })
                                        .fold_with((Watt(0), vec![]), |(acc_power, mut infos), (rhs_power, info)| {
                                            infos.push(info);

                                            (acc_power + rhs_power, infos)
                                        }).reduce(|| (Watt(0), vec![]), |(acc_power, mut infos), (rhs_power, info)| {
                                            infos.extend_from_slice(&info);

                                            (acc_power + rhs_power, infos)
                                        })
                                            },
                                            || {
                                                profiling::scope!("assemblers_4_1 updates");
                                                self.stores
                                        .assemblers_4_1
                                        .par_iter_mut()
                                        .map(|s| {
                                            profiling::scope!("Assembler Update", format!("Recipe: {}", data_store.recipe_names[usize_from(s.recipe.id)]).as_str());
                                            if active_recipes[s.recipe.into_usize()] {
                                                s.update_branchless::<RecipeIdxType>(
                                                    self.last_power_mult,
                                                    &data_store.recipe_index_lookups,
                                                    &data_store.recipe_ings.ing4,
                                                    &data_store.recipe_outputs.out1,
                                                    &data_store.recipe_timers,
                                                )
                                            } else {
                                                (Watt(0), 0, 0)
                                            }
                                        })
                                        .map(|(power_used, times_ings_used, crafts_finished)| {
                                            (power_used, SingleRecipeTickInfo {
                                                full_crafts: times_ings_used as u64,
                                                prod_crafts: crafts_finished.checked_sub(times_ings_used).expect("More ingredients used than crafts finished?!? Negative productivity?") as u64,
                                            })
                                        })
                                        .fold_with((Watt(0), vec![]), |(acc_power, mut infos), (rhs_power, info)| {
                                            infos.push(info);

                                            (acc_power + rhs_power, infos)
                                        }).reduce(|| (Watt(0), vec![]), |(acc_power, mut infos), (rhs_power, info)| {
                                            infos.extend_from_slice(&info);

                                            (acc_power + rhs_power, infos)
                                        })
                                            },
                                        )
                                    },
                                )
                            },
                        )
                    },
                )
            },
            || {
                profiling::scope!("Lab updates");
                self.lab_stores.update(
                    self.last_power_mult,
                    tech_state
                        .current_technology
                        .as_ref()
                        .map(|tech| &*data_store.technology_costs[tech.id as usize].1),
                    data_store,
                )
            },
        );

        let assembler_power_used =
            power_used_0_1 + power_used_1_1 + power_used_2_1 + power_used_3_1 + power_used_4_1;

        let beacon_power_used: Watt = self
            .num_beacons_of_type
            .iter()
            .zip(&data_store.beacon_info)
            .map(|(count, info)| {
                info.power_consumption * u64::try_from(*count).expect("More than u64::MAX Beacons")
            })
            .sum();

        let power_used = assembler_power_used.joules_per_tick()
            + lab_power_used
            + beacon_power_used.joules_per_tick();

        self.last_power_consumption = power_used.watt_from_tick();

        let power_extracted =
            self.extract_power(power_used, solar_panel_production_amounts, data_store);

        assert!(power_extracted <= power_used);
        let next_power_mult = if power_used == Joule(0) {
            0
        } else {
            ((power_extracted.0 * MAX_POWER_MULT as u64) / power_used.0) as u8
        };
        assert!(next_power_mult <= MAX_POWER_MULT);

        self.last_power_consumption = power_used.watt_from_tick();
        self.last_produced_power = power_extracted.watt_from_tick();

        // TODO: Factorio just scales the beacon effect linearly
        let beacon_updates: Vec<(BeaconAffectedEntity<_>, (_, _, _))> = if next_power_mult
            < MIN_BEACON_POWER_MULT
            && self.last_power_mult >= MIN_BEACON_POWER_MULT
        {
            // Disable beacons (But keep power consumption modifier unchanged, to prevent flickering)
            self.beacon_affected_entities
                .iter()
                .map(|(k, v)| (*k, (-v.0, -v.1, -0)))
                .collect()
        } else if next_power_mult >= MIN_BEACON_POWER_MULT
            && self.last_power_mult < MIN_BEACON_POWER_MULT
        {
            // Enable beacons (But keep power consumption modifier unchanged, to prevent flickering)
            self.beacon_affected_entities
                .iter()
                .map(|(k, v)| (*k, (v.0, v.1, 0)))
                .collect()
        } else {
            vec![]
        };

        self.power_mult_history
            .append_single_set_of_samples(next_power_mult.into());

        self.last_power_mult = next_power_mult;

        let parts = RecipeTickInfoParts {
            recipes_0_1: infos_0_1,
            recipes_1_1: infos_1_1,
            recipes_2_1: infos_2_1,
            recipes_3_1: infos_3_1,
            recipes_4_1: infos_4_1,
        };

        (
            tech_progress,
            RecipeTickInfo::from_parts(parts, data_store),
            times_labs_used_science.into(),
            beacon_updates,
        )
    }

    pub fn add_beacon(
        &mut self,
        ty: u8,
        beacon_pos: Position,
        pole_pos: Position,
        modules: Box<[Option<usize>]>,
        affected_entities: Vec<BeaconAffectedEntity<RecipeIdxType>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> WeakIndex {
        let effect: (i16, i16, i16) = modules
            .iter()
            .flatten()
            .map(|module_ty| {
                (
                    data_store.module_info[*module_ty].speed_mod.into(),
                    data_store.module_info[*module_ty].prod_mod.into(),
                    data_store.module_info[*module_ty].power_mod.into(),
                )
            })
            .reduce(|acc, v| (acc.0 + v.0, acc.1 + v.1, acc.2 + v.2))
            .unwrap_or((0, 0, 0));

        let effect = (
            effect.0 * data_store.beacon_info[usize::from(ty)].effectiveness.0 as i16
                / data_store.beacon_info[usize::from(ty)].effectiveness.1 as i16,
            effect.1 * data_store.beacon_info[usize::from(ty)].effectiveness.0 as i16
                / data_store.beacon_info[usize::from(ty)].effectiveness.1 as i16,
            effect.2 * data_store.beacon_info[usize::from(ty)].effectiveness.0 as i16
                / data_store.beacon_info[usize::from(ty)].effectiveness.1 as i16,
        );

        for affected_entity in &affected_entities {
            let entry = self
                .beacon_affected_entities
                .entry(*affected_entity)
                .or_insert((0, 0, 0));

            entry.0 += effect.0;
            entry.1 += effect.1;
            entry.2 += effect.2;
        }

        let idx = self.grid_graph.add_weak_element(
            pole_pos,
            (
                beacon_pos,
                PowerGridEntity::Beacon {
                    ty,
                    modules,
                    affected_entities,
                },
            ),
        );

        self.num_beacons_of_type[usize::from(ty)] += 1;

        idx
    }

    pub fn remove_beacon(
        &mut self,
        pole_pos: Position,
        weak_idx: WeakIndex,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = (BeaconAffectedEntity<RecipeIdxType>, (i16, i16, i16))> {
        let (
            beacon_pos,
            PowerGridEntity::Beacon {
                ty,
                modules,
                affected_entities,
            },
        ) = self.grid_graph.remove_weak_element(pole_pos, weak_idx)
        else {
            unreachable!();
        };

        self.num_beacons_of_type[usize::from(ty)] -= 1;

        let effect: (i16, i16, i16) = modules
            .iter()
            .flatten()
            .map(|module_ty| {
                (
                    data_store.module_info[*module_ty].speed_mod.into(),
                    data_store.module_info[*module_ty].prod_mod.into(),
                    data_store.module_info[*module_ty].power_mod.into(),
                )
            })
            .reduce(|acc, v| (acc.0 + v.0, acc.1 + v.1, acc.2 + v.2))
            .unwrap_or((0, 0, 0));

        let effect = (
            effect.0 * data_store.beacon_info[usize::from(ty)].effectiveness.0 as i16
                / data_store.beacon_info[usize::from(ty)].effectiveness.1 as i16,
            effect.1 * data_store.beacon_info[usize::from(ty)].effectiveness.0 as i16
                / data_store.beacon_info[usize::from(ty)].effectiveness.1 as i16,
            effect.2 * data_store.beacon_info[usize::from(ty)].effectiveness.0 as i16
                / data_store.beacon_info[usize::from(ty)].effectiveness.1 as i16,
        );

        for affected_entity in affected_entities.iter() {
            let stored_effect = self
                .beacon_affected_entities
                .get_mut(affected_entity)
                .unwrap();

            stored_effect.0 -= effect.0;
            stored_effect.1 -= effect.1;
            stored_effect.2 -= effect.2;
        }

        let now_removed_effect = if self.last_power_mult >= MIN_BEACON_POWER_MULT {
            (-effect.0, -effect.1, -effect.2)
        } else {
            (-0, -0, -effect.2)
        };

        affected_entities
            .into_iter()
            .map(move |entity| (entity, now_removed_effect))
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
