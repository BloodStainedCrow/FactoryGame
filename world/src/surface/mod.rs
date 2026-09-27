use std::collections::{BTreeMap, btree_map::Entry};

use backend::Backend;
use data::{
    entity::{
        GlobalTy, allows_flipping, allows_rotation,
        assember::{AssemblerTy, Recipe, default_recipe},
        belt::BeltTy,
        bounding_box,
        chest::{ChestTy, num_slots},
        inserter::{InserterTy, get_input_position, get_output_position},
        power_pole::{PowerPoleTy, power_pole_supply_area, power_pole_wire_connection_area},
    },
    item::item_set::LimitedItemSet,
    spacial::{BoundingBox, Extent, Flipped, Position, Rotation},
};
use entity_info::{EntityDescriptor, EntityDescriptorKind, EntityInfo, EntityInfoKind};
use itertools::Itertools;
use middle::{
    Middle,
    assembler::{AssemblerAdditionInfo, AssemblerRemovalInfo, InserterTransfer},
    belt::BeltTileAdditionInfo,
    chest::{ChestAdditionInfo, ChestRemovalInfo},
    inserter::{InserterAdditionInfo, conn::Conn},
    power_pole::{
        AUTOMATIC_POLE_CONNECTION_LIMIT, EntityPowerPoleTransfer, PowerPoleAdditionInfo,
        PowerPoleRemovalInfo, PowerPoleTransfer,
    },
};
use middle_indices::ChestMiddleID;
use smallvec::SmallVec;
use thiserror::Error;

use crate::surface::world::{CanFitError, SurfaceWorld};

mod belt_logic;
mod inserter_logic;
mod invariants;
mod pipe_logic;
mod power_pole_logic;
mod world;

// TODO: This should probably not live in the frontend IMO
#[derive(Debug, Clone)]
pub struct Surface {
    floor_chests: BTreeMap<Position, ChestMiddleID>,

    world: SurfaceWorld,
    middle: Middle,
    backend: Backend,
}

#[derive(Debug, Error)]
pub enum PlaceEntityError {
    #[error("Entity may not be rotated")]
    RotationForbidden,
    #[error("Entity may not be flipped")]
    FlippingForbidden,
    #[error("Entity does not fit")]
    CanFit(CanFitError),
    #[error("Entity may on be placed on")]
    FloorRule(!),
    #[error("Cannot mix fluids")]
    PipeFluidMixing(!),
}

#[derive(Debug, Error)]
pub enum RemoveEntityError {
    #[error("No Entity at position")]
    NoEntity(Position),
}

pub struct SurfaceCreationOptions {
    pub generated_area: BoundingBox,
}

impl Surface {
    #[must_use]
    pub fn new(options: &SurfaceCreationOptions) -> Self {
        let mut backend = Backend::new();

        Self {
            floor_chests: BTreeMap::new(),

            world: SurfaceWorld::new_with_empty_area(options.generated_area),
            middle: Middle::new(&mut backend),
            backend,
        }
    }

    pub fn get_entity_states_in_area(&self, area: BoundingBox) -> impl Iterator<Item = EntityInfo> {
        self.world
            .get_entities_in_area(area)
            .map(|desc| EntityInfo {
                position: desc.position,
                rotation: desc.rotation,
                flipped: desc.flipped,
                kind: match desc.kind {
                    EntityDescriptorKind::Assembler { id } => EntityInfoKind::Assembler {
                        ty: desc.ty.try_into().expect("Assembler with non AssemblerTy"),
                        middle_id: id,
                    },
                    EntityDescriptorKind::Inserter { id } => EntityInfoKind::Inserter {
                        ty: desc.ty.try_into().expect("Inserter with non InserterTy"),
                        middle_id: id,
                    },
                    EntityDescriptorKind::Belt { id } => EntityInfoKind::Belt {
                        ty: desc.ty.try_into().expect("Belt with non BeltTy"),
                        middle_id: id,
                    },
                    EntityDescriptorKind::Pipe { id: _ } => todo!(),
                    EntityDescriptorKind::Chest { id } => EntityInfoKind::Chest {
                        ty: desc.ty.try_into().expect("Chest with non ChestTy"),
                        middle_id: id,
                    },
                    EntityDescriptorKind::PowerPole { id } => EntityInfoKind::PowerPole {
                        ty: desc.ty.try_into().expect("PowerPole with non PowerPoleTy"),
                        connected_pole_positions: self
                            .middle
                            .get_pole_connected_positions(id)
                            .collect(),
                        middle_id: id,
                    },
                    EntityDescriptorKind::SolarPanel {} => todo!(),
                },
            })
    }

    pub(crate) fn get_entity_at(&self, position: Position) -> Option<EntityDescriptor> {
        self.world
            .get_entities_in_area(BoundingBox::new(position, Extent::single_tile()))
            .next()
    }

    fn follows_rules(
        &self,
        ty: GlobalTy,
        top_left: Position,
        rotation: Rotation,
        flipped: Flipped,
    ) -> Result<BoundingBox, PlaceEntityError> {
        if rotation != Rotation::North && !allows_rotation(ty) {
            return Err(PlaceEntityError::RotationForbidden);
        }

        if flipped != Flipped::unflipped() && !allows_flipping(ty) {
            return Err(PlaceEntityError::FlippingForbidden);
        }

        let bounding_box = bounding_box(ty, top_left, rotation, flipped);

        if let Err(err) = self.world.can_fit(bounding_box) {
            // Cannot fit
            return Err(PlaceEntityError::CanFit(err));
        }

        // let placement_legal: bool =
        //     placement_allowed(ty, todo!("Get the floor from the world"));

        // if !placement_legal {
        //     return Err(PlaceEntityError::FloorRule(todo!()));
        // }

        Ok(bounding_box)
    }

    /// # Errors
    /// If placing this entity is not legal
    pub fn add_assembler(
        &mut self,
        ty: AssemblerTy,
        top_left: Position,
        rotation: Rotation,
        flipped: Flipped,
        recipe: Option<Recipe>,
    ) -> Result<(), PlaceEntityError> {
        log::trace!("Add assembler with ty {ty:?} at {top_left:?}");
        let bounding_box = self.follows_rules(ty.into(), top_left, rotation, flipped)?;

        let recipe = recipe.unwrap_or_else(|| default_recipe(ty, None));

        // let connected_pipes: Vec<(!, !)> = todo!("Get pipe connections");

        // // Ensure there are no illegal pipe connections
        // for (assembler_conn, pipe_network) in &connected_pipes {
        //     if assembler_conn != pipe_network {
        //         return Err(PlaceEntityError::PipeFluidMixing(todo!()));
        //     }
        // }

        // Placement is allowed. Do the placing

        // let power_grid = (todo!("Find grid") as Option<_>).unwrap_or(0);
        // let connected_inserters: Vec<!> = todo!();

        let pole = self.world.get_pole_for_entity_bounding_box(bounding_box);

        let middle_assembler_id = self
            .middle
            .add_assembler(&AssemblerAdditionInfo { recipe, pole }, &mut self.backend);

        self.world.add_entity(EntityDescriptor {
            position: top_left,
            rotation,
            flipped,
            ty: ty.into(),
            kind: EntityDescriptorKind::Assembler {
                id: middle_assembler_id,
            },
        });

        self.check_invariants();

        Ok(())
    }

    /// # Errors
    /// If placing this entity is not legal
    pub fn add_chest(
        &mut self,
        ty: ChestTy,
        top_left: Position,
        rotation: Rotation,
        flipped: Flipped,
    ) -> Result<(), PlaceEntityError> {
        log::trace!("Add chest with ty {ty:?} at {top_left:?}");
        let _bounding_box = self.follows_rules(ty.into(), top_left, rotation, flipped)?;

        // Placement is allowed. Do the placing

        // let connected_inserters: Vec<!> = todo!();

        let middle_chest_id = self.middle.add_chest(
            &ChestAdditionInfo {
                num_slots: num_slots(ty),
            },
            &mut self.backend,
        );

        self.world.add_entity(EntityDescriptor {
            position: top_left,
            rotation,
            flipped,
            ty: ty.into(),
            kind: EntityDescriptorKind::Chest {
                id: middle_chest_id,
            },
        });

        self.check_invariants();

        Ok(())
    }

    /// # Errors
    /// If placing this entity is not legal
    pub fn add_inserter(
        &mut self,
        ty: InserterTy,
        top_left: Position,
        rotation: Rotation,
        flipped: Flipped,
    ) -> Result<(), PlaceEntityError> {
        log::trace!("Add inserter with ty {ty:?} at {top_left:?}");
        let bounding_box = self.follows_rules(ty.into(), top_left, rotation, flipped)?;

        // Placement is allowed. Do the placing

        // TODO: Get power grid

        let power_pole = self.world.get_pole_for_entity_bounding_box(bounding_box);

        let source_pos = get_input_position(ty, top_left, rotation, flipped);
        let dest_pos = get_output_position(ty, top_left, rotation, flipped);

        assert_ne!(source_pos, dest_pos);

        let source_conn = self.get_source_conns_or_add_floor_conn(source_pos);
        let dest_conn = self.get_dest_conns_or_add_floor_conn(dest_pos);

        let middle_chest_id = self.middle.add_inserter(
            InserterAdditionInfo {
                power_pole,
                sources: source_conn,
                dest: dest_conn,
                item_filter: LimitedItemSet::All,
                // TODO:
                movetime: 100,
            },
            &mut self.backend,
        );

        self.world.add_entity(EntityDescriptor {
            position: top_left,
            rotation,
            flipped,
            ty: ty.into(),
            kind: EntityDescriptorKind::Inserter {
                id: middle_chest_id,
            },
        });

        self.check_invariants();

        Ok(())
    }

    /// # Errors
    /// If placing this entity is not legal
    pub fn add_belt(
        &mut self,
        ty: BeltTy,
        position: Position,
        rotation: Rotation,
        flipped: Flipped,
    ) -> Result<(), PlaceEntityError> {
        log::trace!("Add belt with ty {ty:?} at {position:?}");
        let _bounding_box = self.follows_rules(ty.into(), position, rotation, flipped)?;

        // Placement is allowed. Do the placing

        let front_merge = self.get_front_merge_belt(ty, position, rotation, flipped);
        let back_merge = self.get_back_belt_merge(ty, position, rotation, flipped);

        let middle_belt_tile_id = self.middle.add_belt_tile(
            &BeltTileAdditionInfo {
                // TODO: Length adjustable?
                length: 4,
                front_merge,
                back_merge,
                // TODO:
                left_sideload_source: None,
                right_sideload_source: None,

                // TODO
                attached_inserters: vec![],
            },
            &mut self.backend,
        );

        self.world.add_entity(EntityDescriptor {
            position,
            rotation,
            flipped,
            ty: ty.into(),
            kind: EntityDescriptorKind::Belt {
                id: middle_belt_tile_id,
            },
        });

        self.check_invariants();

        Ok(())
    }

    /// # Errors
    /// If placing this entity is not legal
    pub fn add_power_pole(
        &mut self,
        ty: PowerPoleTy,
        top_left: Position,
        rotation: Rotation,
        flipped: Flipped,
    ) -> Result<(), PlaceEntityError> {
        log::trace!("Add power_pole with ty {ty:?} at {top_left:?}");
        let bounding_box = self.follows_rules(ty.into(), top_left, rotation, flipped)?;

        let mut connected_poles: SmallVec<_> = SmallVec::default();
        for conn in self
            .world
            .get_power_poles_overlapping(power_pole_wire_connection_area(
                ty, top_left, rotation, flipped,
            ))
            .filter(|other| {
                let other_connection_area = power_pole_wire_connection_area(
                    other
                        .ty
                        .try_into()
                        .expect("get_power_poles_in_area returned non PowerPole"),
                    other.position,
                    other.rotation,
                    other.flipped,
                );

                other_connection_area.overlaps(bounding_box)
            })
            // FIXME: Manhatten is prob wrong, and using top_left is for sure wrong
            // .sorted_by_key(|e| top_left.manhattan_distance(e.position))
            .map(|e| {
                let EntityDescriptorKind::PowerPole { id } = e.kind else {
                    unreachable!()
                };
                id
            })
            .filter(|id| self.middle.get_num_connected_poles(*id) < AUTOMATIC_POLE_CONNECTION_LIMIT)
            .take(AUTOMATIC_POLE_CONNECTION_LIMIT)
        {
            if connected_poles
                .iter()
                .all(|already| !self.middle.are_poles_connected([*already, conn]))
            {
                // This will not form a triangle
                connected_poles.push(conn);
            }

            assert!(connected_poles.len() <= connected_poles.inline_size());
        }

        let connected_entities = self
            .world
            .get_powered_entites_for_pole(
                top_left,
                power_pole_supply_area(ty, top_left, rotation, flipped),
            )
            .map(|entity| PowerPoleTransfer {
                entity,
                prev_pole: self
                    .world
                    .get_pole_for_entity_bounding_box(entity.bounding_box()),
            });

        let index = self.middle.add_power_pole(
            PowerPoleAdditionInfo {
                position: top_left,
                connections: connected_poles,
                connected_entities,
            },
            &mut self.backend,
        );

        self.world.add_entity(EntityDescriptor {
            position: top_left,
            rotation,
            flipped,
            ty: ty.into(),
            kind: EntityDescriptorKind::PowerPole { id: index },
        });

        self.check_invariants();

        Ok(())
    }

    pub fn remove_entity_at(&mut self, position: Position) -> Result<(), RemoveEntityError> {
        self.check_invariants();

        let Some(entity) = self.world.remove_entity_at(position) else {
            return Err(RemoveEntityError::NoEntity(position));
        };

        match entity.kind {
            EntityDescriptorKind::Assembler { id } => {
                let inserters = self.world.get_inserters_connected_to(bounding_box(
                    entity.ty,
                    entity.position,
                    entity.rotation,
                    entity.flipped,
                ));

                let inserter_changes = inserters
                    .map(|(id, source, dest)| InserterTransfer {
                        id,
                        sources: source.map(|pos| {
                            vec![match self.floor_chests.entry(pos) {
                                Entry::Vacant(vacant_entry) => {
                                    let floor_chest_id = self.middle.add_chest(
                                        &ChestAdditionInfo { num_slots: 1 },
                                        &mut self.backend,
                                    );

                                    vacant_entry.insert(floor_chest_id);

                                    Conn::Chest { id: floor_chest_id }
                                },
                                Entry::Occupied(occupied_entry) => Conn::Chest {
                                    id: *occupied_entry.get(),
                                },
                            }]
                        }),

                        dest: dest.map(|pos| match self.floor_chests.entry(pos) {
                            Entry::Vacant(vacant_entry) => {
                                let floor_chest_id = self.middle.add_chest(
                                    &ChestAdditionInfo { num_slots: 1 },
                                    &mut self.backend,
                                );

                                vacant_entry.insert(floor_chest_id);

                                Some(Conn::Chest { id: floor_chest_id })
                            },
                            Entry::Occupied(occupied_entry) => Some(Conn::Chest {
                                id: *occupied_entry.get(),
                            }),
                        }),
                    })
                    .collect_vec();

                let pole = self
                    .world
                    .get_pole_for_entity_bounding_box(entity.bounding_box());

                let info = self.middle.remove_assembler(
                    AssemblerRemovalInfo {
                        id,
                        inserter_changes,
                        pole,
                    },
                    &mut self.backend,
                );
            },
            EntityDescriptorKind::Inserter { id } => {
                let pole = self
                    .world
                    .get_pole_for_entity_bounding_box(entity.bounding_box());
                let info = self.middle.remove_inserter(id, pole, &mut self.backend);
            },
            EntityDescriptorKind::Belt { id } => {
                // FIXME:
            },
            EntityDescriptorKind::Pipe { id } => todo!(),
            EntityDescriptorKind::PowerPole { id } => {
                let previously_connected_entities = self.world.get_powered_entites_for_pole(
                    entity.position,
                    power_pole_supply_area(
                        entity
                            .ty
                            .try_into()
                            .expect("Power Pole with non PowerPoleTy"),
                        entity.position,
                        entity.rotation,
                        entity.flipped,
                    ),
                );

                let transfers = previously_connected_entities.map(|entity| {
                    let new_pole = self.world.get_pole_for_entity_bounding_box(bounding_box(
                        entity.ty,
                        entity.position,
                        entity.rotation,
                        entity.flipped,
                    ));

                    EntityPowerPoleTransfer { entity, new_pole }
                });

                self.middle
                    .remove_power_pole(PowerPoleRemovalInfo { id, transfers }, &mut self.backend);
            },
            EntityDescriptorKind::Chest { id } => {
                let inserters = self.world.get_inserters_connected_to(bounding_box(
                    entity.ty,
                    entity.position,
                    entity.rotation,
                    entity.flipped,
                ));

                let inserter_changes = inserters
                    .map(|(id, source, dest)| InserterTransfer {
                        id,
                        sources: source.map(|pos| {
                            vec![match self.floor_chests.entry(pos) {
                                Entry::Vacant(vacant_entry) => {
                                    let floor_chest_id = self.middle.add_chest(
                                        &ChestAdditionInfo { num_slots: 1 },
                                        &mut self.backend,
                                    );

                                    vacant_entry.insert(floor_chest_id);

                                    Conn::Chest { id: floor_chest_id }
                                },
                                Entry::Occupied(occupied_entry) => Conn::Chest {
                                    id: *occupied_entry.get(),
                                },
                            }]
                        }),
                        dest: dest.map(|pos| match self.floor_chests.entry(pos) {
                            Entry::Vacant(vacant_entry) => {
                                let floor_chest_id = self.middle.add_chest(
                                    &ChestAdditionInfo { num_slots: 1 },
                                    &mut self.backend,
                                );

                                vacant_entry.insert(floor_chest_id);

                                Some(Conn::Chest { id: floor_chest_id })
                            },
                            Entry::Occupied(occupied_entry) => Some(Conn::Chest {
                                id: *occupied_entry.get(),
                            }),
                        }),
                    })
                    .collect_vec();

                self.middle.remove_chest(
                    ChestRemovalInfo {
                        id,
                        inserter_changes,
                    },
                    &mut self.backend,
                );
            },
            EntityDescriptorKind::SolarPanel {} => todo!(),
        }

        self.check_invariants();

        Ok(())
    }
}
