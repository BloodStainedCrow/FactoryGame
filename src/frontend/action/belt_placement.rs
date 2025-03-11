use std::mem;

use log::warn;
use strum::IntoEnumIterator;

use crate::{
    belt::{
        belt::Belt,
        smart::{EmptyBelt, Side, SmartBelt},
    },
    data::DataStore,
    frontend::world::{
        tile::{BeltTileId, Dir, DirRelative, Entity, World, BELT_LEN_PER_TILE},
        Position,
    },
    item::{IdxTrait, WeakIdxTrait},
    rendering::app_state::{GameState, SimulationState},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BeltState {
    Straight,
    Curved,
    Sideloading,
    DoubleSideloading,
}

#[allow(unreachable_code)]
pub fn handle_belt_placement<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
    new_belt_pos: Position,
    new_belt_direction: Dir,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> (BeltTileId<ItemIdxType>, u16) {
    let our_expected_state = expected_belt_state(new_belt_direction, |dir| {
        game_state.world.get_belt_possible_inputs(new_belt_pos)[*dir]
    });

    let front_belt_dir = get_belt_dir(
        &game_state.world,
        new_belt_pos + new_belt_direction,
        data_store,
    );

    let front_current_state = front_belt_dir.map(|front_belt_dir| {
        expected_belt_state(front_belt_dir, |dir| {
            game_state
                .world
                .get_belt_possible_inputs(new_belt_pos + new_belt_direction)[*dir]
        })
    });

    let front_expected_state = front_belt_dir.map(|front_belt_dir| {
        expected_belt_state(front_belt_dir, |dir| {
            if *dir == new_belt_direction.reverse() {
                true
            } else {
                game_state
                    .world
                    .get_belt_possible_inputs(new_belt_pos + new_belt_direction)[*dir]
            }
        })
    });

    assert_eq!(
        front_expected_state.is_some(),
        front_current_state.is_some()
    );

    dbg!(our_expected_state);
    dbg!(front_expected_state);

    let back_dir = match our_expected_state {
        BeltState::Straight | BeltState::Sideloading | BeltState::DoubleSideloading => {
            new_belt_direction.reverse()
        },
        BeltState::Curved => game_state
            .world
            .get_belt_possible_inputs(new_belt_pos)
            .iter()
            .filter_map(|v| v.1.then_some(v.0))
            .find(|v| v.compare(new_belt_direction) == DirRelative::Turned)
            .unwrap(),
    };

    // If we get input from the back, extend it
    let (new_belt_id, new_belt_len, new_belt_belt_pos) =
        if game_state.world.get_belt_possible_inputs(new_belt_pos)[back_dir] {
            let back_belt_entity = game_state
                .world
                .get_entities_colliding_with(new_belt_pos + back_dir, (1, 1), data_store)
                .into_iter()
                .next();

            let back_belt_id = if let Some(back_belt_entity) = back_belt_entity {
                match back_belt_entity {
                    Entity::Belt {
                        pos,
                        direction,
                        id,
                        belt_pos,
                    } => {
                        let id = *id;

                        Some(id)
                    },
                    Entity::Inserter { .. } => None,
                    Entity::Assembler { .. } => None,
                    Entity::PowerPole { .. } => None,
                }
            } else {
                None
            }
            .expect("We are recieving input from the back, but could not find belt_id???");

            game_state
                .world
                .modify_belt_pos(back_belt_id, BELT_LEN_PER_TILE.try_into().unwrap());

            let new_belt = EmptyBelt::new(BELT_LEN_PER_TILE);

            let (new_belt_id, new_belt_len) = match back_belt_id {
                BeltTileId::EmptyBeltId(idx) => {
                    take_mut::take(
                        &mut game_state.simulation_state.factory.belts.empty_belts[idx],
                        |back_belt| EmptyBelt::join(new_belt, back_belt),
                    );

                    (
                        BeltTileId::EmptyBeltId(idx),
                        game_state.simulation_state.factory.belts.empty_belts[idx].len,
                    )
                },
                BeltTileId::BeltId(belt_id) => {
                    let belt_item_id: usize = belt_id.item.id.into();
                    let back_belt = game_state.simulation_state.factory.belts.belts[belt_item_id]
                        .belts
                        .get_mut(belt_id.index)
                        .expect("id from world not in simulation!!");

                    take_mut::take(back_belt, |back_belt| {
                        back_belt.join_with_empty(new_belt, Side::FRONT)
                    });

                    (BeltTileId::BeltId(belt_id), back_belt.get_len())
                },
            };

            (Some(new_belt_id), new_belt_len, BELT_LEN_PER_TILE)
        } else {
            (None, BELT_LEN_PER_TILE, BELT_LEN_PER_TILE)
        };

    // Handle sideloading onto us
    match our_expected_state {
        BeltState::Straight | BeltState::Curved => {},
        BeltState::Sideloading | BeltState::DoubleSideloading => todo!(),
    }

    // Handle interactions with the belt infront of us
    let (final_belt_id, final_belt_len) = if let Some(front_belt_dir) = front_belt_dir {
        let front_expected_state = front_expected_state.unwrap();

        let front_belt_entity = game_state
            .world
            .get_entities_colliding_with(new_belt_pos + new_belt_direction, (1, 1), data_store)
            .into_iter()
            .next();

        let (front_belt_id, front_belt_pos) = if let Some(front_belt_entity) = front_belt_entity {
            match front_belt_entity {
                Entity::Belt {
                    pos,
                    direction,
                    id,
                    belt_pos,
                } => {
                    let id = *id;

                    Some((id, *belt_pos))
                },
                Entity::Inserter { .. } => None,
                Entity::Assembler { .. } => None,
                Entity::PowerPole { .. } => None,
            }
        } else {
            None
        }
        .unwrap();

        let (final_belt_id, final_belt_len) = match front_belt_dir.compare(new_belt_direction) {
            DirRelative::SameDir => {
                let (final_belt_id, final_belt_len) = attach_to_back_of_belt(
                    game_state,
                    front_belt_id,
                    new_belt_id,
                    new_belt_len,
                    data_store,
                );
                (Some(final_belt_id), final_belt_len)
            },
            DirRelative::Turned => {
                // Either Sideload onto the front belt or merge with it if it is turned
                match front_expected_state {
                    BeltState::Straight => unreachable!(),
                    BeltState::Curved => {
                        let (final_belt_id, final_belt_len) = attach_to_back_of_belt(
                            game_state,
                            front_belt_id,
                            new_belt_id,
                            new_belt_len,
                            data_store,
                        );
                        (Some(final_belt_id), final_belt_len)
                    },
                    BeltState::Sideloading | BeltState::DoubleSideloading => {
                        // TODO: Handle out sideloading, and potentially rip apart a turn that existed before!
                        todo!()
                    },
                }
            },
            DirRelative::Opposite => {
                // Do nothing, if belts face head on they do not interact
                (new_belt_id, new_belt_len)
            },
        };

        (final_belt_id, final_belt_len)
    } else {
        (new_belt_id, new_belt_len)
    };

    if let Some(final_belt_id) = final_belt_id {
        (final_belt_id, new_belt_belt_pos)
    } else {
        // This belt is not in the simulation, add it
        let new_belt = EmptyBelt::new(final_belt_len);

        let idx = game_state.simulation_state.factory.belts.empty_belts.len();
        game_state
            .simulation_state
            .factory
            .belts
            .empty_belts
            .push(new_belt);

        (BeltTileId::EmptyBeltId(idx), new_belt_belt_pos)
    }
}

fn attach_to_back_of_belt<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
    front_belt_id: BeltTileId<ItemIdxType>,
    new_belt_id: Option<BeltTileId<ItemIdxType>>,
    new_belt_len: u16,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> (BeltTileId<ItemIdxType>, u16) {
    game_state.world.modify_belt_pos(
        front_belt_id,
        new_belt_len
            .try_into()
            .expect("FIXME: This could fail without a belt being too long!"),
    );

    let (final_belt_id, final_belt_len) = match new_belt_id {
        Some(new_belt_id_in_sim) => {
            let (final_belt_id, final_belt_len) = merge_belts(
                &mut game_state.simulation_state,
                front_belt_id,
                new_belt_id_in_sim,
                data_store,
            );

            if final_belt_id == front_belt_id {
                game_state
                    .world
                    .update_belt_id(new_belt_id_in_sim, final_belt_id);
            } else {
                game_state
                    .world
                    .update_belt_id(final_belt_id, new_belt_id_in_sim);
            }

            (final_belt_id, final_belt_len)
        },
        None => {
            // We did not merge with a back belt (i.e. the new belt does not exist in the simulation), so we just add this to the front belt
            let new_belt = EmptyBelt::new(new_belt_len);
            match front_belt_id {
                BeltTileId::EmptyBeltId(idx) => {
                    take_mut::take(
                        &mut game_state.simulation_state.factory.belts.empty_belts[idx],
                        |back_belt| EmptyBelt::join(new_belt, back_belt),
                    );

                    (
                        BeltTileId::EmptyBeltId(idx),
                        game_state.simulation_state.factory.belts.empty_belts[idx].len,
                    )
                },
                BeltTileId::BeltId(belt_id) => {
                    let belt_item_id: usize = belt_id.item.id.into();
                    let back_belt = game_state.simulation_state.factory.belts.belts[belt_item_id]
                        .belts
                        .get_mut(belt_id.index)
                        .expect("id from world not in simulation!!");

                    take_mut::take(back_belt, |back_belt| {
                        back_belt.join_with_empty(new_belt, Side::BACK)
                    });

                    (BeltTileId::BeltId(belt_id), back_belt.get_len())
                },
            }
        },
    };

    (final_belt_id, final_belt_len)
}

fn merge_belts<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    simulation_state: &mut SimulationState<RecipeIdxType>,
    front_tile_id: BeltTileId<ItemIdxType>,
    back_tile_id: BeltTileId<ItemIdxType>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> (BeltTileId<ItemIdxType>, u16) {
    if front_tile_id == back_tile_id {
        todo!("Currently I do not handle loops correctly")
    }

    let (final_len, final_id) = match (front_tile_id, back_tile_id) {
        (BeltTileId::EmptyBeltId(front_idx), BeltTileId::EmptyBeltId(back_idx)) => {
            // Remove back belt from simulation
            let mut back_belt = EmptyBelt::new(0);

            mem::swap(
                &mut back_belt,
                &mut simulation_state.factory.belts.empty_belts[back_idx],
            );

            let back_len = back_belt.len;

            simulation_state
                .factory
                .belts
                .empty_belt_holes
                .push(back_idx);

            // Get ref to front belt
            let front_belt = simulation_state
                .factory
                .belts
                .empty_belts
                .get_mut(front_idx)
                .expect("id from world not in simulation!!");

            let front_len = front_belt.len;

            // Merge front and middle and back
            take_mut::take(front_belt, |front_belt| {
                EmptyBelt::join(front_belt, back_belt)
            });

            (front_len + back_len, front_tile_id)
        },
        (BeltTileId::EmptyBeltId(front_idx), BeltTileId::BeltId(back_id)) => {
            // Remove front belt from simulation
            let mut front_belt = EmptyBelt::new(0);

            mem::swap(
                &mut front_belt,
                &mut simulation_state.factory.belts.empty_belts[front_idx],
            );

            let back_item_id: usize = back_id.item.id.into();

            // Get ref to back belt
            let back_belt = simulation_state.factory.belts.belts[back_item_id]
                .belts
                .get_mut(back_id.index)
                .expect("id from world not in simulation!!");

            let back_len = back_belt.get_len();

            let front_len = front_belt.len;

            // Merge front and middle and back
            take_mut::take(back_belt, |back_belt| {
                back_belt.join_with_empty(front_belt, Side::FRONT)
            });

            (front_len + back_len, back_tile_id)
        },
        (BeltTileId::BeltId(front_id), BeltTileId::EmptyBeltId(back_idx)) => {
            // Remove back belt from simulation
            let mut back_belt = EmptyBelt::new(0);

            mem::swap(
                &mut back_belt,
                &mut simulation_state.factory.belts.empty_belts[back_idx],
            );

            let back_len = back_belt.len;

            simulation_state
                .factory
                .belts
                .empty_belt_holes
                .push(back_idx);

            let front_item_id: usize = front_id.item.id.into();

            // Get ref to front belt
            let front_belt = simulation_state.factory.belts.belts[front_item_id]
                .belts
                .get_mut(front_id.index)
                .expect("id from world not in simulation!!");

            let front_len = front_belt.get_len();

            // Merge front and middle and back
            take_mut::take(front_belt, |front_belt| {
                front_belt.join_with_empty(back_belt, Side::BACK)
            });

            (front_len + back_len, front_tile_id)
        },
        (BeltTileId::BeltId(front_id), BeltTileId::BeltId(back_id)) => {
            // Remove back belt from simulation
            let mut back_belt = SmartBelt::new(0);
            let back_item_id: usize = back_id.item.id.into();
            let front_item_id: usize = front_id.item.id.into();

            if front_item_id != back_item_id {
                // TODO: Try to downgrade the belt if it is empty
                warn!("Two belts filled with different items cannot be connected");
                panic!();
            }

            let back_len = back_belt.get_len();

            mem::swap(
                &mut back_belt,
                &mut simulation_state.factory.belts.belts[back_item_id].belts[back_id.index],
            );

            simulation_state.factory.belts.belts[back_item_id]
                .holes
                .push(back_id.index);

            // Get ref to front belt
            let front_belt = simulation_state.factory.belts.belts[front_item_id]
                .belts
                .get_mut(front_id.index)
                .expect("id from world not in simulation!!");

            let front_len = front_belt.get_len();

            // Merge front and middle and back
            take_mut::take(front_belt, |front_belt| {
                SmartBelt::join(front_belt, back_belt)
            });

            (front_len + back_len, front_tile_id)
        },
    };

    (final_id, final_len)
}

fn get_belt_dir<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &World<ItemIdxType, RecipeIdxType>,
    position: Position,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> Option<Dir> {
    world
        .get_entities_colliding_with(position, (1, 1), data_store)
        .into_iter()
        .next()
        .map(|e| match e {
            Entity::Belt { direction, .. } => Some(*direction),
            _ => None,
        })
        .flatten()
}

fn expected_belt_state(belt_dir: Dir, gets_input_from: impl FnMut(&Dir) -> bool) -> BeltState {
    let input_dirs: Vec<Dir> = Dir::iter().filter(gets_input_from).collect();
    // Output dirs are unused for determining this, interesting!
    // let output_dirs: Vec<Dir> = Dir::iter().filter(|dir| dir_info(*dir) == Some(BeltDir::Ouput)).collect();

    dbg!(&input_dirs);

    match input_dirs.len() {
        0 => BeltState::Straight,

        1 => {
            if input_dirs[0] == belt_dir || input_dirs[0] == belt_dir.reverse() {
                BeltState::Straight
            } else {
                BeltState::Curved
            }
        },

        2 => {
            if input_dirs[0] == input_dirs[1].reverse() {
                // The inputs are opposite
                if input_dirs[0] == belt_dir || input_dirs[0] == belt_dir.reverse() {
                    // The inputs are front and back
                    BeltState::Straight
                } else {
                    // The inputs are left and right
                    BeltState::DoubleSideloading
                }
            } else {
                // The inputs are at 90 deg

                if input_dirs[0] == belt_dir.reverse() || input_dirs[1] == belt_dir.reverse() {
                    // One of the belts points into our input dir
                    BeltState::Sideloading
                } else {
                    BeltState::Curved
                }
            }
        },

        3 => {
            if !input_dirs.contains(&belt_dir) || !input_dirs.contains(&belt_dir.reverse()) {
                BeltState::DoubleSideloading
            } else {
                BeltState::Sideloading
            }
        },

        4 => BeltState::DoubleSideloading,

        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {}
