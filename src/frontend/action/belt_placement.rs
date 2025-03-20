use std::mem;

use itertools::Itertools;
use log::{error, warn};
use strum::IntoEnumIterator;

use crate::{
    belt::{
        belt::Belt,
        smart::{EmptyBelt, Side, SmartBelt},
        splitter::{Splitter, SplitterDistributionMode, SPLITTER_BELT_LEN},
    },
    data::DataStore,
    frontend::world::{
        tile::{BeltTileId, Dir, DirRelative, Entity, UndergroundDir, World, BELT_LEN_PER_TILE},
        Position,
    },
    item::{IdxTrait, WeakIdxTrait},
    rendering::app_state::{BeltBeltInserterAdditionInfo, GameState, SimulationState},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BeltState {
    Straight,
    Curved,
    Sideloading,
    DoubleSideloading,
}

pub fn handle_splitter_placement<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
    splitter_pos: Position,
    splitter_direction: Dir,
    in_mode: Option<SplitterDistributionMode>,
    out_mode: Option<SplitterDistributionMode>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    let self_positions = [splitter_pos, splitter_pos + splitter_direction.turn_right()];

    let belt_connections: [[BeltTileId<ItemIdxType>; 2]; 2] = self_positions.map(|self_pos| {
        let front_pos = self_pos + splitter_direction;
        handle_belt_breaking(game_state, front_pos, splitter_direction, data_store);

        // Handle front
        let (self_front_id, self_front_len) = if let Some(id) =
            should_merge(game_state, splitter_direction, front_pos, data_store)
        {
            debug_assert!(
                should_sideload(game_state, splitter_direction, front_pos, data_store).is_none()
            );
            let result = lengthen(game_state, id, SPLITTER_BELT_LEN, Side::BACK);
            (id, result.new_belt_len)
        } else if let Some((id, pos)) =
            should_sideload(game_state, splitter_direction, front_pos, data_store)
        {
            // Add Sideloading inserter
            let new_id = match id {
                BeltTileId::EmptyBeltId(_) => todo!("Handle Sideloading on empty belts"),
                BeltTileId::BeltId(dest_id) => {
                    let new_id = game_state
                        .simulation_state
                        .factory
                        .belts
                        .add_belt(dest_id.item, SmartBelt::new(SPLITTER_BELT_LEN));
                    game_state.simulation_state.factory.add_belt_belt_inserter(
                        (new_id, 0),
                        (dest_id, pos),
                        BeltBeltInserterAdditionInfo { cooldown: 0 },
                    );
                    BeltTileId::BeltId(new_id)
                },
            };
            (new_id, SPLITTER_BELT_LEN)
        } else {
            let id = game_state
                .simulation_state
                .factory
                .belts
                .add_empty_belt(SPLITTER_BELT_LEN);
            (id, SPLITTER_BELT_LEN)
        };

        let back_pos = self_pos + splitter_direction.reverse();
        let belt_dir = get_belt_out_dir(&game_state.world, back_pos, data_store);
        let (self_back_id, self_back_len) = if let Some(belt_dir) = belt_dir {
            if belt_dir == splitter_direction {
                // The belt at this position is pointing at the back of the splitter
                let back_id = match game_state
                    .world
                    .get_entities_colliding_with(back_pos, (1, 1), data_store)
                    .into_iter()
                    .next()
                    .unwrap()
                {
                    Entity::Belt { id, .. } => *id,
                    Entity::Underground {
                        underground_dir: UndergroundDir::Entrance,
                        id,
                        ..
                    } => *id,
                    Entity::Splitter { .. } => todo!("get the id from the simstate"),
                    _ => unreachable!(),
                };

                let result = lengthen(game_state, back_id, SPLITTER_BELT_LEN, Side::FRONT);

                (back_id, result.new_belt_len)
            } else {
                let id = game_state
                    .simulation_state
                    .factory
                    .belts
                    .add_empty_belt(SPLITTER_BELT_LEN);
                (id, SPLITTER_BELT_LEN)
            }
        } else {
            let id = game_state
                .simulation_state
                .factory
                .belts
                .add_empty_belt(SPLITTER_BELT_LEN);
            (id, SPLITTER_BELT_LEN)
        };

        [self_front_id, self_back_id]
    });

    let item = match belt_connections
        .iter()
        .flatten()
        .filter_map(|b| match b {
            BeltTileId::EmptyBeltId(_) => None,
            BeltTileId::BeltId(belt_id) => Some(belt_id.item),
        })
        .all_equal_value()
    {
        Ok(item) => Some(item),
        Err(None) => None,
        Err(Some(mismatched)) => {
            error!("Cannot merge belts carrying different items");
            todo!("Cannot merge belts carrying different items")
        },
    };

    let belt_connections = belt_connections.map(|tile_id| match tile_id {
        [BeltTileId::EmptyBeltId(front_idx), BeltTileId::EmptyBeltId(back_idx)] => match item {
            Some(item) => todo!("Instantiate the belt"),
            None => [front_idx, back_idx],
        },
        [BeltTileId::EmptyBeltId(_), BeltTileId::BeltId(belt_id)] => todo!("Instantiate the belt"),
        [BeltTileId::BeltId(belt_id_front), BeltTileId::EmptyBeltId(_)] => {
            todo!("Instantiate the belt")
        },
        [BeltTileId::BeltId(belt_id_front), BeltTileId::BeltId(belt_id_back)] => {
            [belt_id_back.index, belt_id_front.index]
        },
    });

    let splitter = Splitter {
        in_mode: in_mode.unwrap_or_default(),
        out_mode: out_mode.unwrap_or_default(),
        input_belts: [belt_connections[0][0], belt_connections[1][0]],
        output_belts: [belt_connections[0][1], belt_connections[1][1]],
    };

    let id = game_state
        .simulation_state
        .factory
        .add_splitter(splitter, item);

    game_state.world.add_entity(
        Entity::Splitter {
            pos: splitter_pos,
            direction: splitter_direction,
            item,
            id,
        },
        &game_state.simulation_state,
        data_store,
    );
}

#[allow(unreachable_code)]
pub fn handle_belt_placement<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
    new_belt_pos: Position,
    new_belt_direction: Dir,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    let front_pos = new_belt_pos + new_belt_direction;

    handle_belt_breaking(game_state, front_pos, new_belt_direction, data_store);

    let (self_id, self_len) =
        if let Some(id) = should_merge(game_state, new_belt_direction, front_pos, data_store) {
            debug_assert!(
                should_sideload(game_state, new_belt_direction, front_pos, data_store).is_none()
            );
            let result = lengthen(game_state, id, BELT_LEN_PER_TILE, Side::BACK);
            game_state.world.add_entity(
                Entity::Belt {
                    pos: new_belt_pos,
                    direction: new_belt_direction,
                    id,
                    belt_pos: result.belt_pos_of_segment,
                },
                &game_state.simulation_state,
                data_store,
            );
            (id, result.new_belt_len)
        } else if let Some((id, pos)) =
            should_sideload(game_state, new_belt_direction, front_pos, data_store)
        {
            // Add Sideloading inserter
            let new_id = match id {
                BeltTileId::EmptyBeltId(_) => todo!("Handle Sideloading on empty belts"),
                BeltTileId::BeltId(dest_id) => {
                    let new_id = game_state
                        .simulation_state
                        .factory
                        .belts
                        .add_belt(dest_id.item, SmartBelt::new(BELT_LEN_PER_TILE));
                    game_state.simulation_state.factory.add_belt_belt_inserter(
                        (new_id, 0),
                        (dest_id, pos),
                        BeltBeltInserterAdditionInfo { cooldown: 0 },
                    );
                    BeltTileId::BeltId(new_id)
                },
            };

            game_state.world.add_entity(
                Entity::Belt {
                    pos: new_belt_pos,
                    direction: new_belt_direction,
                    id: new_id,
                    belt_pos: BELT_LEN_PER_TILE,
                },
                &game_state.simulation_state,
                data_store,
            );
            (new_id, BELT_LEN_PER_TILE)
        } else {
            let id = game_state
                .simulation_state
                .factory
                .belts
                .add_empty_belt(BELT_LEN_PER_TILE);
            game_state.world.add_entity(
                Entity::Belt {
                    pos: new_belt_pos,
                    direction: new_belt_direction,
                    id,
                    belt_pos: BELT_LEN_PER_TILE,
                },
                &game_state.simulation_state,
                data_store,
            );
            (id, BELT_LEN_PER_TILE)
        };

    for dir in Dir::iter() {
        if dir.reverse() == new_belt_direction {
            continue;
        }
        dbg!(dir);
        let potentially_incoming_pos = new_belt_pos + dir.reverse();
        let belt_dir = get_belt_out_dir(&game_state.world, potentially_incoming_pos, data_store);
        if let Some(belt_dir) = belt_dir {
            if belt_dir != dir {
                // The belt at this position is not pointing at the new belt
                continue;
            }

            dbg!(belt_dir);
            if let Some(id) = should_merge(game_state, dir, new_belt_pos, data_store) {
                debug_assert!(should_sideload(game_state, dir, new_belt_pos, data_store).is_none());
                debug_assert_eq!(id, self_id);

                let back_id = match game_state
                    .world
                    .get_entities_colliding_with(potentially_incoming_pos, (1, 1), data_store)
                    .into_iter()
                    .next()
                    .unwrap()
                {
                    Entity::Belt { id, .. }
                    | Entity::Underground {
                        id,
                        underground_dir: UndergroundDir::Exit,
                        ..
                    } => *id,
                    Entity::Splitter { pos, direction, .. } => todo!(),
                    _ => unreachable!(),
                };

                let (final_id, final_len) = merge_belts(
                    &mut game_state.simulation_state,
                    self_id,
                    back_id,
                    data_store,
                );
                // TODO: try_into could fail even if the belt len is fine
                if self_id != back_id {
                    game_state
                        .world
                        .modify_belt_pos(back_id, self_len.try_into().unwrap());
                }
                if final_id == back_id {
                    game_state.world.update_belt_id(self_id, final_id);
                } else {
                    game_state.world.update_belt_id(back_id, final_id);
                }
            } else if let Some((id, belt_pos)) =
                should_sideload(game_state, dir, new_belt_pos, data_store)
            {
                debug_assert_eq!(id, self_id);

                let (back_id, back_pos) = match game_state
                    .world
                    .get_entities_colliding_with(potentially_incoming_pos, (1, 1), data_store)
                    .into_iter()
                    .next()
                    .unwrap()
                {
                    Entity::Belt { id, belt_pos, .. }
                    | Entity::Underground {
                        id,
                        belt_pos,
                        underground_dir: UndergroundDir::Exit,
                        ..
                    } => (*id, *belt_pos),
                    Entity::Splitter { pos, direction, .. } => todo!(),
                    _ => unreachable!(),
                };

                // Add Sideloading inserter
                match id {
                    BeltTileId::EmptyBeltId(_) => todo!("Handle Sideloading on empty belts"),
                    BeltTileId::BeltId(dest_id) => {
                        let BeltTileId::BeltId(back_id) = back_id else {
                            todo!("Handle Sideloading on empty belts")
                        };
                        game_state.simulation_state.factory.add_belt_belt_inserter(
                            (back_id, 0),
                            (dest_id, belt_pos),
                            BeltBeltInserterAdditionInfo { cooldown: 0 },
                        );
                    },
                }
            } else {
                // Do nothing
            }
        }
    }
}

enum BeltChange {
    Added,
    Removed,
}

fn handle_belt_breaking<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
    pos_which_might_break: Position,
    dir_which_the_new_belt_is_pointing: Dir,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    let Some(dir_for_belt_which_might_break) =
        get_belt_dir_for_sideloading(&game_state.world, pos_which_might_break, data_store)
    else {
        return;
    };

    let old_state = expected_belt_state(dir_for_belt_which_might_break, |dir| {
        game_state
            .world
            .get_belt_possible_inputs(pos_which_might_break)[*dir]
    });
    let new_state = expected_belt_state(dir_for_belt_which_might_break, |dir| {
        if *dir == dir_which_the_new_belt_is_pointing.reverse() {
            true
        } else {
            game_state
                .world
                .get_belt_possible_inputs(pos_which_might_break)[*dir]
        }
    });

    match (old_state, new_state) {
        (BeltState::Straight, BeltState::Straight) => {},
        (BeltState::Straight, BeltState::Curved) => {},
        (BeltState::Straight, BeltState::Sideloading) => {},
        (BeltState::Straight, BeltState::DoubleSideloading) => unreachable!("Should be impossible"),
        (BeltState::Curved, BeltState::Straight) => unreachable!("Should be impossible"),
        (BeltState::Curved, BeltState::Curved) => {},
        (BeltState::Curved, BeltState::Sideloading) | (BeltState::Curved, BeltState::DoubleSideloading) => {
            let entity = game_state.world.get_entities_colliding_with(pos_which_might_break, (1,1), data_store).into_iter().next().unwrap();

            let (id,belt_pos_to_break_at) = match entity {
                Entity::Belt { belt_pos, id, .. } => (*id, *belt_pos),
                Entity::Underground { .. } => {
                    // Undergrounds cannot be curved, so this is not a problem
                    return;
                },
                Entity::Splitter { .. } => {
                    // Splitters cannot be curved, so this is not a problem
                    return;
                },
                e => unreachable!("{e:?} does not have a belt_pos")
            };

            let res = game_state.simulation_state.factory.belts.break_belt_at(id, belt_pos_to_break_at);

            match res.kept_side {
                Side::FRONT => {
                    // FIXME: Understand this + 1
                    game_state.world.update_belt_id_after(res.kept_id, res.new_id, belt_pos_to_break_at + 1);
                    game_state.world.modify_belt_pos(res.new_id, -i16::try_from(belt_pos_to_break_at).unwrap());

                    let new_len = game_state.simulation_state.factory.belts.get_len(res.new_id);

                    match res.kept_id {
                        BeltTileId::EmptyBeltId(_) => todo!("Handle Sideloading on empty belts"),
                        BeltTileId::BeltId(kept_id) => {
                            let BeltTileId::BeltId(new_id) = res.new_id else {unreachable!()};
                            game_state.simulation_state.factory.add_belt_belt_inserter((new_id, 0), (kept_id, belt_pos_to_break_at - 1), BeltBeltInserterAdditionInfo { cooldown: 0});
                        },
                    }
                },
                Side::BACK => unimplemented!("In the currerent implementation we will always keep the Front."),
            }
        },
        (BeltState::Sideloading, BeltState::Straight) => unreachable!("Should be impossible"),
        (BeltState::Sideloading, BeltState::Curved) => unreachable!("Should be impossible"),
        (BeltState::Sideloading, BeltState::Sideloading) => {},
        (BeltState::Sideloading, BeltState::DoubleSideloading) => {},
        (BeltState::DoubleSideloading, _) => unreachable!("For the belt to be DoubleSideloading before, there would have to have been a belt here before"),
    }
}

fn attach_to_back_of_belt<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
    front_belt_id: BeltTileId<ItemIdxType>,
    back_belt_id: Option<BeltTileId<ItemIdxType>>,
    back_belt_len: u16,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> (BeltTileId<ItemIdxType>, u16) {
    let (final_belt_id, final_belt_len) = match back_belt_id {
        Some(back_belt_id_in_sim) => {
            game_state.world.modify_belt_pos(
                back_belt_id_in_sim,
                back_belt_len
                    .try_into()
                    .expect("FIXME: This could fail without a belt being too long!"),
            );

            let (final_belt_id, final_belt_len) = merge_belts(
                &mut game_state.simulation_state,
                front_belt_id,
                back_belt_id_in_sim,
                data_store,
            );

            if final_belt_id == front_belt_id {
                game_state
                    .world
                    .update_belt_id(back_belt_id_in_sim, final_belt_id);
            } else {
                game_state
                    .world
                    .update_belt_id(final_belt_id, back_belt_id_in_sim);
            }

            (final_belt_id, final_belt_len)
        },
        None => {
            // We did not merge with a back belt (i.e. the new belt does not exist in the simulation), so we just add this to the front belt
            let new_back_belt = EmptyBelt::new(back_belt_len);
            match front_belt_id {
                BeltTileId::EmptyBeltId(front_idx) => {
                    take_mut::take(
                        &mut game_state.simulation_state.factory.belts.empty_belts[front_idx],
                        |front_belt| EmptyBelt::join(new_back_belt, front_belt),
                    );

                    (
                        BeltTileId::EmptyBeltId(front_idx),
                        game_state.simulation_state.factory.belts.empty_belts[front_idx].len,
                    )
                },
                BeltTileId::BeltId(front_belt_id) => {
                    let front_item_id: usize = front_belt_id.item.id.into();
                    let front_belt = game_state.simulation_state.factory.belts.belts[front_item_id]
                        .belts
                        .get_mut(front_belt_id.index)
                        .expect("id from world not in simulation!!");

                    take_mut::take(front_belt, |front_belt| {
                        front_belt.join_with_empty(new_back_belt, Side::BACK)
                    });

                    (BeltTileId::BeltId(front_belt_id), front_belt.get_len())
                },
            }
        },
    };

    (final_belt_id, final_belt_len)
}

/// Returns (final_id, final_len)
fn merge_belts<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    simulation_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
    front_tile_id: BeltTileId<ItemIdxType>,
    back_tile_id: BeltTileId<ItemIdxType>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> (BeltTileId<ItemIdxType>, u16) {
    if front_tile_id == back_tile_id {
        let len = simulation_state.factory.belts.get_len(front_tile_id);

        match front_tile_id {
            BeltTileId::EmptyBeltId(idx) => {
                simulation_state.factory.belts.empty_belts[idx].make_circular()
            },
            BeltTileId::BeltId(belt_id) => {
                simulation_state.factory.belts.belts[Into::<usize>::into(belt_id.item.id)].belts
                    [belt_id.index]
                    .make_circular();
            },
        }
        return (front_tile_id, len);
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

fn get_belt_dir_for_sideloading<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
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
            Entity::Underground { direction, .. } => Some(*direction),
            Entity::Splitter { direction, .. } => Some(*direction),
            _ => None,
        })
        .flatten()
}

fn get_belt_in_dir<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
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
            Entity::Underground {
                direction,
                underground_dir: UndergroundDir::Entrance,
                ..
            } => Some(*direction),
            Entity::Splitter { direction, .. } => Some(*direction),
            _ => None,
        })
        .flatten()
}

fn get_belt_out_dir<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
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
            Entity::Underground {
                direction,
                underground_dir: UndergroundDir::Exit,
                ..
            } => Some(*direction),
            Entity::Splitter { direction, .. } => Some(*direction),
            _ => None,
        })
        .flatten()
}

struct LengthenResult {
    belt_pos_of_segment: u16,
    new_belt_len: u16,
}

/// Returns which belt_pos the attached amount has
fn lengthen<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
    belt: BeltTileId<ItemIdxType>,
    amount: u16,
    side: Side,
) -> LengthenResult {
    let new_len = game_state
        .simulation_state
        .factory
        .belts
        .add_length(belt, amount, side);

    match side {
        Side::FRONT => {
            game_state.world.modify_belt_pos(
                belt,
                amount
                    .try_into()
                    .expect("TODO: This could fail even if the belt is not too long!"),
            );
            LengthenResult {
                belt_pos_of_segment: new_len,
                new_belt_len: amount,
            }
        },
        Side::BACK => LengthenResult {
            belt_pos_of_segment: new_len,
            new_belt_len: new_len,
        },
    }
}

fn should_merge<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
    self_dir: Dir,
    front_pos: Position,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> Option<BeltTileId<ItemIdxType>> {
    let front_belt_dir = get_belt_in_dir(&game_state.world, front_pos, data_store);

    let front_expected_state = front_belt_dir.map(|front_belt_dir| {
        (
            expected_belt_state(front_belt_dir, |dir| {
                if *dir == self_dir.reverse() {
                    true
                } else {
                    game_state.world.get_belt_possible_inputs(front_pos)[*dir]
                }
            }),
            front_belt_dir,
        )
    });

    if let Some((front_belt_state, front_belt_dir)) = front_expected_state {
        match front_belt_state {
            BeltState::Straight | BeltState::Sideloading | BeltState::DoubleSideloading => {
                if front_belt_dir == self_dir {
                    match game_state
                        .world
                        .get_entities_colliding_with(front_pos, (1, 1), data_store)
                        .into_iter()
                        .next()
                        .unwrap()
                    {
                        Entity::Belt { id, .. } => Some(*id),
                        Entity::Underground {
                            underground_dir: UndergroundDir::Entrance,
                            id,
                            ..
                        } => Some(*id),
                        Entity::Underground {
                            underground_dir: UndergroundDir::Exit,
                            ..
                        } => None,
                        Entity::Splitter { .. } => Some(todo!("get the id from the simstate")),
                        _ => unreachable!(),
                    }
                } else {
                    None
                }
            },
            BeltState::Curved => {
                let rel = self_dir.compare(front_belt_dir);

                match rel {
                    DirRelative::SameDir => unreachable!(),
                    DirRelative::Turned => {
                        match game_state
                            .world
                            .get_entities_colliding_with(front_pos, (1, 1), data_store)
                            .into_iter()
                            .next()
                            .unwrap()
                        {
                            Entity::Belt { id, .. } => Some(*id),
                            Entity::Underground { .. } => None,
                            Entity::Splitter { .. } => None,
                            _ => unreachable!(),
                        }
                    },
                    DirRelative::Opposite => None,
                }
            },
        }
    } else {
        None
    }
}

fn should_sideload<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
    self_dir: Dir,
    front_pos: Position,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> Option<(BeltTileId<ItemIdxType>, u16)> {
    let front_belt_dir = get_belt_dir_for_sideloading(&game_state.world, front_pos, data_store);

    let front_expected_state = front_belt_dir.map(|front_belt_dir| {
        (
            expected_belt_state(front_belt_dir, |dir| {
                if *dir == self_dir.reverse() {
                    true
                } else {
                    game_state.world.get_belt_possible_inputs(front_pos)[*dir]
                }
            }),
            front_belt_dir,
        )
    });

    if let Some((front_belt_state, front_belt_dir)) = front_expected_state {
        match front_belt_state {
            BeltState::Straight => None,
            BeltState::Curved => None,
            BeltState::Sideloading | BeltState::DoubleSideloading => {
                let rel = self_dir.compare(front_belt_dir);

                match rel {
                    DirRelative::SameDir => None,
                    DirRelative::Turned => {
                        match game_state
                            .world
                            .get_entities_colliding_with(front_pos, (1, 1), data_store)
                            .into_iter()
                            .next()
                            .unwrap()
                        {
                            Entity::Belt { id, belt_pos, .. }
                            | Entity::Underground { id, belt_pos, .. } => Some((*id, *belt_pos)),
                            Entity::Splitter { .. } => None,
                            _ => unreachable!(),
                        }
                    },
                    DirRelative::Opposite => None,
                }
            },
        }
    } else {
        None
    }
}

fn expected_belt_state(belt_dir: Dir, gets_input_from: impl FnMut(&Dir) -> bool) -> BeltState {
    let input_dirs: Vec<Dir> = Dir::iter().filter(gets_input_from).collect();
    // Output dirs are unused for determining this, interesting!
    // let output_dirs: Vec<Dir> = Dir::iter().filter(|dir| dir_info(*dir) == Some(BeltDir::Ouput)).collect();

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
mod tests {

    proptest! {

        // TODO: Make sure that `should_merge` and `should_sideload` are mutually exclusive!
    }
}
