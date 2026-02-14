use std::iter::successors;

use itertools::Itertools;
use log::{error, info};
use strum::IntoEnumIterator;

use crate::{
    app_state::SimulationState,
    belt::{
        BeltTileId, SplitterInfo,
        belt::BeltLenType,
        smart::Side,
        splitter::{SPLITTER_BELT_LEN, SplitterDistributionMode, SplitterSide},
    },
    data::DataStore,
    frontend::world::{
        Position,
        tile::{BELT_LEN_PER_TILE, Dir, DirRelative, Entity, UndergroundDir, World},
    },
    item::IdxTrait,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BeltState {
    Straight,
    Curved,
    Sideloading,
    DoubleSideloading,
}

pub struct FakeGameState<'a, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    pub world: &'a mut World<ItemIdxType, RecipeIdxType>,
    pub simulation_state: &'a mut SimulationState<ItemIdxType, RecipeIdxType>,
}

pub fn handle_splitter_placement<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &mut World<ItemIdxType, RecipeIdxType>,
    simulation_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
    splitter_pos: Position,
    splitter_direction: Dir,
    splitter_ty: u8,
    in_mode: Option<SplitterDistributionMode>,
    out_mode: Option<SplitterDistributionMode>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    let mut fake_game_state = FakeGameState {
        world,
        simulation_state,
    };
    let game_state = &mut fake_game_state;

    #[cfg(debug_assertions)]
    {
        let all_belt_connections = game_state
            .world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .flat_map(|e| match e {
                Entity::Belt {
                    pos, direction, id, ..
                } => vec![(*pos, *direction, *id), (*pos, direction.reverse(), *id)],
                Entity::Underground {
                    pos,
                    underground_dir: UndergroundDir::Entrance,
                    direction,
                    id,
                    ..
                } => vec![(*pos, direction.reverse(), *id)],
                Entity::Underground {
                    pos,
                    underground_dir: UndergroundDir::Exit,
                    direction,
                    id,
                    ..
                } => vec![(*pos, *direction, *id)],
                Entity::Splitter { pos, direction, id } => {
                    // TODO:
                    vec![]
                },
                _ => vec![],
            });

        for (belt_pos, check_dir, id_that_should_exist) in all_belt_connections {
            if let Some(Entity::Splitter { id, .. }) = game_state
                .world
                .get_entity_at(belt_pos + check_dir, data_store)
            {
                assert!(
                    game_state
                        .simulation_state
                        .factory
                        .belts
                        .get_splitter_belt_ids(*id)
                        .iter()
                        .flatten()
                        .contains(&id_that_should_exist)
                );
            }
        }
    }
    let (left_pos, right_pos) = match splitter_direction {
        Dir::North => (splitter_pos, splitter_pos + Dir::East),
        Dir::East => (splitter_pos, splitter_pos + Dir::South),
        Dir::South => (splitter_pos + Dir::East, splitter_pos),
        Dir::West => (splitter_pos + Dir::South, splitter_pos),
    };
    let self_positions = [left_pos, right_pos];

    let [[left_front, left_back], [right_front, right_back]]: [[BeltTileId<ItemIdxType>; 2]; 2] =
        self_positions.map(|self_pos| {
            let front_pos = self_pos + splitter_direction;
            handle_belt_breaking(
                game_state.world,
                game_state.simulation_state,
                front_pos,
                splitter_direction,
                data_store,
            );

            // Handle front
            let (self_front_id, _self_front_len) = if let Some(id) = should_merge(
                game_state.world,
                &game_state.simulation_state,
                splitter_direction,
                front_pos,
                data_store,
            ) {
                debug_assert!(
                    should_sideload(game_state.world, splitter_direction, front_pos, data_store)
                        .is_none()
                );
                let result = lengthen(
                    game_state.world,
                    game_state.simulation_state,
                    id,
                    SPLITTER_BELT_LEN,
                    Side::BACK,
                );
                (id, result.new_belt_len)
            } else if let Some((id, pos)) =
                should_sideload(game_state.world, splitter_direction, front_pos, data_store)
            {
                // Add the little belt at the front of the splitter
                let new_belt = game_state
                    .simulation_state
                    .factory
                    .belts
                    .add_empty_belt(splitter_ty, SPLITTER_BELT_LEN);
                // Add Sideloading inserter
                game_state
                    .simulation_state
                    .factory
                    .belts
                    .add_sideloading_inserter(new_belt, (id, pos));
                (new_belt, SPLITTER_BELT_LEN)
            } else {
                let id = game_state
                    .simulation_state
                    .factory
                    .belts
                    .add_empty_belt(splitter_ty, SPLITTER_BELT_LEN);
                (id, SPLITTER_BELT_LEN)
            };

            let back_pos = self_pos + splitter_direction.reverse();
            let belt_dir = get_belt_out_dir(&game_state.world, back_pos, data_store);
            let (self_back_id, _self_back_len) = if let Some(belt_dir) = belt_dir {
                if belt_dir == splitter_direction {
                    // The belt at this position is pointing at the back of the splitter
                    let back_id = match game_state
                        .world
                        .get_entity_at(back_pos, data_store)
                        .unwrap()
                    {
                        Entity::Belt { id, .. } => *id,
                        Entity::Underground {
                            underground_dir: UndergroundDir::Exit,
                            id,
                            ..
                        } => *id,
                        Entity::Splitter { .. } => todo!("get the id from the simstate"),
                        e => unreachable!("{:?}", e),
                    };

                    let result = lengthen(
                        game_state.world,
                        game_state.simulation_state,
                        back_id,
                        SPLITTER_BELT_LEN,
                        Side::FRONT,
                    );

                    (back_id, result.new_belt_len)
                } else {
                    let id = game_state
                        .simulation_state
                        .factory
                        .belts
                        .add_empty_belt(splitter_ty, SPLITTER_BELT_LEN);
                    (id, SPLITTER_BELT_LEN)
                }
            } else {
                let id = game_state
                    .simulation_state
                    .factory
                    .belts
                    .add_empty_belt(splitter_ty, SPLITTER_BELT_LEN);
                (id, SPLITTER_BELT_LEN)
            };

            [self_front_id, self_back_id]
        });

    let splitter = SplitterInfo {
        in_mode: in_mode.unwrap_or_default(),
        out_mode: out_mode.unwrap_or_default(),
        input_belts: [left_back, right_back],
        output_belts: [left_front, right_front],
    };

    let id = game_state
        .simulation_state
        .factory
        .belts
        .add_splitter(splitter);

    game_state
        .world
        .add_entity(
            Entity::Splitter {
                pos: Position {
                    x: self_positions.into_iter().map(|pos| pos.x).min().unwrap(),
                    y: self_positions.into_iter().map(|pos| pos.y).min().unwrap(),
                },
                direction: splitter_direction,
                id,
            },
            &mut game_state.simulation_state,
            data_store,
        )
        .expect("Called handle_splitter_placement without enough space");

    #[cfg(debug_assertions)]
    {
        let all_belt_connections = game_state
            .world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .flat_map(|e| match e {
                Entity::Belt {
                    pos, direction, id, ..
                } => vec![(*pos, *direction, *id), (*pos, direction.reverse(), *id)],
                Entity::Underground {
                    pos,
                    underground_dir: UndergroundDir::Entrance,
                    direction,
                    id,
                    ..
                } => vec![(*pos, direction.reverse(), *id)],
                Entity::Underground {
                    pos,
                    underground_dir: UndergroundDir::Exit,
                    direction,
                    id,
                    ..
                } => vec![(*pos, *direction, *id)],
                Entity::Splitter { pos, direction, id } => {
                    // TODO:
                    vec![]
                },
                _ => vec![],
            });

        for (belt_pos, check_dir, id_that_should_exist) in all_belt_connections {
            if let Some(Entity::Splitter { pos, direction, id }) = game_state
                .world
                .get_entity_at(belt_pos + check_dir, data_store)
            {
                assert!(
                    game_state
                        .simulation_state
                        .factory
                        .belts
                        .get_splitter_belt_ids(*id)
                        .iter()
                        .flatten()
                        .contains(&id_that_should_exist),
                    "{:?}",
                    belt_pos
                );
            }
        }
    }
}

pub fn handle_belt_removal<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &mut World<ItemIdxType, RecipeIdxType>,
    sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
    our_belt_id: BeltTileId<ItemIdxType>,
    our_belt_pos: BeltLenType,
    belt_pos: Position,
    belt_dir: Dir,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    world.check_inserters(sim_state);
    // Handle outgoing direction (front_pos)
    let front_pos = belt_pos + belt_dir;

    let (front_id, back_id) =
        split_belt_at(world, sim_state, our_belt_id, our_belt_pos, data_store);

    world.check_inserters(sim_state);

    shorten(world, sim_state, front_id, BELT_LEN_PER_TILE, Side::BACK);

    world.check_inserters(sim_state);

    let front_entity = world.get_entity_at(front_pos, data_store);

    if let Some(e) = front_entity {
        match e {
            Entity::Belt {
                pos,
                direction,
                ty,
                id,
                belt_pos: front_belt_pos,

                state: front_state_stored,
            } => {
                let inputs = world.get_belt_possible_inputs_no_cache(*pos);
                let front_state = expected_belt_state(*direction, inputs);

                match (front_state, belt_dir.compare(*direction)) {
                    (_, DirRelative::Opposite) => {},

                    (BeltState::Straight, DirRelative::SameDir)
                    | (BeltState::Curved, DirRelative::Turned)
                    | (BeltState::DoubleSideloading, DirRelative::SameDir) => {
                        assert_eq!(our_belt_id, *id);
                    },
                    (BeltState::Straight, DirRelative::Turned) => unreachable!(),
                    (BeltState::Curved, DirRelative::SameDir) => unreachable!(),
                    (BeltState::Sideloading, DirRelative::SameDir) => {
                        todo!("Switch from sideloading to curved")
                    },
                    (BeltState::Sideloading, DirRelative::Turned)
                    | (BeltState::DoubleSideloading, DirRelative::Turned) => {
                        todo!("Remove Sideloading")
                    },
                }
            },

            _ => {},
        }
    }
    world.check_inserters(sim_state);

    // Any incoming direction could either be attached to us, which is handled by split_belt_at.
    // Or it is sideloading which will be removed by shorten
}

pub fn handle_underground_removal<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &mut World<ItemIdxType, RecipeIdxType>,
    sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
    our_belt_id: BeltTileId<ItemIdxType>,
    our_belt_pos: BeltLenType,
    underground_belt_pos: Position,
    underground_belt_dir: Dir,
    underground_belt_kind: UndergroundDir,
    underground_belt_ty: u8,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    let max_len = data_store.belt_infos[underground_belt_ty as usize]
        .has_underground
        .unwrap()
        .max_distance;
    let search_dir = match underground_belt_kind {
        UndergroundDir::Entrance => underground_belt_dir,
        UndergroundDir::Exit => underground_belt_dir.reverse(),
    };
    let self_len = 'self_len: {
        let mut pos = underground_belt_pos;
        for i in 1..max_len {
            pos = pos + search_dir;

            if let Some(Entity::Underground {
                pos: _,
                underground_dir,
                direction,
                ty,
                id,
                belt_pos,
            }) = world.get_entity_at(pos, data_store)
            {
                if *ty == underground_belt_ty && *direction == underground_belt_dir {
                    let self_len = i as u16 * BELT_LEN_PER_TILE;
                    if *underground_dir == underground_belt_kind {
                        // FIXME: We need to handle if us being gone allows two other undergrounds to connect (crashing replay 004)

                        break 'self_len 0;
                    }
                    assert_eq!(*id, our_belt_id);

                    break 'self_len self_len;
                }
            }
        }
        BELT_LEN_PER_TILE
    };

    let removal_start_pos = match underground_belt_kind {
        UndergroundDir::Entrance => our_belt_pos,
        UndergroundDir::Exit => our_belt_pos
            .checked_add(self_len - BELT_LEN_PER_TILE)
            .unwrap(),
    };

    // Handle outgoing direction (front_pos)
    let front_pos = underground_belt_pos + underground_belt_dir;

    let (front_id, _back_id) =
        split_belt_at(world, sim_state, our_belt_id, removal_start_pos, data_store);

    shorten(world, sim_state, front_id, self_len, Side::BACK);

    if underground_belt_kind == UndergroundDir::Exit {
        let front_entity = world.get_entity_at(front_pos, data_store);

        if let Some(e) = front_entity {
            match e {
                Entity::Belt {
                    pos,
                    direction,
                    ty,
                    id,
                    belt_pos: front_belt_pos,

                    state: front_state_stored,
                } => {
                    let inputs = world.get_belt_possible_inputs_no_cache(*pos);
                    let front_state = expected_belt_state(*direction, inputs);

                    match (front_state, underground_belt_dir.compare(*direction)) {
                        (_, DirRelative::Opposite) => {},

                        (BeltState::Straight, DirRelative::SameDir)
                        | (BeltState::Curved, DirRelative::Turned)
                        | (BeltState::DoubleSideloading, DirRelative::SameDir) => {
                            assert_eq!(our_belt_id, *id);
                        },
                        (BeltState::Straight, DirRelative::Turned) => unreachable!(),
                        (BeltState::Curved, DirRelative::SameDir) => unreachable!(),
                        (BeltState::Sideloading, DirRelative::SameDir) => {
                            todo!("Switch from sideloading to curved")
                        },
                        (BeltState::Sideloading, DirRelative::Turned)
                        | (BeltState::DoubleSideloading, DirRelative::Turned) => {
                            todo!("Remove Sideloading")
                        },
                    }
                },

                _ => {},
            }
        }
    }

    // Any incoming direction could either be attached to us, which is handled by split_belt_at.
    // Or it is sideloading which will be removed by shorten
}

#[allow(unreachable_code)]
pub fn handle_belt_placement<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &mut World<ItemIdxType, RecipeIdxType>,
    sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
    new_belt_pos: Position,
    new_belt_direction: Dir,
    new_belt_ty: u8,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    // world.check_inserters(&game_state.simulation_state);

    let front_pos = new_belt_pos + new_belt_direction;

    handle_belt_breaking(world, sim_state, front_pos, new_belt_direction, data_store);

    let our_inputs = world.get_belt_possible_inputs(new_belt_pos);
    let our_state = expected_belt_state(new_belt_direction, our_inputs);

    let our_state_stored = match our_state {
        BeltState::Straight | BeltState::Sideloading | BeltState::DoubleSideloading => {
            crate::frontend::world::tile::BeltState::Straight
        },
        BeltState::Curved => crate::frontend::world::tile::BeltState::Curved {
            source_dir: our_inputs
                .into_iter()
                .find(|(_dir, has_source)| *has_source)
                .unwrap()
                .0,
        },
    };

    let (self_id, self_len) = if let Some(id) =
        should_merge(world, &sim_state, new_belt_direction, front_pos, data_store)
    {
        debug_assert!(should_sideload(world, new_belt_direction, front_pos, data_store).is_none());
        let result = lengthen(world, sim_state, id, BELT_LEN_PER_TILE, Side::BACK);
        world
            .add_entity(
                Entity::Belt {
                    pos: new_belt_pos,
                    direction: new_belt_direction,
                    ty: new_belt_ty,
                    id,
                    belt_pos: result.belt_pos_of_segment,

                    state: our_state_stored,
                },
                sim_state,
                data_store,
            )
            .expect("Called handle_belt_placement without enough space");
        (id, result.new_belt_len)
    } else if let Some((id, pos)) =
        should_sideload(world, new_belt_direction, front_pos, data_store)
    {
        // Add new belt
        let new_belt = sim_state
            .factory
            .belts
            .add_empty_belt(new_belt_ty, BELT_LEN_PER_TILE);
        // Add Sideloading inserter
        sim_state
            .factory
            .belts
            .add_sideloading_inserter(new_belt, (id, pos));

        world
            .add_entity(
                Entity::Belt {
                    pos: new_belt_pos,
                    direction: new_belt_direction,
                    ty: new_belt_ty,
                    id: new_belt,
                    belt_pos: BELT_LEN_PER_TILE,

                    state: our_state_stored,
                },
                sim_state,
                data_store,
            )
            .expect("Called handle_belt_placement without enough space");
        (new_belt, BELT_LEN_PER_TILE)
    } else {
        let id = sim_state
            .factory
            .belts
            .add_empty_belt(new_belt_ty, BELT_LEN_PER_TILE);
        world
            .add_entity(
                Entity::Belt {
                    pos: new_belt_pos,
                    direction: new_belt_direction,
                    ty: new_belt_ty,
                    id,
                    belt_pos: BELT_LEN_PER_TILE,

                    state: our_state_stored,
                },
                sim_state,
                data_store,
            )
            .expect("Called handle_belt_placement without enough space");
        (id, BELT_LEN_PER_TILE)
    };

    for dir in Dir::iter() {
        if dir.reverse() == new_belt_direction {
            continue;
        }
        let potentially_incoming_pos = new_belt_pos + dir.reverse();
        let belt_dir = get_belt_out_dir(&world, potentially_incoming_pos, data_store);
        if let Some(belt_dir) = belt_dir {
            if belt_dir != dir {
                // The belt at this position is not pointing at the new belt
                continue;
            }

            if let Some(id) = should_merge(world, &sim_state, dir, new_belt_pos, data_store) {
                debug_assert!(should_sideload(world, dir, new_belt_pos, data_store).is_none());
                debug_assert_eq!(id, self_id);

                let back_id = match world
                    .get_entity_at(potentially_incoming_pos, data_store)
                    .unwrap()
                {
                    Entity::Belt { id, .. }
                    | Entity::Underground {
                        id,
                        underground_dir: UndergroundDir::Exit,
                        ..
                    } => *id,
                    Entity::Splitter {
                        pos,
                        id,
                        direction: splitter_dir,
                        ..
                    } => {
                        let mut side = if potentially_incoming_pos == *pos {
                            SplitterSide::Left
                        } else {
                            SplitterSide::Right
                        };

                        match splitter_dir {
                            Dir::North | Dir::East => {},
                            Dir::South | Dir::West => side = side.switch(),
                        }

                        let [_, outputs] = sim_state.factory.belts.get_splitter_belt_ids(*id);

                        outputs[usize::from(bool::from(side))]
                    },
                    _ => unreachable!(),
                };

                let (final_id, final_len) = merge_belts(sim_state, self_id, back_id, data_store);
                // TODO: try_into could fail even if the belt len is fine
                if self_id != back_id {
                    world.modify_belt_pos(back_id, false, self_len);
                }
                if final_id == back_id {
                    world.update_belt_id(sim_state, self_id, final_id, data_store);
                } else {
                    world.update_belt_id(sim_state, back_id, final_id, data_store);
                }
            } else if let Some((id, belt_pos)) =
                should_sideload(world, dir, new_belt_pos, data_store)
            {
                debug_assert_eq!(id, self_id);

                let (back_id, back_pos) = match world
                    .get_entity_at(potentially_incoming_pos, data_store)
                    .unwrap()
                {
                    Entity::Belt { id, belt_pos, .. }
                    | Entity::Underground {
                        id,
                        belt_pos,
                        underground_dir: UndergroundDir::Exit,
                        ..
                    } => (*id, *belt_pos),
                    Entity::Splitter { .. } => todo!(),
                    _ => unreachable!(),
                };

                // Add Sideloading inserter
                sim_state
                    .factory
                    .belts
                    .add_sideloading_inserter(back_id, (id, belt_pos));
            } else {
                // Do nothing
            }
        }
    }

    #[cfg(debug_assertions)]
    {
        let all_belt_connections = world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .flat_map(|e| match e {
                Entity::Belt {
                    pos,
                    direction,
                    ty,
                    id,
                    belt_pos,
                    ..
                } => vec![(*pos, *direction, *id), (*pos, direction.reverse(), *id)],
                Entity::Underground {
                    pos,
                    underground_dir: UndergroundDir::Entrance,
                    direction,
                    ty,
                    id,
                    belt_pos,
                } => vec![(*pos, direction.reverse(), *id)],
                Entity::Underground {
                    pos,
                    underground_dir: UndergroundDir::Exit,
                    direction,
                    ty,
                    id,
                    belt_pos,
                } => vec![(*pos, *direction, *id)],
                Entity::Splitter { pos, direction, id } => {
                    // TODO:
                    vec![]
                },
                _ => vec![],
            });

        for (belt_pos, check_dir, id_that_should_exist) in all_belt_connections {
            if let Some(Entity::Splitter { pos, direction, id }) =
                world.get_entity_at(belt_pos + check_dir, data_store)
            {
                assert!(
                    sim_state
                        .factory
                        .belts
                        .get_splitter_belt_ids(*id)
                        .iter()
                        .flatten()
                        .contains(&id_that_should_exist)
                );
            }
        }

        // world.check_inserters(&game_state.simulation_state);
    }
}

#[allow(unreachable_code)]
pub fn handle_underground_belt_placement<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &mut World<ItemIdxType, RecipeIdxType>,
    sim_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
    new_belt_pos: Position,
    new_belt_direction: Dir,
    new_belt_ty: u8,
    new_underground_dir: UndergroundDir,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    world.check_inserters(sim_state);

    let max_distance: u8 = data_store.belt_infos[usize::from(new_belt_ty)]
        .has_underground
        .expect("Tried to place underground of type without underground support")
        .max_distance;

    // Break front belt if necessary
    match new_underground_dir {
        UndergroundDir::Entrance => {
            // Find (and break if necessary) the connecting underground
            let first_underground =
                successors(Some(new_belt_pos), |pos| Some(*pos + new_belt_direction))
                    .skip(1)
                    .take(max_distance.into())
                    .find_map(|check_pos| {
                        let e = world.get_entity_at(check_pos, data_store);

                        if let Some(Entity::Underground { ty, direction, .. }) = e {
                            if *direction == new_belt_direction {
                                // Only match with belts of the same type (this allows belt weaving)
                                if *ty == new_belt_ty {
                                    Some(e.unwrap())
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    });

            let (self_id, self_len) = if let Some(e) = first_underground {
                let Entity::Underground {
                    pos,
                    underground_dir,
                    direction,
                    ty,
                    id,
                    belt_pos,
                } = e.clone()
                else {
                    unreachable!()
                };

                match underground_dir {
                    UndergroundDir::Entrance => {
                        // The first underground we found was another Entrance, so we do not need to connect, nor break anything

                        let id = sim_state
                            .factory
                            .belts
                            .add_empty_belt(new_belt_ty, BELT_LEN_PER_TILE);
                        world
                            .add_entity(
                                Entity::Underground {
                                    pos: new_belt_pos,
                                    direction: new_belt_direction,
                                    ty: new_belt_ty,
                                    underground_dir: new_underground_dir,
                                    id,
                                    belt_pos: BELT_LEN_PER_TILE,
                                },
                                sim_state,
                                data_store,
                            )
                            .expect("Called handle_underground_placement without enough space");
                        (id, BELT_LEN_PER_TILE)
                    },
                    UndergroundDir::Exit => {
                        let exit_id = if belt_pos == sim_state.factory.belts.get_len(id) {
                            // This is the start of the belt
                            // no breaking necessary

                            id
                        } else {
                            info!("Breaking underground connection");
                            let (front_id, back_id) =
                                break_belt_at(world, sim_state, id, belt_pos, data_store);
                            let shorten_by_amount = 'amount: {
                                let mut pos = pos;
                                for i in 1..=max_distance {
                                    pos = pos + direction.reverse();

                                    if let Some(Entity::Underground {
                                        pos: _,
                                        underground_dir: found_underground_dir,
                                        direction: found_direction,
                                        ty: found_ty,
                                        id: found_id,
                                        belt_pos: found_belt_pos,
                                    }) = world.get_entity_at(pos, data_store)
                                    {
                                        if *found_ty == ty && *found_direction == direction {
                                            let underground_len =
                                                (i as u16 - 1) * BELT_LEN_PER_TILE;
                                            assert_eq!(
                                                UndergroundDir::Entrance,
                                                *found_underground_dir,
                                            );
                                            assert_eq!(*found_id, back_id);
                                            assert_eq!(
                                                *found_belt_pos - underground_len,
                                                BELT_LEN_PER_TILE
                                            );

                                            break 'amount underground_len;
                                        }
                                    }
                                }
                                unreachable!(
                                    "A underground that is attached, does not have a matching underground in range"
                                )
                            };
                            shorten(world, sim_state, back_id, shorten_by_amount, Side::FRONT);
                            front_id
                        };

                        world.check_inserters(sim_state);

                        // Add the "underground" belt locs
                        let res = lengthen(
                            world,
                            sim_state,
                            id,
                            // Underground
                            BELT_LEN_PER_TILE
                                * u16::try_from(
                                    new_belt_pos.x.abs_diff(pos.x) + new_belt_pos.y.abs_diff(pos.y),
                                )
                                .unwrap(),
                            Side::BACK,
                        );

                        world
                            .add_entity(
                                Entity::Underground {
                                    pos: new_belt_pos,
                                    direction: new_belt_direction,
                                    ty: new_belt_ty,
                                    underground_dir: new_underground_dir,
                                    id: exit_id,
                                    belt_pos: res.new_belt_len,
                                },
                                sim_state,
                                data_store,
                            )
                            .expect("Called handle_underground_placement without enough space");

                        (exit_id, res.new_belt_len)
                    },
                }
            } else {
                // No underground found to connect to
                let id = sim_state
                    .factory
                    .belts
                    .add_empty_belt(new_belt_ty, BELT_LEN_PER_TILE);
                world
                    .add_entity(
                        Entity::Underground {
                            pos: new_belt_pos,
                            direction: new_belt_direction,
                            ty: new_belt_ty,
                            underground_dir: new_underground_dir,
                            id,
                            belt_pos: BELT_LEN_PER_TILE,
                        },
                        sim_state,
                        data_store,
                    )
                    .expect("Called handle_underground_placement without enough space");
                (id, BELT_LEN_PER_TILE)
            };

            // An underground Entrance cannot break any belt
            // should_merge does not work if the entity at "front_pos" is not in the world yet
            if let Some(id) = should_merge(
                world,
                &sim_state,
                new_belt_direction,
                new_belt_pos,
                data_store,
            ) {
                assert_eq!(self_id, id);
                debug_assert!(
                    should_sideload(world, new_belt_direction, new_belt_pos, data_store).is_none()
                );

                let potentially_incoming_pos = new_belt_pos + new_belt_direction.reverse();

                let back_id = match world.get_entity_at(potentially_incoming_pos, data_store) {
                    Some(Entity::Belt { id, .. })
                    | Some(Entity::Underground {
                        id,
                        underground_dir: UndergroundDir::Exit,
                        ..
                    }) => Some(*id),
                    Some(Entity::Splitter { .. }) => todo!(),
                    Some(_) => None,
                    None => None,
                };

                if let Some(back_id) = back_id {
                    world.modify_belt_pos(back_id, false, self_len);
                    let (final_id, final_len) =
                        merge_belts(sim_state, self_id, back_id, data_store);
                    if final_id == self_id {
                        world.update_belt_id(sim_state, back_id, final_id, data_store);
                    } else {
                        world.update_belt_id(sim_state, self_id, final_id, data_store);
                    }
                    (final_id, final_len)
                } else {
                    (self_id, self_len)
                }
            } else if let Some((id, pos)) =
                should_sideload(world, new_belt_direction, new_belt_pos, data_store)
            {
                unreachable!()
            } else {
                (self_id, self_len)
            };
        },
        UndergroundDir::Exit => {
            let front_pos = new_belt_pos + new_belt_direction;

            handle_belt_breaking(world, sim_state, front_pos, new_belt_direction, data_store);

            // Handle front tile
            let (self_id, self_len) = if let Some(id) =
                should_merge(world, &sim_state, new_belt_direction, front_pos, data_store)
            {
                debug_assert!(
                    should_sideload(world, new_belt_direction, front_pos, data_store).is_none()
                );
                let result = lengthen(world, sim_state, id, BELT_LEN_PER_TILE, Side::BACK);
                world
                    .add_entity(
                        Entity::Underground {
                            pos: new_belt_pos,
                            underground_dir: new_underground_dir,
                            direction: new_belt_direction,
                            ty: new_belt_ty,
                            id,
                            belt_pos: result.belt_pos_of_segment,
                        },
                        sim_state,
                        data_store,
                    )
                    .expect("Called handle_underground_placement without enough space");
                (id, result.new_belt_len)
            } else if let Some((id, pos)) =
                should_sideload(world, new_belt_direction, front_pos, data_store)
            {
                // Add new belt
                let new_belt = sim_state
                    .factory
                    .belts
                    .add_empty_belt(new_belt_ty, BELT_LEN_PER_TILE);
                // Add Sideloading inserter
                sim_state
                    .factory
                    .belts
                    .add_sideloading_inserter(new_belt, (id, pos));

                world
                    .add_entity(
                        Entity::Underground {
                            pos: new_belt_pos,
                            underground_dir: new_underground_dir,
                            direction: new_belt_direction,
                            ty: new_belt_ty,
                            id,
                            belt_pos: BELT_LEN_PER_TILE,
                        },
                        sim_state,
                        data_store,
                    )
                    .expect("Called handle_underground_placement without enough space");
                (new_belt, BELT_LEN_PER_TILE)
            } else {
                let id = sim_state
                    .factory
                    .belts
                    .add_empty_belt(new_belt_ty, BELT_LEN_PER_TILE);
                world
                    .add_entity(
                        Entity::Underground {
                            pos: new_belt_pos,
                            direction: new_belt_direction,
                            ty: new_belt_ty,
                            underground_dir: new_underground_dir,
                            id,
                            belt_pos: BELT_LEN_PER_TILE,
                        },
                        sim_state,
                        data_store,
                    )
                    .expect("Called handle_underground_placement without enough space");
                (id, BELT_LEN_PER_TILE)
            };

            // Handle side tiles
            for dir in Dir::iter() {
                if dir == new_belt_direction {
                    // This is the front tile, which we already handled
                    continue;
                }
                if dir == new_belt_direction.reverse() {
                    // This is the back tile, which we do not care about for sideloading
                    continue;
                }

                let potentially_incoming_pos = new_belt_pos + dir;

                debug_assert!(
                    should_merge(world, &sim_state, dir.reverse(), new_belt_pos, data_store)
                        .is_none()
                );

                if let Some((id, belt_pos)) =
                    should_sideload(world, dir.reverse(), new_belt_pos, data_store)
                {
                    debug_assert_eq!(id, self_id);

                    let (back_id, back_pos) = match world
                        .get_entity_at(potentially_incoming_pos, data_store)
                        .unwrap()
                    {
                        Entity::Belt { id, belt_pos, .. }
                        | Entity::Underground {
                            id,
                            belt_pos,
                            underground_dir: UndergroundDir::Exit,
                            ..
                        } => (*id, *belt_pos),
                        Entity::Splitter { .. } => todo!(),
                        e => unreachable!("{:?}", e),
                    };

                    // Add Sideloading inserter
                    sim_state
                        .factory
                        .belts
                        .add_sideloading_inserter(back_id, (id, belt_pos));
                }
            }

            let first_underground = successors(Some(new_belt_pos), |pos| {
                Some(*pos + new_belt_direction.reverse())
            })
            .skip(1)
            .take(max_distance.into())
            .find_map(|check_pos| {
                let e = world.get_entity_at(check_pos, data_store);

                if let Some(Entity::Underground { ty, direction, .. }) = e {
                    if *direction == new_belt_direction {
                        // Only match with belts of the same type (this allows belt weaving)
                        if *ty == new_belt_ty {
                            Some(e.unwrap())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            });

            if let Some(e) = first_underground {
                let Entity::Underground {
                    pos,
                    underground_dir,
                    direction,
                    ty,
                    id,
                    belt_pos,
                } = e.clone()
                else {
                    unreachable!()
                };

                match underground_dir {
                    UndergroundDir::Entrance => {
                        let entrance_id = if belt_pos == BELT_LEN_PER_TILE {
                            // This is the end of the belt
                            // no breaking necessary
                            id
                        } else {
                            info!("Breaking underground connection");
                            let (front_id, back_id) = break_belt_at(
                                world,
                                sim_state,
                                id,
                                belt_pos - BELT_LEN_PER_TILE,
                                data_store,
                            );
                            let shorten_by_amount = 'amount: {
                                let mut pos = pos;
                                for i in 1..=max_distance {
                                    pos = pos + direction;

                                    if let Some(Entity::Underground {
                                        pos: _,
                                        underground_dir: found_underground_dir,
                                        direction: found_direction,
                                        ty: found_ty,
                                        id: found_id,
                                        belt_pos: found_belt_pos,
                                    }) = world.get_entity_at(pos, data_store)
                                    {
                                        if *found_ty == ty
                                            && *found_direction == direction
                                            && *found_id == front_id
                                        {
                                            let underground_len =
                                                (i as u16 - 1) * BELT_LEN_PER_TILE;
                                            assert_eq!(
                                                UndergroundDir::Exit,
                                                *found_underground_dir,
                                            );
                                            assert_eq!(
                                                *found_belt_pos + underground_len,
                                                sim_state.factory.belts.get_len(*found_id)
                                            );

                                            break 'amount underground_len;
                                        }
                                    }
                                }
                                unreachable!(
                                    "A underground that is attached, does not have a matching underground in range"
                                )
                            };
                            shorten(world, sim_state, front_id, shorten_by_amount, Side::BACK);
                            back_id
                        };

                        world.check_inserters(sim_state);

                        // Add the "underground" belt locs
                        let res = lengthen(
                            world,
                            sim_state,
                            self_id,
                            BELT_LEN_PER_TILE
                                * (u16::try_from(
                                    new_belt_pos.x.abs_diff(pos.x) + new_belt_pos.y.abs_diff(pos.y),
                                )
                                .unwrap()
                                    - 1), // We need to subract 1 here to account for the BELT_LEN_PER_TILE we already added before
                            Side::BACK,
                        );

                        // Connect them together
                        let (new_id, new_len) =
                            merge_belts(sim_state, self_id, entrance_id, data_store);

                        if self_id != entrance_id {
                            world.modify_belt_pos(entrance_id, false, res.new_belt_len);
                        }
                        if new_id == self_id {
                            world.update_belt_id(sim_state, entrance_id, new_id, data_store);
                        } else {
                            world.update_belt_id(sim_state, self_id, new_id, data_store);
                        }
                    },
                    UndergroundDir::Exit => {
                        // The first underground we found was another Exit, so we do not need to connect, nor break anything
                    },
                }
            }
        },
    }

    #[cfg(debug_assertions)]
    {
        let all_belt_connections = world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .flat_map(|e| match e {
                Entity::Belt {
                    pos,
                    direction,
                    ty,
                    id,
                    belt_pos,
                    ..
                } => vec![(*pos, *direction, *id), (*pos, direction.reverse(), *id)],
                Entity::Underground {
                    pos,
                    underground_dir: UndergroundDir::Entrance,
                    direction,
                    ty,
                    id,
                    belt_pos,
                } => vec![(*pos, direction.reverse(), *id)],
                Entity::Underground {
                    pos,
                    underground_dir: UndergroundDir::Exit,
                    direction,
                    ty,
                    id,
                    belt_pos,
                } => vec![(*pos, *direction, *id)],
                Entity::Splitter { pos, direction, id } => {
                    // TODO:
                    vec![]
                },
                _ => vec![],
            });

        for (belt_pos, check_dir, id_that_should_exist) in all_belt_connections {
            if let Some(Entity::Splitter { pos, direction, id }) =
                world.get_entity_at(belt_pos + check_dir, data_store)
            {
                assert!(
                    sim_state
                        .factory
                        .belts
                        .get_splitter_belt_ids(*id)
                        .iter()
                        .flatten()
                        .contains(&id_that_should_exist)
                );
            }
        }

        world.check_inserters(sim_state);
    }
}

enum BeltChange {
    Added,
    Removed,
}

fn handle_belt_breaking<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &mut World<ItemIdxType, RecipeIdxType>,
    simulation_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
    pos_which_might_break: Position,
    dir_which_the_new_belt_is_pointing: Dir,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    let Some(dir_for_belt_which_might_break) =
        get_belt_dir_for_sideloading(world, pos_which_might_break, data_store)
    else {
        return;
    };

    let mut input_dirs = world
        .get_belt_possible_inputs(pos_which_might_break)
        .clone();
    let old_state = expected_belt_state(dir_for_belt_which_might_break, input_dirs);
    input_dirs[dir_which_the_new_belt_is_pointing.reverse()] = true;
    let new_state = expected_belt_state(dir_for_belt_which_might_break, input_dirs);

    match (old_state, new_state) {
        (BeltState::Straight, BeltState::Straight) => {},
        (BeltState::Straight, BeltState::Curved) => {},
        (BeltState::Straight, BeltState::Sideloading) => {},
        (BeltState::Straight, BeltState::DoubleSideloading) => unreachable!("Should be impossible"),
        (BeltState::Curved, BeltState::Straight) => unreachable!("Should be impossible"),
        (BeltState::Curved, BeltState::Curved) => {},
        (BeltState::Curved, BeltState::Sideloading)
        | (BeltState::Curved, BeltState::DoubleSideloading) => {
            let entity = world
                .get_entity_at(pos_which_might_break, data_store)
                .unwrap();

            let (id, belt_pos_to_break_at) = match entity {
                Entity::Belt { belt_pos, id, .. } => (*id, *belt_pos),
                Entity::Underground { .. } => {
                    // Undergrounds cannot be curved, so this is not a problem
                    return;
                },
                Entity::Splitter { .. } => {
                    // Splitters cannot be curved, so this is not a problem
                    return;
                },
                e => unreachable!("{e:?} does not have a belt_pos"),
            };

            let res = simulation_state
                .factory
                .belts
                .break_belt_at(id, belt_pos_to_break_at);

            match res.new_belt {
                Some((new_id, new_belt_side)) => {
                    match new_belt_side {
                        Side::FRONT => unimplemented!(
                            "In the currerent implementation we will always keep the Front."
                        ),
                        Side::BACK => {
                            // FIXME: Understand this + 1
                            world.update_belt_id_after(
                                simulation_state,
                                res.kept_id,
                                new_id,
                                belt_pos_to_break_at + 1,
                                data_store,
                            );
                            world.modify_belt_pos(new_id, true, belt_pos_to_break_at);

                            let new_len = simulation_state.factory.belts.get_len(new_id);

                            simulation_state.factory.belts.add_sideloading_inserter(
                                new_id,
                                (res.kept_id, belt_pos_to_break_at - 1),
                            );
                        },
                    }
                },
                None => {
                    error!("The belt stopped being circular. What to do?");

                    // The belt_id is still correct.

                    // TODO: What about belt_positions?

                    // TODO: What about sideloading?
                },
            }
        },
        (BeltState::Sideloading, BeltState::Straight) => unreachable!("Should be impossible"),
        (BeltState::Sideloading, BeltState::Curved) => unreachable!("Should be impossible"),
        (BeltState::Sideloading, BeltState::Sideloading) => {},
        (BeltState::Sideloading, BeltState::DoubleSideloading) => {},
        (BeltState::DoubleSideloading, _) => unreachable!(
            "For the belt to be DoubleSideloading before, there would have to have been a belt here before"
        ),
    }

    #[cfg(debug_assertions)]
    {
        let all_belt_connections = world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .flat_map(|e| match e {
                Entity::Belt {
                    pos,
                    direction,
                    ty,
                    id,
                    belt_pos,
                    ..
                } => vec![(*pos, *direction, *id), (*pos, direction.reverse(), *id)],
                Entity::Underground {
                    pos,
                    underground_dir: UndergroundDir::Entrance,
                    direction,
                    ty,
                    id,
                    belt_pos,
                } => vec![(*pos, direction.reverse(), *id)],
                Entity::Underground {
                    pos,
                    underground_dir: UndergroundDir::Exit,
                    direction,
                    ty,
                    id,
                    belt_pos,
                } => vec![(*pos, *direction, *id)],
                Entity::Splitter { pos, direction, id } => {
                    // TODO:
                    vec![]
                },
                _ => vec![],
            });

        for (belt_pos, check_dir, id_that_should_exist) in all_belt_connections {
            if let Some(Entity::Splitter { pos, direction, id }) =
                world.get_entity_at(belt_pos + check_dir, data_store)
            {
                assert!(
                    simulation_state
                        .factory
                        .belts
                        .get_splitter_belt_ids(*id)
                        .iter()
                        .flatten()
                        .contains(&id_that_should_exist)
                );
            }
        }
    }
}

fn break_belt_at<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &mut World<ItemIdxType, RecipeIdxType>,
    simulation_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
    old_belt_id: BeltTileId<ItemIdxType>,
    first_belt_pos_belonging_to_back: BeltLenType,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> (BeltTileId<ItemIdxType>, BeltTileId<ItemIdxType>) {
    world.check_inserters(simulation_state);

    let res = simulation_state
        .factory
        .belts
        .break_belt_at(old_belt_id, first_belt_pos_belonging_to_back);

    let ret = match res.new_belt {
        Some((new_id, new_belt_side)) => match new_belt_side {
            Side::FRONT => {
                unimplemented!("In the currerent implementation we will always keep the Front.")
            },
            Side::BACK => {
                world.update_belt_id_after(
                    simulation_state,
                    res.kept_id,
                    new_id,
                    first_belt_pos_belonging_to_back + 1,
                    data_store,
                );
                let reduce_amount = first_belt_pos_belonging_to_back;
                assert_eq!(reduce_amount % BELT_LEN_PER_TILE, 0);
                world.modify_belt_pos(new_id, true, reduce_amount);

                (old_belt_id, new_id)
            },
        },
        None => (old_belt_id, old_belt_id),
    };

    world.check_inserters(simulation_state);
    ret
}

fn attach_to_back_of_belt<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    game_state: &mut FakeGameState<'_, ItemIdxType, RecipeIdxType>,
    front_belt_id: BeltTileId<ItemIdxType>,
    back_belt_id: Option<BeltTileId<ItemIdxType>>,
    back_belt_len: u16,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> (BeltTileId<ItemIdxType>, u16) {
    let (final_belt_id, final_belt_len) = match back_belt_id {
        Some(back_belt_id_in_sim) => {
            game_state
                .world
                .modify_belt_pos(back_belt_id_in_sim, false, back_belt_len);

            let (final_belt_id, final_belt_len) = merge_belts(
                &mut game_state.simulation_state,
                front_belt_id,
                back_belt_id_in_sim,
                data_store,
            );

            if final_belt_id == front_belt_id {
                game_state.world.update_belt_id(
                    &mut game_state.simulation_state,
                    back_belt_id_in_sim,
                    final_belt_id,
                    data_store,
                );
            } else {
                game_state.world.update_belt_id(
                    &mut game_state.simulation_state,
                    final_belt_id,
                    back_belt_id_in_sim,
                    data_store,
                );
            }

            (final_belt_id, final_belt_len)
        },
        None => {
            let new_length = game_state.simulation_state.factory.belts.add_length(
                front_belt_id,
                back_belt_len,
                Side::BACK,
            );

            (front_belt_id, new_length)
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
    simulation_state
        .factory
        .belts
        .merge_belts(front_tile_id, back_tile_id, data_store)
}

fn get_belt_dir_for_sideloading<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &World<ItemIdxType, RecipeIdxType>,
    position: Position,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> Option<Dir> {
    world
        .get_entity_at(position, data_store)
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
        .get_entity_at(position, data_store)
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
        .get_entity_at(position, data_store)
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

#[derive(Debug, Clone, Copy)]
struct LengthenResult {
    belt_pos_of_segment: u16,
    new_belt_len: u16,
}

/// Returns which belt_pos the attached amount has
fn lengthen<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &mut World<ItemIdxType, RecipeIdxType>,
    simulation_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
    belt: BeltTileId<ItemIdxType>,
    amount: u16,
    side: Side,
) -> LengthenResult {
    let new_len = simulation_state
        .factory
        .belts
        .add_length(belt, amount, side);

    match side {
        Side::FRONT => {
            world.modify_belt_pos(belt, false, amount);
            LengthenResult {
                belt_pos_of_segment: amount,
                new_belt_len: new_len,
            }
        },
        Side::BACK => LengthenResult {
            belt_pos_of_segment: new_len,
            new_belt_len: new_len,
        },
    }
}

/// Removes some length from a belt, updates the world entities and removes inserters if connected to the removed part
fn shorten<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &mut World<ItemIdxType, RecipeIdxType>,
    simulation_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
    belt: BeltTileId<ItemIdxType>,
    amount: u16,
    side: Side,
) {
    let (removed_items, new_len) = simulation_state
        .factory
        .belts
        .remove_length(belt, amount, side);

    // FIXME: Remove belt belt inserters

    if new_len == 0 {
        // The belt no longer exists
        return;
    }

    match side {
        Side::FRONT => {
            world.modify_belt_pos(belt, true, amount);
        },
        Side::BACK => {},
    }
}

fn split_belt_at<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &mut World<ItemIdxType, RecipeIdxType>,
    simulation_state: &mut SimulationState<ItemIdxType, RecipeIdxType>,
    old_id: BeltTileId<ItemIdxType>,
    belt_pos_to_break_at: BeltLenType,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> (BeltTileId<ItemIdxType>, BeltTileId<ItemIdxType>) {
    let res = simulation_state
        .factory
        .belts
        .break_belt_at(old_id, belt_pos_to_break_at);

    match res.new_belt {
        Some((new_id, new_belt_side)) => {
            match new_belt_side {
                Side::FRONT => {
                    unimplemented!("In the currerent implementation we will always keep the Front.")
                },
                Side::BACK => {
                    // FIXME: Understand this + 1
                    world.update_belt_id_after(
                        simulation_state,
                        res.kept_id,
                        new_id,
                        belt_pos_to_break_at + 1,
                        data_store,
                    );
                    world.modify_belt_pos(new_id, true, belt_pos_to_break_at);

                    // FIXME: Fix belt belt inserter ids and positions

                    (old_id, new_id)
                },
            }
        },
        None => {
            // The belt was circular before. Both sides still have the same id
            (old_id, old_id)
        },
    }
}

fn should_merge<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &mut World<ItemIdxType, RecipeIdxType>,
    simulation_state: &SimulationState<ItemIdxType, RecipeIdxType>,
    self_dir: Dir,
    front_pos: Position,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> Option<BeltTileId<ItemIdxType>> {
    let front_belt_dir = get_belt_in_dir(&world, front_pos, data_store);

    let front_expected_state = front_belt_dir.map(|front_belt_dir| {
        let mut inputs_dirs = world.get_belt_possible_inputs(front_pos).clone();
        inputs_dirs[self_dir.reverse()] = true;
        (
            expected_belt_state(front_belt_dir, inputs_dirs),
            front_belt_dir,
        )
    });

    if let Some((front_belt_state, front_belt_dir)) = front_expected_state {
        match front_belt_state {
            BeltState::Straight | BeltState::Sideloading | BeltState::DoubleSideloading => {
                if front_belt_dir == self_dir {
                    match world.get_entity_at(front_pos, data_store).unwrap() {
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
                        Entity::Splitter {
                            pos,
                            id,
                            direction: splitter_dir,
                            ..
                        } => {
                            let mut side = if front_pos == *pos {
                                SplitterSide::Left
                            } else {
                                SplitterSide::Right
                            };

                            match splitter_dir {
                                Dir::North | Dir::East => {},
                                Dir::South | Dir::West => side = side.switch(),
                            }

                            // TODO: Only calculate what we need
                            let [inputs, _] =
                                simulation_state.factory.belts.get_splitter_belt_ids(*id);

                            Some(inputs[usize::from(bool::from(side))])
                        },
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
                        match world.get_entity_at(front_pos, data_store).unwrap() {
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
    world: &mut World<ItemIdxType, RecipeIdxType>,
    self_dir: Dir,
    front_pos: Position,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> Option<(BeltTileId<ItemIdxType>, u16)> {
    let front_belt_dir = get_belt_dir_for_sideloading(&world, front_pos, data_store);

    let front_expected_state = front_belt_dir.map(|front_belt_dir| {
        let mut inputs_dirs = world.get_belt_possible_inputs(front_pos).clone();
        inputs_dirs[self_dir.reverse()] = true;
        (
            expected_belt_state(front_belt_dir, inputs_dirs),
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
                        match world.get_entity_at(front_pos, data_store).unwrap() {
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

pub fn expected_belt_state(
    belt_dir: Dir,
    input_dirs: crate::get_size::EnumMap<Dir, bool>,
) -> BeltState {
    // let dirs = [Dir::North, Dir::East, Dir::South, Dir::West];
    // let input_dirs: EnumMap<Dir, bool> = EnumMap::from_array(dirs.map(gets_input_from));
    // Output dirs are unused for determining this, interesting!
    // let output_dirs: Vec<Dir> = Dir::iter().filter(|dir| dir_info(*dir) == Some(BeltDir::Ouput)).collect();

    match input_dirs.iter().filter(|(_, v)| **v).count() {
        0 => BeltState::Straight,

        1 => {
            if input_dirs[belt_dir] || input_dirs[belt_dir.reverse()] {
                BeltState::Straight
            } else {
                BeltState::Curved
            }
        },

        2 => {
            if input_dirs[belt_dir] == input_dirs[belt_dir.reverse()] {
                // The inputs are opposite
                if input_dirs[belt_dir] {
                    // The inputs are front and back
                    BeltState::Straight
                } else {
                    // The inputs are left and right
                    BeltState::DoubleSideloading
                }
            } else {
                // The inputs are at 90 deg
                if input_dirs[belt_dir.reverse()] {
                    // One of the belts points into our input dir
                    BeltState::Sideloading
                } else {
                    BeltState::Curved
                }
            }
        },

        3 => {
            if !input_dirs[belt_dir] || !input_dirs[belt_dir.reverse()] {
                BeltState::DoubleSideloading
            } else {
                BeltState::Sideloading
            }
        },

        4 => BeltState::DoubleSideloading,

        _ => unreachable!(),
    }
}

// #[cfg(test)]
// mod test {
//     use proptest::prelude::{Just, Strategy};
//     use proptest::{prop_assert, prop_assume, proptest};

//     use crate::DATA_STORE;
//     use crate::app_state::GameState;
//     use crate::blueprint::Blueprint;
//     use crate::frontend::action::ActionType;
//     use crate::frontend::action::set_recipe::SetRecipeInfo;
//     use crate::frontend::world::Position;
//     use crate::frontend::world::tile::{AssemblerInfo, Dir, Entity, InserterInfo, PlaceEntityType};
//     use crate::item::Recipe;

//     fn chest_onto_belt() -> impl Strategy<Value = Vec<ActionType<u8, u8>>> {
//         Just(vec![
//             place(PlaceEntityType::Assembler {
//                 pos: Position { x: 0, y: 3 },
//                 ty: 0,
//                 rotation: Dir::North,
//             }),
//             ActionType::SetRecipe(SetRecipeInfo {
//                 pos: Position { x: 0, y: 3 },
//                 recipe: Recipe { id: 0 },
//             }),
//             place(PlaceEntityType::Inserter {
//                 pos: Position { x: 2, y: 2 },
//                 dir: crate::frontend::world::tile::Dir::North,
//                 filter: None,
//                 ty: 0,
//                 user_movetime: None,
//             }),
//             place(PlaceEntityType::Belt {
//                 pos: Position { x: 2, y: 1 },
//                 direction: crate::frontend::world::tile::Dir::East,
//                 ty: 0,
//             }),
//             place(PlaceEntityType::PowerPole {
//                 pos: Position { x: 0, y: 2 },
//                 ty: 0,
//             }),
//             place(PlaceEntityType::PowerPole {
//                 pos: Position { x: 5, y: 0 },
//                 ty: 0,
//             }),
//             place(PlaceEntityType::SolarPanel {
//                 pos: Position { x: 6, y: 0 },
//                 ty: 0,
//             }),
//         ])
//     }

//     fn belts_into_sideload() -> impl Strategy<Value = Vec<ActionType<u8, u8>>> {
//         Just(vec![
//             place(PlaceEntityType::Belt {
//                 pos: Position { x: 3, y: 1 },
//                 direction: crate::frontend::world::tile::Dir::East,
//                 ty: 0,
//             }),
//             place(PlaceEntityType::Belt {
//                 pos: Position { x: 4, y: 0 },
//                 direction: crate::frontend::world::tile::Dir::South,
//                 ty: 0,
//             }),
//             place(PlaceEntityType::Belt {
//                 pos: Position { x: 4, y: 1 },
//                 direction: crate::frontend::world::tile::Dir::South,
//                 ty: 0,
//             }),
//             place(PlaceEntityType::Belt {
//                 pos: Position { x: 4, y: 2 },
//                 direction: crate::frontend::world::tile::Dir::South,
//                 ty: 0,
//             }),
//         ])
//     }

//     fn sideload_items() -> impl Strategy<Value = Vec<ActionType<u8, u8>>> {
//         (chest_onto_belt(), belts_into_sideload()).prop_map(|(mut a, b)| {
//             a.extend(b.into_iter());
//             a
//         })
//     }

//     fn place(ty: PlaceEntityType<u8>) -> ActionType<u8, u8> {
//         ActionType::PlaceEntity(crate::frontend::action::place_entity::PlaceEntityInfo {
//             entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(ty),
//             force: false,
//         })
//     }

//     proptest! {

//         #[test]
//         fn inserter_always_attaches(actions in chest_onto_belt().prop_shuffle()) {
//             let mut state = GameState::new( &DATA_STORE);

//             let bp = Blueprint { actions };

//             bp.apply(false, Position { x: 1600, y: 1600 }, &mut state, &DATA_STORE);

//             let ent = state.world.get_entity_at(Position { x: 1600, y: 1603 }, &DATA_STORE).unwrap();

//             let assembler_powered = matches!(ent, Entity::Assembler { info: AssemblerInfo::Powered { .. } |  AssemblerInfo::PoweredNoRecipe { .. }, .. });

//             prop_assert!(assembler_powered);

//             let assembler_working = matches!(ent, Entity::Assembler { info: AssemblerInfo::Powered { .. }, .. });

//             prop_assume!(assembler_working, "{:?}", ent);

//             let ent = state.world.get_entity_at(Position { x: 1602, y: 1602 }, &DATA_STORE).unwrap();

//             let inserter_attached = matches!(ent, Entity::Inserter { info: InserterInfo::Attached { .. }, .. });

//             prop_assert!(inserter_attached, "{:?}", ent);
//         }

//         #[test]
//         fn inserter_always_attaches_full_bp(actions in sideload_items().prop_shuffle()) {
//             let mut state = GameState::new(&DATA_STORE);

//             let bp = Blueprint { actions };

//             bp.apply(false, Position { x: 1600, y: 1600 }, &mut state, &DATA_STORE);

//             let ent = state.world.get_entity_at(Position { x: 1600, y: 1603 }, &DATA_STORE).unwrap();

//             let assembler_powered = matches!(ent, Entity::Assembler { info: AssemblerInfo::Powered { .. } |  AssemblerInfo::PoweredNoRecipe { .. }, .. });

//             prop_assert!(assembler_powered);

//             let assembler_working = matches!(ent, Entity::Assembler { info: AssemblerInfo::Powered { .. }, .. });

//             prop_assume!(assembler_working, "{:?}", ent);

//             let ent = state.world.get_entity_at(Position { x: 1602, y: 1602 }, &DATA_STORE).unwrap();

//             let inserter_attached = matches!(ent, Entity::Inserter { info: InserterInfo::Attached { .. }, .. });

//             prop_assert!(inserter_attached, "{:?}", ent);
//         }

//         #[test]
//         fn sideload_empty_does_not_crash(actions in belts_into_sideload().prop_shuffle()) {
//             let mut state = GameState::new(&DATA_STORE);

//             let bp = Blueprint { actions };

//             bp.apply(false, Position { x: 1600, y: 1600 }, &mut state, &DATA_STORE);
//         }

//         #[test]
//         fn sideload_with_items_at_source_does_not_crash(actions in sideload_items().prop_shuffle()) {
//             let mut state = GameState::new(&DATA_STORE);

//             let bp = Blueprint { actions };

//             bp.apply(false, Position { x: 1600, y: 1600 }, &mut state, &DATA_STORE);
//         }

//         #[test]
//         fn sideload_with_items_at_source_items_reach_the_intersection(actions in chest_onto_belt().prop_shuffle()) {
//             let mut state = GameState::new( &DATA_STORE);

//             let bp = Blueprint { actions };

//             bp.apply(false, Position { x: 1600, y: 1600 }, &mut state, &DATA_STORE);

//             let assembler = state.world.get_entity_at(Position { x: 1600, y: 1603 }, &DATA_STORE).unwrap();

//             let assembler_working = matches!(assembler, Entity::Assembler { info: AssemblerInfo::Powered { .. }, .. });

//             prop_assume!(assembler_working, "{:?}", assembler);

//             let ent = state.world.get_entity_at(Position { x: 1602, y: 1602 }, &DATA_STORE).unwrap();

//             let inserter_attached = matches!(ent, Entity::Inserter { info: InserterInfo::Attached { .. }, .. });

//             prop_assume!(inserter_attached, "{:?}", ent);

//             for _ in 0..200 {
//                 state.update(&DATA_STORE);
//             }

//             let Some(Entity::Belt { id, .. }) = state.world.get_entity_at(Position { x: 1602, y: 1601 }, &DATA_STORE) else {
//                 unreachable!()
//             };

//             let items_at_intersection = state.simulation_state.factory.belts.get_item_iter(*id).into_iter().next().expect(&format!("{:?}", state.simulation_state.factory.belts.get_item_iter(*id).into_iter().collect::<Vec<_>>())).is_some();

//             prop_assert!(state.statistics.production.total.unwrap().items_produced.iter().copied().sum::<u64>() > 0);

//             prop_assert!(items_at_intersection, "{:?}, \n{:?}", state.simulation_state.factory.belts, state.simulation_state.factory.belts.get_item_iter(*id).into_iter().collect::<Vec<_>>());
//         }

//         #[test]
//         fn sideload_with_items_at_source_items_actually_reach(actions in sideload_items().prop_shuffle()) {
//             let mut state = GameState::new(&DATA_STORE);

//             let bp = Blueprint { actions };

//             bp.apply(false, Position { x: 1600, y: 1600 }, &mut state, &DATA_STORE);

//             let ent = state.world.get_entity_at(Position { x: 1600, y: 1603 }, &DATA_STORE).unwrap();

//             let assembler_powered = matches!(ent, Entity::Assembler { info: AssemblerInfo::Powered { .. } |  AssemblerInfo::PoweredNoRecipe { .. }, .. });

//             prop_assume!(assembler_powered);

//             let assembler_working = matches!(ent, Entity::Assembler { info: AssemblerInfo::Powered { .. }, .. });

//             prop_assume!(assembler_working, "{:?}", ent);

//             let inserter_attached = matches!(state.world.get_entity_at(Position { x: 1602, y: 1602 }, &DATA_STORE).unwrap(), Entity::Inserter { info: InserterInfo::Attached { .. }, .. });

//             prop_assume!(inserter_attached);

//             for _ in 0..2000 {
//                 state.update(&DATA_STORE);
//             }

//             let Some(Entity::Belt { id: id_going_right, .. }) = state.world.get_entity_at(Position { x: 1602, y: 1601 }, &DATA_STORE) else {
//                 unreachable!()
//             };

//             let Some(Entity::Belt { id: id_going_down, .. }) = state.world.get_entity_at(Position { x: 1604, y: 1602 }, &DATA_STORE) else {
//                 unreachable!()
//             };

//             let produced = state.statistics.production.total.unwrap().items_produced.iter().copied().sum::<u64>();

//             prop_assume!(produced > 0, "{:?}", produced);

//             prop_assert!(dbg!(state.simulation_state.factory.belts.get_item_iter(*id_going_down).into_iter().next().unwrap()).is_some(),"down: {:?}\n, right:{:?}", state.simulation_state.factory.belts.get_item_iter(*id_going_down).into_iter().collect::<Vec<_>>(), state.simulation_state.factory.belts.get_item_iter(*id_going_right).into_iter().collect::<Vec<_>>());
//         }

//     }
// }
