use std::iter::successors;

use itertools::Itertools;
use strum::IntoEnumIterator;

use crate::{
    belt::{
        BeltTileId, SplitterInfo,
        smart::Side,
        splitter::{SPLITTER_BELT_LEN, SplitterDistributionMode, SplitterSide},
    },
    data::DataStore,
    frontend::world::{
        Position,
        tile::{BELT_LEN_PER_TILE, Dir, DirRelative, Entity, UndergroundDir, World},
    },
    item::IdxTrait,
    rendering::app_state::{GameState, SimulationState},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BeltState {
    Straight,
    Curved,
    Sideloading,
    DoubleSideloading,
}

pub fn handle_splitter_placement<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
    splitter_pos: Position,
    splitter_direction: Dir,
    splitter_ty: u8,
    in_mode: Option<SplitterDistributionMode>,
    out_mode: Option<SplitterDistributionMode>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
    #[cfg(debug_assertions)]
    {
        let all_belt_connections = game_state
            .world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .flat_map(|e| match e {
                Entity::Belt {
                    pos,
                    direction,
                    ty,
                    id,
                    belt_pos,
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
            if let Some(Entity::Splitter { pos, direction, id }) = game_state
                .world
                .get_entities_colliding_with(belt_pos + check_dir, (1, 1), data_store)
                .into_iter()
                .next()
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
            handle_belt_breaking(game_state, front_pos, splitter_direction, data_store);

            // Handle front
            let (self_front_id, self_front_len) = if let Some(id) =
                should_merge(game_state, splitter_direction, front_pos, data_store)
            {
                debug_assert!(
                    should_sideload(game_state, splitter_direction, front_pos, data_store)
                        .is_none()
                );
                let result = lengthen(game_state, id, SPLITTER_BELT_LEN, Side::BACK);
                (id, result.new_belt_len)
            } else if let Some((id, pos)) =
                should_sideload(game_state, splitter_direction, front_pos, data_store)
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
            dbg!(back_pos);
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
                            underground_dir: UndergroundDir::Exit,
                            id,
                            ..
                        } => *id,
                        Entity::Splitter { .. } => todo!("get the id from the simstate"),
                        e => unreachable!("{:?}", e),
                    };

                    let result = lengthen(game_state, back_id, SPLITTER_BELT_LEN, Side::FRONT);

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

    game_state.world.add_entity(
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
    );

    #[cfg(debug_assertions)]
    {
        let all_belt_connections = game_state
            .world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .flat_map(|e| match e {
                Entity::Belt {
                    pos,
                    direction,
                    ty,
                    id,
                    belt_pos,
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
            if let Some(Entity::Splitter { pos, direction, id }) = game_state
                .world
                .get_entities_colliding_with(belt_pos + check_dir, (1, 1), data_store)
                .into_iter()
                .next()
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

#[allow(unreachable_code)]
pub fn handle_belt_placement<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
    new_belt_pos: Position,
    new_belt_direction: Dir,
    new_belt_ty: u8,
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
                    ty: new_belt_ty,
                    id,
                    belt_pos: result.belt_pos_of_segment,
                },
                &mut game_state.simulation_state,
                data_store,
            );
            (id, result.new_belt_len)
        } else if let Some((id, pos)) =
            should_sideload(game_state, new_belt_direction, front_pos, data_store)
        {
            // Add new belt
            let new_belt = game_state
                .simulation_state
                .factory
                .belts
                .add_empty_belt(new_belt_ty, BELT_LEN_PER_TILE);
            // Add Sideloading inserter
            game_state
                .simulation_state
                .factory
                .belts
                .add_sideloading_inserter(new_belt, (id, pos));

            game_state.world.add_entity(
                Entity::Belt {
                    pos: new_belt_pos,
                    direction: new_belt_direction,
                    ty: new_belt_ty,
                    id: new_belt,
                    belt_pos: BELT_LEN_PER_TILE,
                },
                &mut game_state.simulation_state,
                data_store,
            );
            (new_belt, BELT_LEN_PER_TILE)
        } else {
            let id = game_state
                .simulation_state
                .factory
                .belts
                .add_empty_belt(new_belt_ty, BELT_LEN_PER_TILE);
            game_state.world.add_entity(
                Entity::Belt {
                    pos: new_belt_pos,
                    direction: new_belt_direction,
                    ty: new_belt_ty,
                    id,
                    belt_pos: BELT_LEN_PER_TILE,
                },
                &mut game_state.simulation_state,
                data_store,
            );
            (id, BELT_LEN_PER_TILE)
        };

    for dir in Dir::iter() {
        if dir.reverse() == new_belt_direction {
            continue;
        }
        let potentially_incoming_pos = new_belt_pos + dir.reverse();
        let belt_dir = get_belt_out_dir(&game_state.world, potentially_incoming_pos, data_store);
        if let Some(belt_dir) = belt_dir {
            if belt_dir != dir {
                // The belt at this position is not pointing at the new belt
                continue;
            }

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

                        let [_, outputs] = game_state
                            .simulation_state
                            .factory
                            .belts
                            .get_splitter_belt_ids(*id);

                        outputs[usize::from(bool::from(side))]
                    },
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
                    Entity::Splitter { .. } => todo!(),
                    _ => unreachable!(),
                };

                // Add Sideloading inserter
                game_state
                    .simulation_state
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
        let all_belt_connections = game_state
            .world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .flat_map(|e| match e {
                Entity::Belt {
                    pos,
                    direction,
                    ty,
                    id,
                    belt_pos,
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
            if let Some(Entity::Splitter { pos, direction, id }) = game_state
                .world
                .get_entities_colliding_with(belt_pos + check_dir, (1, 1), data_store)
                .into_iter()
                .next()
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
}

#[allow(unreachable_code)]
pub fn handle_underground_belt_placement<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
    new_belt_pos: Position,
    new_belt_direction: Dir,
    new_belt_ty: u8,
    new_underground_dir: UndergroundDir,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) {
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
                        let e = game_state
                            .world
                            .get_entities_colliding_with(check_pos, (1, 1), data_store)
                            .into_iter()
                            .next();

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
                } = e
                else {
                    unreachable!()
                };

                match underground_dir {
                    UndergroundDir::Entrance => {
                        // The first underground we found was another Entrance, so we do not need to connect, nor break anything

                        let id = game_state
                            .simulation_state
                            .factory
                            .belts
                            .add_empty_belt(new_belt_ty, BELT_LEN_PER_TILE);
                        game_state.world.add_entity(
                            Entity::Underground {
                                pos: new_belt_pos,
                                direction: new_belt_direction,
                                ty: new_belt_ty,
                                underground_dir: new_underground_dir,
                                id,
                                belt_pos: BELT_LEN_PER_TILE,
                            },
                            &mut game_state.simulation_state,
                            data_store,
                        );
                        (id, BELT_LEN_PER_TILE)
                    },
                    UndergroundDir::Exit => {
                        if *belt_pos == game_state.simulation_state.factory.belts.get_len(*id) {
                            // This is the start of the belt
                            // no breaking necessary
                        } else {
                            todo!("Break its connection");
                        }

                        let exit_id: BeltTileId<ItemIdxType> = *id;

                        // Add the "underground" belt locs
                        let res = lengthen(
                            game_state,
                            *id,
                            // Underground
                            BELT_LEN_PER_TILE
                                * u16::try_from(
                                    new_belt_pos.x.abs_diff(pos.x) + new_belt_pos.y.abs_diff(pos.y),
                                )
                                .unwrap() +
                                // The newly placed tile itself
                                BELT_LEN_PER_TILE,
                            Side::BACK,
                        );

                        game_state.world.add_entity(
                            Entity::Underground {
                                pos: new_belt_pos,
                                direction: new_belt_direction,
                                ty: new_belt_ty,
                                underground_dir: new_underground_dir,
                                id: exit_id,
                                belt_pos: res.new_belt_len,
                            },
                            &mut game_state.simulation_state,
                            data_store,
                        );

                        (exit_id, res.new_belt_len)
                    },
                }
            } else {
                // No underground found to connect to
                let id = game_state
                    .simulation_state
                    .factory
                    .belts
                    .add_empty_belt(new_belt_ty, BELT_LEN_PER_TILE);
                game_state.world.add_entity(
                    Entity::Underground {
                        pos: new_belt_pos,
                        direction: new_belt_direction,
                        ty: new_belt_ty,
                        underground_dir: new_underground_dir,
                        id,
                        belt_pos: BELT_LEN_PER_TILE,
                    },
                    &mut game_state.simulation_state,
                    data_store,
                );
                (id, BELT_LEN_PER_TILE)
            };

            // An underground Entrance cannot break any belt
            // should_merge does not work if the entity at "front_pos" is not in the world yet
            if let Some(id) = should_merge(game_state, new_belt_direction, new_belt_pos, data_store)
            {
                assert_eq!(self_id, id);
                debug_assert!(
                    should_sideload(game_state, new_belt_direction, new_belt_pos, data_store)
                        .is_none()
                );

                let potentially_incoming_pos = new_belt_pos + new_belt_direction.reverse();

                let back_id = match game_state
                    .world
                    .get_entities_colliding_with(potentially_incoming_pos, (1, 1), data_store)
                    .into_iter()
                    .next()
                {
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
                    game_state
                        .world
                        .modify_belt_pos(back_id, self_len.try_into().unwrap());
                    let (final_id, final_len) = merge_belts(
                        &mut game_state.simulation_state,
                        self_id,
                        back_id,
                        data_store,
                    );
                    if final_id == self_id {
                        game_state.world.update_belt_id(back_id, final_id);
                    } else {
                        game_state.world.update_belt_id(self_id, final_id);
                    }
                    (final_id, final_len)
                } else {
                    (self_id, self_len)
                }
            } else if let Some((id, pos)) =
                should_sideload(game_state, new_belt_direction, new_belt_pos, data_store)
            {
                unreachable!()
            } else {
                (self_id, self_len)
            };
        },
        UndergroundDir::Exit => {
            let front_pos = new_belt_pos + new_belt_direction;

            handle_belt_breaking(game_state, front_pos, new_belt_direction, data_store);

            // Handle front tile
            let (self_id, self_len) = if let Some(id) =
                should_merge(game_state, new_belt_direction, front_pos, data_store)
            {
                debug_assert!(
                    should_sideload(game_state, new_belt_direction, front_pos, data_store)
                        .is_none()
                );
                let result = lengthen(game_state, id, BELT_LEN_PER_TILE, Side::BACK);
                game_state.world.add_entity(
                    Entity::Underground {
                        pos: new_belt_pos,
                        underground_dir: new_underground_dir,
                        direction: new_belt_direction,
                        ty: new_belt_ty,
                        id,
                        belt_pos: result.belt_pos_of_segment,
                    },
                    &mut game_state.simulation_state,
                    data_store,
                );
                (id, result.new_belt_len)
            } else if let Some((id, pos)) =
                should_sideload(game_state, new_belt_direction, front_pos, data_store)
            {
                // Add new belt
                let new_belt = game_state
                    .simulation_state
                    .factory
                    .belts
                    .add_empty_belt(new_belt_ty, BELT_LEN_PER_TILE);
                // Add Sideloading inserter
                game_state
                    .simulation_state
                    .factory
                    .belts
                    .add_sideloading_inserter(new_belt, (id, pos));

                game_state.world.add_entity(
                    Entity::Underground {
                        pos: new_belt_pos,
                        underground_dir: new_underground_dir,
                        direction: new_belt_direction,
                        ty: new_belt_ty,
                        id,
                        belt_pos: BELT_LEN_PER_TILE,
                    },
                    &mut game_state.simulation_state,
                    data_store,
                );
                (new_belt, BELT_LEN_PER_TILE)
            } else {
                let id = game_state
                    .simulation_state
                    .factory
                    .belts
                    .add_empty_belt(new_belt_ty, BELT_LEN_PER_TILE);
                game_state.world.add_entity(
                    Entity::Underground {
                        pos: new_belt_pos,
                        direction: new_belt_direction,
                        ty: new_belt_ty,
                        underground_dir: new_underground_dir,
                        id,
                        belt_pos: BELT_LEN_PER_TILE,
                    },
                    &mut game_state.simulation_state,
                    data_store,
                );
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
                    should_merge(game_state, dir.reverse(), new_belt_pos, data_store).is_none()
                );

                if let Some((id, belt_pos)) =
                    should_sideload(game_state, dir.reverse(), new_belt_pos, data_store)
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
                        Entity::Splitter { .. } => todo!(),
                        e => unreachable!("{:?}", e),
                    };

                    // Add Sideloading inserter
                    game_state
                        .simulation_state
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
                let e = game_state
                    .world
                    .get_entities_colliding_with(check_pos, (1, 1), data_store)
                    .into_iter()
                    .next();

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
                } = e
                else {
                    unreachable!()
                };

                match underground_dir {
                    UndergroundDir::Entrance => {
                        if *belt_pos == BELT_LEN_PER_TILE {
                            // This is the end of the belt
                            // no breaking necessary
                        } else {
                            todo!("Break its connection");
                        }

                        let entrance_id: BeltTileId<ItemIdxType> = *id;

                        // Add the "underground" belt locs
                        let res = lengthen(
                            game_state,
                            self_id,
                            BELT_LEN_PER_TILE
                                * u16::try_from(
                                    new_belt_pos.x.abs_diff(pos.x) + new_belt_pos.y.abs_diff(pos.y),
                                )
                                .unwrap(),
                            Side::BACK,
                        );

                        // Connect them together
                        let (new_id, new_len) = merge_belts(
                            &mut game_state.simulation_state,
                            self_id,
                            entrance_id,
                            data_store,
                        );

                        if self_id != entrance_id {
                            game_state
                                .world
                                .modify_belt_pos(entrance_id, res.new_belt_len.try_into().unwrap());
                        }
                        if new_id == self_id {
                            game_state.world.update_belt_id(entrance_id, new_id);
                        } else {
                            game_state.world.update_belt_id(self_id, new_id);
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
        let all_belt_connections = game_state
            .world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .flat_map(|e| match e {
                Entity::Belt {
                    pos,
                    direction,
                    ty,
                    id,
                    belt_pos,
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
            if let Some(Entity::Splitter { pos, direction, id }) = game_state
                .world
                .get_entities_colliding_with(belt_pos + check_dir, (1, 1), data_store)
                .into_iter()
                .next()
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
        (BeltState::Curved, BeltState::Sideloading)
        | (BeltState::Curved, BeltState::DoubleSideloading) => {
            let entity = game_state
                .world
                .get_entities_colliding_with(pos_which_might_break, (1, 1), data_store)
                .into_iter()
                .next()
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

            let res = game_state
                .simulation_state
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
                            game_state.world.update_belt_id_after(
                                res.kept_id,
                                new_id,
                                belt_pos_to_break_at + 1,
                            );
                            game_state.world.modify_belt_pos(
                                new_id,
                                -i16::try_from(belt_pos_to_break_at).unwrap(),
                            );

                            let new_len = game_state.simulation_state.factory.belts.get_len(new_id);

                            game_state
                                .simulation_state
                                .factory
                                .belts
                                .add_sideloading_inserter(
                                    new_id,
                                    (res.kept_id, belt_pos_to_break_at - 1),
                                );
                        },
                    }
                },
                None => todo!("The belt stopped being circular. What to do?"),
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
        let all_belt_connections = game_state
            .world
            .get_chunks()
            .flat_map(|chunk| chunk.get_entities())
            .flat_map(|e| match e {
                Entity::Belt {
                    pos,
                    direction,
                    ty,
                    id,
                    belt_pos,
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
            if let Some(Entity::Splitter { pos, direction, id }) = game_state
                .world
                .get_entities_colliding_with(belt_pos + check_dir, (1, 1), data_store)
                .into_iter()
                .next()
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

#[derive(Debug, Clone, Copy)]
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
                            let [inputs, _] = game_state
                                .simulation_state
                                .factory
                                .belts
                                .get_splitter_belt_ids(*id);

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

pub fn expected_belt_state(belt_dir: Dir, gets_input_from: impl FnMut(&Dir) -> bool) -> BeltState {
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
mod test {
    use proptest::prelude::{Just, Strategy};
    use proptest::{prop_assert, prop_assume, proptest};

    use crate::DATA_STORE;
    use crate::blueprint::Blueprint;
    use crate::frontend::action::ActionType;
    use crate::frontend::action::set_recipe::SetRecipeInfo;
    use crate::frontend::world::Position;
    use crate::frontend::world::tile::{AssemblerInfo, Dir, Entity, InserterInfo, PlaceEntityType};
    use crate::item::Recipe;
    use crate::rendering::app_state::GameState;

    fn chest_onto_belt() -> impl Strategy<Value = Vec<ActionType<u8, u8>>> {
        Just(vec![
            place(PlaceEntityType::Assembler {
                pos: Position { x: 0, y: 3 },
                ty: 0,
                rotation: Dir::North,
            }),
            ActionType::SetRecipe(SetRecipeInfo {
                pos: Position { x: 0, y: 3 },
                recipe: Recipe { id: 0 },
            }),
            place(PlaceEntityType::Inserter {
                pos: Position { x: 2, y: 2 },
                dir: crate::frontend::world::tile::Dir::North,
                filter: None,
            }),
            place(PlaceEntityType::Belt {
                pos: Position { x: 2, y: 1 },
                direction: crate::frontend::world::tile::Dir::East,
                ty: 0,
            }),
            place(PlaceEntityType::PowerPole {
                pos: Position { x: 0, y: 2 },
                ty: 0,
            }),
            place(PlaceEntityType::PowerPole {
                pos: Position { x: 5, y: 0 },
                ty: 0,
            }),
            place(PlaceEntityType::SolarPanel {
                pos: Position { x: 6, y: 0 },
                ty: 0,
            }),
        ])
    }

    fn belts_into_sideload() -> impl Strategy<Value = Vec<ActionType<u8, u8>>> {
        Just(vec![
            place(PlaceEntityType::Belt {
                pos: Position { x: 3, y: 1 },
                direction: crate::frontend::world::tile::Dir::East,
                ty: 0,
            }),
            place(PlaceEntityType::Belt {
                pos: Position { x: 4, y: 0 },
                direction: crate::frontend::world::tile::Dir::South,
                ty: 0,
            }),
            place(PlaceEntityType::Belt {
                pos: Position { x: 4, y: 1 },
                direction: crate::frontend::world::tile::Dir::South,
                ty: 0,
            }),
            place(PlaceEntityType::Belt {
                pos: Position { x: 4, y: 2 },
                direction: crate::frontend::world::tile::Dir::South,
                ty: 0,
            }),
        ])
    }

    fn sideload_items() -> impl Strategy<Value = Vec<ActionType<u8, u8>>> {
        (chest_onto_belt(), belts_into_sideload()).prop_map(|(mut a, b)| {
            a.extend(b.into_iter());
            a
        })
    }

    fn place(ty: PlaceEntityType<u8>) -> ActionType<u8, u8> {
        ActionType::PlaceEntity(crate::frontend::action::place_entity::PlaceEntityInfo {
            entities: crate::frontend::action::place_entity::EntityPlaceOptions::Single(ty),
        })
    }

    proptest! {

        #[test]
        fn inserter_always_attaches(actions in chest_onto_belt().prop_shuffle()) {
            let mut state = GameState::new( &DATA_STORE);

            let bp = Blueprint { actions };

            bp.apply(Position { x: 1600, y: 1600 }, &mut state, &DATA_STORE);

            let ent = state.world.get_entities_colliding_with(Position { x: 1600, y: 1603 }, (1, 1), &DATA_STORE).into_iter().next().unwrap();

            let assembler_powered = matches!(ent, Entity::Assembler { info: AssemblerInfo::Powered { .. } |  AssemblerInfo::PoweredNoRecipe { .. }, .. });

            prop_assert!(assembler_powered);

            let assembler_working = matches!(ent, Entity::Assembler { info: AssemblerInfo::Powered { .. }, .. });

            prop_assume!(assembler_working, "{:?}", ent);

            let ent = state.world.get_entities_colliding_with(Position { x: 1602, y: 1602 }, (1, 1), &DATA_STORE).into_iter().next().unwrap();

            let inserter_attached = matches!(ent, Entity::Inserter { info: InserterInfo::Attached { .. }, .. });

            prop_assert!(inserter_attached, "{:?}", ent);
        }

        #[test]
        fn inserter_always_attaches_full_bp(actions in sideload_items().prop_shuffle()) {
            let mut state = GameState::new(&DATA_STORE);

            let bp = Blueprint { actions };

            bp.apply(Position { x: 1600, y: 1600 }, &mut state, &DATA_STORE);

            let ent = state.world.get_entities_colliding_with(Position { x: 1600, y: 1603 }, (1, 1), &DATA_STORE).into_iter().next().unwrap();

            let assembler_powered = matches!(ent, Entity::Assembler { info: AssemblerInfo::Powered { .. } |  AssemblerInfo::PoweredNoRecipe { .. }, .. });

            prop_assert!(assembler_powered);

            let assembler_working = matches!(ent, Entity::Assembler { info: AssemblerInfo::Powered { .. }, .. });

            prop_assume!(assembler_working, "{:?}", ent);

            let ent = state.world.get_entities_colliding_with(Position { x: 1602, y: 1602 }, (1, 1), &DATA_STORE).into_iter().next().unwrap();

            let inserter_attached = matches!(ent, Entity::Inserter { info: InserterInfo::Attached { .. }, .. });

            prop_assert!(inserter_attached, "{:?}", ent);
        }

        #[test]
        fn sideload_empty_does_not_crash(actions in belts_into_sideload().prop_shuffle()) {
            let mut state = GameState::new(&DATA_STORE);

            let bp = Blueprint { actions };

            bp.apply(Position { x: 1600, y: 1600 }, &mut state, &DATA_STORE);
        }

        #[test]
        fn sideload_with_items_at_source_does_not_crash(actions in sideload_items().prop_shuffle()) {
            let mut state = GameState::new(&DATA_STORE);

            let bp = Blueprint { actions };

            bp.apply(Position { x: 1600, y: 1600 }, &mut state, &DATA_STORE);
        }

        #[test]
        fn sideload_with_items_at_source_items_reach_the_intersection(actions in chest_onto_belt().prop_shuffle()) {
            let mut state = GameState::new( &DATA_STORE);

            let bp = Blueprint { actions };

            bp.apply(Position { x: 1600, y: 1600 }, &mut state, &DATA_STORE);

            let assembler = state.world.get_entities_colliding_with(Position { x: 1600, y: 1603 }, (1, 1), &DATA_STORE).into_iter().next().unwrap();

            let assembler_working = matches!(assembler, Entity::Assembler { info: AssemblerInfo::Powered { .. }, .. });

            prop_assume!(assembler_working, "{:?}", assembler);

            let ent = state.world.get_entities_colliding_with(Position { x: 1602, y: 1602 }, (1, 1), &DATA_STORE).into_iter().next().unwrap();

            let inserter_attached = matches!(ent, Entity::Inserter { info: InserterInfo::Attached { .. }, .. });

            prop_assume!(inserter_attached, "{:?}", ent);

            for _ in 0..200 {
                state.update(&DATA_STORE);
            }

            let Some(Entity::Belt { id, .. }) = state.world.get_entities_colliding_with(Position { x: 1602, y: 1601 }, (1, 1), &DATA_STORE).into_iter().next() else {
                unreachable!()
            };

            let items_at_intersection = state.simulation_state.factory.belts.get_item_iter(*id).into_iter().next().expect(&format!("{:?}", state.simulation_state.factory.belts.get_item_iter(*id).into_iter().collect::<Vec<_>>())).is_some();

            prop_assert!(state.statistics.production.total.unwrap().items_produced.iter().copied().sum::<u64>() > 0);

            prop_assert!(items_at_intersection, "{:?}, \n{:?}", state.simulation_state.factory.belts, state.simulation_state.factory.belts.get_item_iter(*id).into_iter().collect::<Vec<_>>());
        }

        #[test]
        fn sideload_with_items_at_source_items_actually_reach(actions in sideload_items().prop_shuffle()) {
            let mut state = GameState::new(&DATA_STORE);

            let bp = Blueprint { actions };

            bp.apply(Position { x: 1600, y: 1600 }, &mut state, &DATA_STORE);

            let ent = state.world.get_entities_colliding_with(Position { x: 1600, y: 1603 }, (1, 1), &DATA_STORE).into_iter().next().unwrap();

            let assembler_powered = matches!(ent, Entity::Assembler { info: AssemblerInfo::Powered { .. } |  AssemblerInfo::PoweredNoRecipe { .. }, .. });

            prop_assume!(assembler_powered);

            let assembler_working = matches!(ent, Entity::Assembler { info: AssemblerInfo::Powered { .. }, .. });

            prop_assume!(assembler_working, "{:?}", ent);

            let inserter_attached = matches!(state.world.get_entities_colliding_with(Position { x: 1602, y: 1602 }, (1, 1), &DATA_STORE).into_iter().next().unwrap(), Entity::Inserter { info: InserterInfo::Attached { .. }, .. });

            prop_assume!(inserter_attached);

            for _ in 0..2000 {
                state.update(&DATA_STORE);
            }

            let Some(Entity::Belt { id: id_going_right, .. }) = state.world.get_entities_colliding_with(Position { x: 1602, y: 1601 }, (1, 1), &DATA_STORE).into_iter().next() else {
                unreachable!()
            };

            let Some(Entity::Belt { id: id_going_down, .. }) = state.world.get_entities_colliding_with(Position { x: 1604, y: 1602 }, (1, 1), &DATA_STORE).into_iter().next() else {
                unreachable!()
            };

            let produced = state.statistics.production.total.unwrap().items_produced.iter().copied().sum::<u64>();

            prop_assume!(produced > 0, "{:?}", produced);

            prop_assert!(dbg!(state.simulation_state.factory.belts.get_item_iter(*id_going_down).into_iter().next().unwrap()).is_some(),"down: {:?}\n, right:{:?}", state.simulation_state.factory.belts.get_item_iter(*id_going_down).into_iter().collect::<Vec<_>>(), state.simulation_state.factory.belts.get_item_iter(*id_going_right).into_iter().collect::<Vec<_>>());
        }

    }
}
