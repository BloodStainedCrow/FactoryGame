use std::cmp::min;

use crate::data::{FluidConnection, PipeConnectionType};
use crate::frontend::world::tile::Dir;
use crate::{
    data::DataStore,
    frontend::world::{Position, tile::DirRelative},
    item::IdxTrait,
};

pub fn can_fluid_tanks_connect<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    first_tank_pos: Position,
    first_tank_ty: u8,
    first_rotation: Dir,
    second_tank_pos: Position,
    second_tank_ty: u8,
    second_rotation: Dir,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> Option<(Position, Dir)> {
    let first_size = data_store.fluid_tank_infos[usize::from(first_tank_ty)].size;
    let second_size = data_store.fluid_tank_infos[usize::from(second_tank_ty)].size;

    assert!(
        !first_tank_pos.overlap(first_size.into(), second_tank_pos, second_size.into()),
        "If these entities overlap, weird thing can happen"
    );

    for first_conn in &data_store.fluid_tank_infos[usize::from(first_tank_ty)].fluid_connections {
        for second_conn in
            &data_store.fluid_tank_infos[usize::from(second_tank_ty)].fluid_connections
        {
            if let Some((connection_pos, connection_dir)) = can_two_connections_connect(
                first_tank_pos,
                *first_conn,
                first_rotation,
                first_size,
                second_tank_pos,
                *second_conn,
                second_rotation,
                second_size,
            ) {
                return Some((connection_pos, connection_dir));
            }
        }
    }

    return None;
}

pub fn can_fluid_tanks_connect_to_single_connection<
    ItemIdxType: IdxTrait,
    RecipeIdxType: IdxTrait,
>(
    first_tank_pos: Position,
    first_tank_ty: u8,
    first_rotation: Dir,

    second_conn_pos: Position,
    second_conn: FluidConnection,
    second_conn_rotation: Dir,
    second_size: [u16; 2],
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> Option<(Position, Dir)> {
    let first_tank_size = data_store.fluid_tank_infos[usize::from(first_tank_ty)].size;

    for first_conn in &data_store.fluid_tank_infos[usize::from(first_tank_ty)].fluid_connections {
        if let Some(v) = can_two_connections_connect(
            first_tank_pos,
            *first_conn,
            first_rotation,
            first_tank_size,
            second_conn_pos,
            second_conn,
            second_conn_rotation,
            second_size,
        ) {
            return Some(v);
        }
    }

    return None;
}

fn can_two_connections_connect(
    first_conn_pos: Position,
    mut first_conn: FluidConnection,
    first_rotation: Dir,
    first_size: [u16; 2],
    second_conn_pos: Position,
    mut second_conn: FluidConnection,
    second_rotation: Dir,
    second_size: [u16; 2],
) -> Option<(Position, Dir)> {
    // FIXME: I effectively just made rotation matrices here xD
    match first_rotation {
        Dir::North => {},
        Dir::East => {
            first_conn.dir = first_conn.dir.turn_right();
            first_conn.offset = [
                first_size[0] as i16 - 1 - first_conn.offset[1],
                first_conn.offset[0],
            ];
        },
        Dir::South => {
            first_conn.dir = first_conn.dir.reverse();
            first_conn.offset = [
                first_size[0] as i16 - 1 - first_conn.offset[0],
                first_size[1] as i16 - 1 - first_conn.offset[1],
            ];
        },
        Dir::West => {
            first_conn.dir = first_conn.dir.turn_right().turn_right().turn_right();
            first_conn.offset = [
                first_conn.offset[1],
                first_size[0] as i16 - 1 - first_conn.offset[0],
            ];
        },
    }

    match second_rotation {
        Dir::North => {},
        Dir::East => {
            second_conn.dir = second_conn.dir.turn_right();
            second_conn.offset = [
                second_size[0] as i16 - 1 - second_conn.offset[1],
                second_conn.offset[0],
            ];
        },
        Dir::South => {
            second_conn.dir = second_conn.dir.reverse();
            second_conn.offset = [
                second_size[0] as i16 - 1 - second_conn.offset[0],
                second_size[1] as i16 - 1 - second_conn.offset[1],
            ];
        },
        Dir::West => {
            second_conn.dir = second_conn.dir.turn_right().turn_right().turn_right();
            second_conn.offset = [
                second_conn.offset[1],
                second_size[0] as i16 - 1 - second_conn.offset[0],
            ];
        },
    }

    match (first_conn.kind, second_conn.kind) {
        (PipeConnectionType::Direct, PipeConnectionType::Direct) => {
            // Ok
            let second_conn_pos = Position {
                x: second_conn_pos.x + i32::from(second_conn.offset[0]),
                y: second_conn_pos.y + i32::from(second_conn.offset[1]),
            };

            if (first_conn_pos + first_conn.dir) == second_conn_pos
                && (second_conn_pos + second_conn.dir) == first_conn_pos
            {
                assert!(first_conn.dir.compare(second_conn.dir) == DirRelative::Opposite);

                // We can connect via these connections
                return Some((second_conn_pos, second_conn.dir));
            }
        },
        (PipeConnectionType::Direct, PipeConnectionType::Underground { .. })
        | (PipeConnectionType::Underground { .. }, PipeConnectionType::Direct) => {
            // We can never connect, if the types differ
        },
        (
            PipeConnectionType::Underground {
                max_distance: first_max_distance,
                underground_group_mask: first_underground_group_mask,
            },
            PipeConnectionType::Underground {
                max_distance: second_max_distance,
                underground_group_mask: second_underground_group_mask,
            },
        ) => {
            if first_underground_group_mask & second_underground_group_mask != 0 {
                // The pipes are allowed to connect
                let second_conn_pos = Position {
                    x: second_conn_pos.x + i32::from(second_conn.offset[0]),
                    y: second_conn_pos.y + i32::from(second_conn.offset[1]),
                };

                let dist_x = second_conn_pos.x - first_conn_pos.x;
                let dist_y = second_conn_pos.y - first_conn_pos.y;

                if dist_x == 0 || dist_y == 0 {
                    // TODO: Make sure this is right
                    if dist_x + dist_y - 1
                        <= i32::from(min(first_max_distance, second_max_distance))
                    {
                        if first_conn.dir.compare(second_conn.dir) == DirRelative::Opposite {
                            return Some((second_conn_pos, second_conn.dir));
                        }
                    }
                } else {
                    // We do not line up, since the direction is diagonal
                }
            } else {
                // Pipe connection masks do not match
            }
        },
    }

    None
}
