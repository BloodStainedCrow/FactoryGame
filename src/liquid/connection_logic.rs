use crate::frontend::world::tile::Dir;
use crate::{
    data::DataStore,
    frontend::world::{tile::DirRelative, Position},
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
) -> bool {
    // FIXME: Stop ignoring rotation

    let first_size = data_store.fluid_tank_infos[usize::from(first_tank_ty)].size;
    let second_size = data_store.fluid_tank_infos[usize::from(second_tank_ty)].size;

    assert!(
        !first_tank_pos.overlap(first_size.into(), second_tank_pos, second_size.into()),
        "If these entities overlap, weird thing can happen"
    );

    for first_conn in &data_store.fluid_tank_infos[usize::from(first_tank_ty)].fluid_connections {
        let first_conn_pos = Position {
            x: first_tank_pos.x + i32::from(first_conn.offset[0]),
            y: first_tank_pos.y + i32::from(first_conn.offset[1]),
        };

        for second_conn in
            &data_store.fluid_tank_infos[usize::from(second_tank_ty)].fluid_connections
        {
            let second_conn_pos = Position {
                x: second_tank_pos.x + i32::from(second_conn.offset[0]),
                y: second_tank_pos.y + i32::from(second_conn.offset[1]),
            };

            if (first_conn_pos + first_conn.dir) == second_conn_pos
                && (second_conn_pos + second_conn.dir) == first_conn_pos
            {
                assert!(first_conn.dir.compare(second_conn.dir) == DirRelative::Opposite);

                // We can connect via these connections
                return true;
            }
        }
    }

    return false;
}

pub fn can_fluid_tanks_connect_to_single_connection<
    ItemIdxType: IdxTrait,
    RecipeIdxType: IdxTrait,
>(
    first_tank_pos: Position,
    first_tank_ty: u8,
    first_rotation: Dir,

    conn_pos: Position,
    conn_dir: Dir,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> bool {
    // FIXME: Stop ignoring rotation

    for first_conn in &data_store.fluid_tank_infos[usize::from(first_tank_ty)].fluid_connections {
        let first_conn_pos = Position {
            x: first_tank_pos.x + i32::from(first_conn.offset[0]),
            y: first_tank_pos.y + i32::from(first_conn.offset[1]),
        };

        if (first_conn_pos + first_conn.dir) == conn_pos && (conn_pos + conn_dir) == first_conn_pos
        {
            assert!(first_conn.dir.compare(conn_dir) == DirRelative::Opposite);

            // We can connect via these connections
            return true;
        }
    }

    return false;
}
