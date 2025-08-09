use std::{
    cmp::min,
    time::{Duration, Instant},
};

use egui::Color32;
use itertools::Itertools;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use tilelib::types::{DrawInstance, Layer, RendererTrait};

use crate::{
    frontend::world::{Position, tile::World},
    item::IdxTrait,
};

pub struct MapViewUpdate {
    pub pos: Position,
    pub color: Color32,
}

const NUM_MAP_TILE_SIZES: usize = 4;
// TODO: Figure out a good tilesize. 1024 seems to work fine, but is larger or smaller better?
const TILE_SIZE_PIXELS: [usize; NUM_MAP_TILE_SIZES] = [1024, 1024, 1024, 1024];
// TODO: Since array::map is not const, we hack it like this
const NUM_TILES_PER_AXIS: [usize; NUM_MAP_TILE_SIZES] = {
    let mut b = [0; NUM_MAP_TILE_SIZES];
    let mut i = 0;
    while i < NUM_MAP_TILE_SIZES {
        b[i] = 2_000_000usize.div_ceil(TILE_SIZE_PIXELS[i]); // map
        i += 1;
    }
    b
};
const TILE_PIXEL_TO_WORLD_TILE: [usize; NUM_MAP_TILE_SIZES] = [1, 4, 16, 64];
const MIN_WIDTH: [usize; NUM_MAP_TILE_SIZES] = [0, 5_000, 10_000, 50_000];

#[profiling::function]
pub fn create_map_textures_if_needed<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &World<ItemIdxType, RecipeIdxType>,
    renderer: &mut impl RendererTrait,
    camera_pos: Position,
    view_width_in_tiles: usize,
    view_height_in_tiles: usize,
    allowed_time: Option<Duration>,
    data_store: &crate::data::DataStore<ItemIdxType, RecipeIdxType>,
) {
    let start_time = Instant::now();
    let (map_tile_size, pixel_to_tile, num_per_axis, texture_id_offset) = {
        let idx = MIN_WIDTH
            .iter()
            .filter(|min| **min < view_width_in_tiles)
            .count()
            - 1;
        (
            TILE_SIZE_PIXELS[idx],
            TILE_PIXEL_TO_WORLD_TILE[idx],
            NUM_TILES_PER_AXIS[idx],
            NUM_TILES_PER_AXIS[0..idx]
                .iter()
                .map(|v| v * v)
                .sum::<usize>(),
        )
    };

    let tile_x_center =
        usize::try_from(camera_pos.x + 1_000_000).unwrap_or(0) / pixel_to_tile / map_tile_size;
    let tile_y_center =
        usize::try_from(camera_pos.y + 1_000_000).unwrap_or(0) / pixel_to_tile / map_tile_size;

    let tile_x_left_edge = (usize::try_from(camera_pos.x + 1_000_000)
        .unwrap_or(0)
        .saturating_sub(view_width_in_tiles.div_ceil(2)))
        / pixel_to_tile
        / map_tile_size;
    let tile_y_left_edge = (usize::try_from(camera_pos.y + 1_000_000)
        .unwrap_or(0)
        .saturating_sub(view_height_in_tiles.div_ceil(2)))
        / pixel_to_tile
        / map_tile_size;
    let tile_x_right_edge = min(
        usize::try_from(camera_pos.x + 1_000_000)
            .unwrap_or(0)
            .saturating_add(view_width_in_tiles.div_ceil(2)),
        2_000_000,
    ) / pixel_to_tile
        / map_tile_size;
    let tile_y_right_edge = min(
        usize::try_from(camera_pos.y + 1_000_000)
            .unwrap_or(0)
            .saturating_add(view_height_in_tiles.div_ceil(2)),
        2_000_000,
    ) / pixel_to_tile
        / map_tile_size;

    let tile_x = tile_x_left_edge..=(min(tile_x_right_edge, num_per_axis));
    let tile_y = tile_y_left_edge..=(min(tile_y_right_edge, num_per_axis));

    let full_tile_iter = tile_x.flat_map(|tile_x| std::iter::repeat(tile_x).zip(tile_y.clone()));

    for (tile_x, tile_y) in full_tile_iter.sorted_unstable_by_key(|(tile_x, tile_y)| {
        let x_dist = tile_x.abs_diff(tile_x_center);
        let y_dist = tile_y.abs_diff(tile_y_center);

        x_dist * x_dist + y_dist * y_dist
    }) {
        let tile_texture_id = tile_x * num_per_axis + tile_y + texture_id_offset;

        profiling::scope!(
            "Create Runtime texture",
            format!(
                "Size: {:?}, tile_per_pixel: {}",
                [map_tile_size; 2], pixel_to_tile
            )
        );
        renderer.create_runtime_texture_if_missing(tile_texture_id, [map_tile_size; 2], || {
            collect_colors(
                world,
                [tile_x, tile_y],
                map_tile_size,
                pixel_to_tile,
                data_store,
            )
        });

        if let Some(allowed_time) = allowed_time {
            if start_time.elapsed() > allowed_time {
                break;
            }
        }
    }
}

#[profiling::function]
#[inline(never)]
fn collect_colors<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &World<ItemIdxType, RecipeIdxType>,
    [tile_x, tile_y]: [usize; 2],
    map_tile_size: usize,
    pixel_to_tile: usize,
    data_store: &crate::data::DataStore<ItemIdxType, RecipeIdxType>,
) -> Vec<u8> {
    let data = ((tile_y * map_tile_size)..((tile_y + 1) * map_tile_size))
        .into_par_iter()
        .flat_map_iter(|y_pos| {
            std::iter::repeat(y_pos).zip((tile_x * map_tile_size)..((tile_x + 1) * map_tile_size))
        })
        .map(|(y_pos, x_pos)| {
            let x_pos_world = (i32::try_from(x_pos * pixel_to_tile).unwrap() - 1_000_000) as i32;
            let y_pos_world = (i32::try_from(y_pos * pixel_to_tile).unwrap() - 1_000_000) as i32;

            let color = world.get_entity_color(
                Position {
                    x: x_pos_world,
                    y: y_pos_world,
                },
                data_store,
            );

            color
        })
        .collect();

    // let size = map_tile_size * map_tile_size;
    // let mut data = {
    //     profiling::scope!("Allocate vec");
    //     vec![Color32::BLACK; size]
    // };

    // let start_x =
    //     (i32::try_from((tile_x * map_tile_size) * pixel_to_tile).unwrap() - 1_000_000) as i32;
    // let end_x =
    //     (i32::try_from(((tile_x + 1) * map_tile_size) * pixel_to_tile).unwrap() - 1_000_000) as i32;
    // let start_y =
    //     (i32::try_from((tile_y * map_tile_size) * pixel_to_tile).unwrap() - 1_000_000) as i32;
    // let end_y =
    //     (i32::try_from(((tile_y + 1) * map_tile_size) * pixel_to_tile).unwrap() - 1_000_000) as i32;

    // world.get_area_colors(
    //     start_x..end_x,
    //     start_y..end_y,
    //     pixel_to_tile,
    //     &mut data,
    //     data_store,
    // );

    {
        profiling::scope!("Color32 to u8");
        bytemuck::cast_vec(data)
    }
}

#[profiling::function]
pub fn apply_updates(
    updates: impl IntoIterator<Item = MapViewUpdate>,
    renderer: &mut impl RendererTrait,
) {
    for update in updates {
        for tile_idx in 0..NUM_MAP_TILE_SIZES {
            let (map_tile_size, pixel_to_tile, num_per_axis, texture_id_offset) = {
                (
                    TILE_SIZE_PIXELS[tile_idx],
                    TILE_PIXEL_TO_WORLD_TILE[tile_idx],
                    NUM_TILES_PER_AXIS[tile_idx],
                    NUM_TILES_PER_AXIS[0..tile_idx]
                        .iter()
                        .map(|v| v * v)
                        .sum::<usize>(),
                )
            };

            if update.pos.x % pixel_to_tile as i32 == 0 && update.pos.y % pixel_to_tile as i32 == 0
            {
                let texture_id = (usize::try_from(update.pos.x + 1_000_000).unwrap()
                    / pixel_to_tile
                    / map_tile_size)
                    * num_per_axis
                    + (usize::try_from(update.pos.y + 1_000_000).unwrap()
                        / pixel_to_tile
                        / map_tile_size)
                    + texture_id_offset;

                let x = (usize::try_from(update.pos.x + 1_000_000).unwrap() / pixel_to_tile)
                    % map_tile_size;
                let y = (usize::try_from(update.pos.y + 1_000_000).unwrap() / pixel_to_tile)
                    % map_tile_size;

                renderer.do_texture_updates(
                    texture_id,
                    [(
                        x,
                        y,
                        [update.color.r(), update.color.g(), update.color.b(), 255],
                    )],
                );
            }
        }
    }
}

#[profiling::function]
pub fn render_map_view(
    renderer: &mut impl RendererTrait,
    camera_pos: Position,
    view_width: f32,
    view_height: f32,
    view_width_in_tiles: usize,
    view_height_in_tiles: usize,
    tile_size: f32,
    aspect_ratio: f32,
    player_pos: (f32, f32),
) {
    let (map_tile_size, pixel_to_tile, num_per_axis, texture_id_offset) = {
        let idx = MIN_WIDTH
            .iter()
            .filter(|min| **min < view_width_in_tiles)
            .count()
            - 1;

        (
            TILE_SIZE_PIXELS[idx],
            TILE_PIXEL_TO_WORLD_TILE[idx],
            NUM_TILES_PER_AXIS[idx],
            NUM_TILES_PER_AXIS[0..idx]
                .iter()
                .map(|v| v * v)
                .sum::<usize>(),
        )
    };
    let tile_x_left_edge = (usize::try_from(camera_pos.x + 1_000_000)
        .unwrap()
        .saturating_sub(view_width_in_tiles.div_ceil(2)))
        / pixel_to_tile
        / map_tile_size;
    let tile_y_left_edge = (usize::try_from(camera_pos.y + 1_000_000)
        .unwrap()
        .saturating_sub(view_height_in_tiles.div_ceil(2)))
        / pixel_to_tile
        / map_tile_size;
    let tile_x_right_edge = min(
        usize::try_from(camera_pos.x + 1_000_000)
            .unwrap()
            .saturating_add(view_width_in_tiles.div_ceil(2)),
        2_000_000,
    ) / pixel_to_tile
        / map_tile_size;
    let tile_y_right_edge = min(
        usize::try_from(camera_pos.y + 1_000_000)
            .unwrap()
            .saturating_add(view_height_in_tiles.div_ceil(2)),
        2_000_000,
    ) / pixel_to_tile
        / map_tile_size;

    let mut map_layer = Layer::square_tile_grid(tile_size, aspect_ratio);
    for tile_x in tile_x_left_edge..=(min(tile_x_right_edge, num_per_axis)) {
        for tile_y in tile_y_left_edge..=(min(tile_y_right_edge, num_per_axis)) {
            let texture_id = tile_x * num_per_axis + tile_y + texture_id_offset;

            let tile_draw_offs = (
                (tile_x * map_tile_size * pixel_to_tile) as f32 - 1_000_000.0 - player_pos.0
                    + (0.5 * view_width),
                (tile_y * map_tile_size * pixel_to_tile) as f32 - 1_000_000.0 - player_pos.1
                    + (0.5 * view_height),
            );

            if renderer.has_runtime_texture(texture_id) {
                map_layer.draw_runtime_texture(
                    texture_id,
                    DrawInstance {
                        position: [tile_draw_offs.0, tile_draw_offs.1],
                        size: [(map_tile_size * pixel_to_tile) as f32; 2],
                        animation_frame: 0,
                    },
                );
            }
            // map_layer.draw_sprite(
            //     &Sprite::new(Texture::default()),
            //     DrawInstance {
            //         position: [tile_draw_offs.0, tile_draw_offs.1],
            //         size: [MEDIUM_SIZE as f32; 2],
            //         animation_frame: 0,
            //     },
            // );
        }
    }

    renderer.draw(&map_layer);
}
