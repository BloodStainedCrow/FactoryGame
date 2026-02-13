use std::{
    borrow::Borrow,
    cmp::{max, min},
    collections::HashMap,
    sync::LazyLock,
    time::Duration,
};

use wasm_timer::Instant;

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

// FIXME: It seems we are rendering one map tile to much in the positive directions

const NUM_MAP_TILE_SIZES: usize = 5;
// TODO: Figure out a good tilesize. 1024 seems to work fine, but is larger or smaller better?
const TILE_SIZE_PIXELS: [u32; NUM_MAP_TILE_SIZES] = [1024, 1024, 1024, 1024, 4000];
// TODO: Since array::map is not const, we hack it like this
const NUM_TILES_PER_AXIS: [u32; NUM_MAP_TILE_SIZES] = {
    let mut b = [0; NUM_MAP_TILE_SIZES];
    let mut i = 0;
    while i < NUM_MAP_TILE_SIZES {
        b[i] = 2_000_000u32.div_ceil(TILE_SIZE_PIXELS[i] * TILE_PIXEL_TO_WORLD_TILE[i]); // map
        i += 1;
    }
    b
};
const TILE_PIXEL_TO_WORLD_TILE: [u32; NUM_MAP_TILE_SIZES] = [1, 4, 16, 64, 256];
pub const MIN_WIDTH: [u32; NUM_MAP_TILE_SIZES] = [0, 10_000, 40_000, 100_000, 300_000];

#[profiling::function]
pub fn create_map_textures_if_needed<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &World<ItemIdxType, RecipeIdxType>,
    renderer: &mut impl RendererTrait,
    camera_pos: Position,
    view_width_in_tiles: u32,
    view_height_in_tiles: u32,
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
                .sum::<u32>(),
        )
    };

    let tile_x_center = (camera_pos.x + 1_000_000)
        / i32::try_from(pixel_to_tile).unwrap()
        / i32::try_from(map_tile_size).unwrap();
    let tile_y_center = (camera_pos.y + 1_000_000)
        / i32::try_from(pixel_to_tile).unwrap()
        / i32::try_from(map_tile_size).unwrap();

    let tile_x_left_edge = ((camera_pos.x + 1_000_000)
        .saturating_sub_unsigned(view_width_in_tiles.div_ceil(2)))
        / i32::try_from(pixel_to_tile).unwrap()
        / i32::try_from(map_tile_size).unwrap();
    let tile_y_left_edge = ((camera_pos.y + 1_000_000)
        .saturating_sub_unsigned(view_height_in_tiles.div_ceil(2)))
        / i32::try_from(pixel_to_tile).unwrap()
        / i32::try_from(map_tile_size).unwrap();
    let tile_x_right_edge = min(
        (camera_pos.x + 1_000_000).saturating_add_unsigned(view_width_in_tiles.div_ceil(2)),
        2_000_000,
    ) / i32::try_from(pixel_to_tile).unwrap()
        / i32::try_from(map_tile_size).unwrap();
    let tile_y_right_edge = min(
        (camera_pos.y + 1_000_000).saturating_add_unsigned(view_height_in_tiles.div_ceil(2)),
        2_000_000,
    ) / i32::try_from(pixel_to_tile).unwrap()
        / i32::try_from(map_tile_size).unwrap();

    let tile_x =
        max(tile_x_left_edge, 0)..=(min(tile_x_right_edge, i32::try_from(num_per_axis).unwrap()));
    let tile_y =
        max(tile_y_left_edge, 0)..=(min(tile_y_right_edge, i32::try_from(num_per_axis).unwrap()));

    let full_tile_iter = tile_x.flat_map(|tile_x| std::iter::repeat(tile_x).zip(tile_y.clone()));

    for (tile_x, tile_y) in full_tile_iter.sorted_unstable_by_key(|(tile_x, tile_y)| {
        let x_dist = tile_x.abs_diff(tile_x_center);
        let y_dist = tile_y.abs_diff(tile_y_center);

        x_dist * x_dist + y_dist * y_dist
    }) {
        assert!(tile_x >= 0);
        assert!(tile_x < i32::try_from(num_per_axis).unwrap());

        assert!(tile_y >= 0);
        assert!(tile_y < i32::try_from(num_per_axis).unwrap());
        let tile_texture_id =
            tile_x as usize * num_per_axis as usize + tile_y as usize + texture_id_offset as usize;

        profiling::scope!(
            "Create Runtime texture",
            format!(
                "Size: {:?}, tile_per_pixel: {}",
                [map_tile_size; 2], pixel_to_tile
            )
        );

        let tile_x_end = ((tile_x + 1) * map_tile_size as i32 * pixel_to_tile as i32) - 1_000_000;
        let real_tile_x_end = min(tile_x_end, 1_000_000);
        let tile_y_end = ((tile_y + 1) * map_tile_size as i32 * pixel_to_tile as i32) - 1_000_000;
        let real_tile_y_end = min(tile_y_end, 1_000_000);
        assert!(tile_x_end >= real_tile_x_end);
        assert!(tile_y_end >= real_tile_y_end);
        let size = [
            (((map_tile_size) * pixel_to_tile - (tile_x_end - real_tile_x_end) as u32)
                / pixel_to_tile) as usize,
            (((map_tile_size) * pixel_to_tile - (tile_y_end - real_tile_y_end) as u32)
                / pixel_to_tile) as usize,
        ];

        assert!(size[0] > 0);
        assert!(size[1] > 0);

        renderer.create_runtime_texture_if_missing(tile_texture_id, size, || {
            let ret = collect_colors(
                world,
                [tile_x as u32, tile_y as u32],
                size.map(|v| v as u32),
                map_tile_size,
                pixel_to_tile,
                data_store,
            );

            assert_eq!(Borrow::<[u8]>::borrow(&ret).len(), size[0] * size[1] * 4);

            ret
        });

        if let Some(allowed_time) = allowed_time {
            if start_time.elapsed() > allowed_time {
                break;
            }
        }
    }
}

enum ColorResult {
    Const(&'static [u8]),
    Generated(Vec<u8>),
}

impl Borrow<[u8]> for ColorResult {
    fn borrow(&self) -> &[u8] {
        &match self {
            ColorResult::Const(data) => data,
            ColorResult::Generated(data) => data.as_slice(),
        }
    }
}

static DEDUP_MAP: LazyLock<HashMap<u32, Vec<u8>>> = LazyLock::new(|| {
    let mut map = HashMap::new();
    for map_tile_size in TILE_SIZE_PIXELS {
        map.entry(map_tile_size * map_tile_size)
            .or_insert(bytemuck::cast_vec(vec![
                Color32::BLACK;
                map_tile_size as usize
                    * map_tile_size as usize
            ]));
    }
    map
});

#[profiling::function]
fn collect_colors<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    world: &World<ItemIdxType, RecipeIdxType>,
    [tile_x, tile_y]: [u32; 2],
    [size_x, size_y]: [u32; 2],
    map_tile_size: u32,
    pixel_to_tile: u32,
    data_store: &crate::data::DataStore<ItemIdxType, RecipeIdxType>,
) -> ColorResult {
    let data = if world
        .any_generated_chunks_in_area(
            Position {
                x: (i32::try_from(tile_x * map_tile_size * pixel_to_tile).unwrap() - 1_000_000)
                    as i32,
                y: (i32::try_from(tile_y * map_tile_size * pixel_to_tile).unwrap() - 1_000_000)
                    as i32,
            },
            Position {
                x: (i32::try_from((tile_x * map_tile_size + size_x) * pixel_to_tile).unwrap()
                    - 1_000_000) as i32,
                y: (i32::try_from((tile_y * map_tile_size + size_y) * pixel_to_tile).unwrap()
                    - 1_000_000) as i32,
            },
        )
        .unwrap_or(true)
    {
        ColorResult::Generated(bytemuck::cast_vec(
            ((tile_y * map_tile_size)..(tile_y * map_tile_size + size_y))
                .into_par_iter()
                .flat_map_iter(|y_pos| {
                    std::iter::repeat(y_pos)
                        .zip((tile_x * map_tile_size)..(tile_x * map_tile_size + size_x))
                })
                .map(|(y_pos, x_pos)| {
                    let x_pos_world =
                        (i32::try_from(x_pos * pixel_to_tile).unwrap() - 1_000_000) as i32;
                    let y_pos_world =
                        (i32::try_from(y_pos * pixel_to_tile).unwrap() - 1_000_000) as i32;

                    assert!(x_pos_world <= 1_000_000);
                    assert!(y_pos_world <= 1_000_000);

                    let color = world.get_entity_color(
                        Position {
                            x: x_pos_world,
                            y: y_pos_world,
                        },
                        data_store,
                    );

                    color
                })
                .collect(),
        ))
    } else {
        match DEDUP_MAP.get(&(size_x * size_y)) {
            Some(cached_alloc) => ColorResult::Const(&cached_alloc),
            None => ColorResult::Generated(bytemuck::cast_vec(vec![
                Color32::BLACK;
                size_x as usize
                    * size_y as usize
            ])),
        }
    };

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

    data
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
                        .sum::<u32>(),
                )
            };

            if update.pos.x % pixel_to_tile as i32 == 0 && update.pos.y % pixel_to_tile as i32 == 0
            {
                assert!(update.pos.x > -1_000_000);
                assert!(update.pos.x < 1_000_000);

                assert!(update.pos.y > -1_000_000);
                assert!(update.pos.y < 1_000_000);

                let texture_id = ((update.pos.x + 1_000_000)
                    / i32::try_from(pixel_to_tile).unwrap()
                    / i32::try_from(map_tile_size).unwrap())
                    * i32::try_from(num_per_axis).unwrap()
                    + ((update.pos.y + 1_000_000)
                        / i32::try_from(pixel_to_tile).unwrap()
                        / i32::try_from(map_tile_size).unwrap())
                    + i32::try_from(texture_id_offset).unwrap();

                let x = ((update.pos.x + 1_000_000) / i32::try_from(pixel_to_tile).unwrap())
                    % i32::try_from(map_tile_size).unwrap();
                let y = ((update.pos.y + 1_000_000) / i32::try_from(pixel_to_tile).unwrap())
                    % i32::try_from(map_tile_size).unwrap();

                renderer.do_texture_updates(
                    texture_id.try_into().unwrap(),
                    [(
                        x as usize,
                        y as usize,
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
    view_width_in_tiles: u32,
    view_height_in_tiles: u32,
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
                .sum::<u32>(),
        )
    };
    let tile_x_left_edge = ((camera_pos.x + 1_000_00)
        .saturating_sub_unsigned(view_width_in_tiles.div_ceil(2)))
        / i32::try_from(pixel_to_tile).unwrap()
        / i32::try_from(map_tile_size).unwrap();
    let tile_y_left_edge = ((camera_pos.y + 1_000_000)
        .saturating_sub_unsigned(view_height_in_tiles.div_ceil(2)))
        / i32::try_from(pixel_to_tile).unwrap()
        / i32::try_from(map_tile_size).unwrap();
    let tile_x_right_edge = min(
        (camera_pos.x + 1_000_000).saturating_add_unsigned(view_width_in_tiles.div_ceil(2)),
        2_000_000,
    ) / i32::try_from(pixel_to_tile).unwrap()
        / i32::try_from(map_tile_size).unwrap();
    let tile_y_right_edge = min(
        (camera_pos.y + 1_000_000).saturating_add_unsigned(view_height_in_tiles.div_ceil(2)),
        2_000_000,
    ) / i32::try_from(pixel_to_tile).unwrap()
        / i32::try_from(map_tile_size).unwrap();

    let mut map_layer = Layer::square_tile_grid(tile_size, aspect_ratio);
    for tile_x in
        max(tile_x_left_edge, 0)..=(min(tile_x_right_edge, i32::try_from(num_per_axis).unwrap()))
    {
        for tile_y in max(tile_y_left_edge, 0)
            ..=(min(tile_y_right_edge, i32::try_from(num_per_axis).unwrap()))
        {
            assert!(tile_x >= 0);
            assert!(tile_x < i32::try_from(num_per_axis).unwrap());

            assert!(tile_y >= 0);
            assert!(tile_y < i32::try_from(num_per_axis).unwrap());

            let texture_id = tile_x as usize * num_per_axis as usize
                + tile_y as usize
                + texture_id_offset as usize;

            let tile_draw_offs = (
                (tile_x as u32 * map_tile_size * pixel_to_tile) as f32 - 1_000_000.0 - player_pos.0
                    + (0.5 * view_width),
                (tile_y as u32 * map_tile_size * pixel_to_tile) as f32 - 1_000_000.0 - player_pos.1
                    + (0.5 * view_height),
            );

            if renderer.has_runtime_texture(texture_id) {
                let tile_x_end =
                    ((tile_x + 1) * map_tile_size as i32 * pixel_to_tile as i32) - 1_000_000;
                let real_tile_x_end = min(tile_x_end, 1_000_000);
                let tile_y_end =
                    ((tile_y + 1) * map_tile_size as i32 * pixel_to_tile as i32) - 1_000_000;
                let real_tile_y_end = min(tile_y_end, 1_000_000);
                assert!(tile_x_end >= real_tile_x_end);
                assert!(tile_y_end >= real_tile_y_end);
                let size = [
                    ((map_tile_size) * pixel_to_tile - (tile_x_end - real_tile_x_end) as u32)
                        as f32,
                    ((map_tile_size) * pixel_to_tile - (tile_y_end - real_tile_y_end) as u32)
                        as f32,
                ];
                if size[0] > 0.0 && size[1] > 0.0 {
                    map_layer.draw_runtime_texture(
                        texture_id,
                        DrawInstance {
                            position: [tile_draw_offs.0, tile_draw_offs.1],
                            size,
                            animation_frame: 0,
                        },
                    );
                }
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
