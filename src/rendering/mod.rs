use image::GenericImageView;
use tilelib::types::{DrawInstance, Layer, Sprite, Texture};

use crate::frontend::world::tile::Dir;

pub mod app_state;
pub mod eframe_app;
mod render_world;
pub mod window;

pub mod map_view;

#[derive(Debug)]
struct EntitySprite {
    pub sprite: Sprite,
    pub aspect_ratio: f32,
    pub offset: (f32, f32),
    pub scaling: (f32, f32),
}

impl EntitySprite {
    const fn new_tiling(sprite: Sprite) -> Self {
        Self {
            sprite,
            aspect_ratio: 1.0,
            offset: (0.0, 0.0),
            scaling: (1.0, 1.0),
        }
    }

    const fn new_scaled(sprite: Sprite, size: f32) -> Self {
        Self {
            sprite,
            aspect_ratio: 1.0,
            offset: (0.0, 0.0),
            scaling: (size, size),
        }
    }

    fn new_tall(sprite: Sprite, aspect_ratio: f32) -> Self {
        assert!(aspect_ratio < 1.0);
        let height = 1.0 / aspect_ratio;
        Self {
            sprite,
            aspect_ratio,
            offset: (0.0, -(height - 1.0)),
            scaling: (1.0, height),
        }
    }

    fn draw(&self, pos: [f32; 2], tile_size: [u16; 2], animation_frame: u32, layer: &mut Layer) {
        layer.draw_sprite(
            &self.sprite,
            DrawInstance {
                position: [
                    pos[0] + (self.offset.0 * tile_size[0] as f32),
                    pos[1] + (self.offset.1 * tile_size[1] as f32),
                ],
                size: [
                    f32::from(tile_size[0]) * self.scaling.0,
                    f32::from(tile_size[1]) * self.scaling.1,
                ],
                animation_frame,
            },
        );
    }

    fn draw_centered_on(
        &self,
        underlying: &Self,
        pos: [f32; 2],
        tile_size: [u16; 2],
        animation_frame: u32,
        layer: &mut Layer,
    ) {
        let other_pos = [pos[0] + underlying.offset.0, pos[1] + underlying.offset.1];
        let other_size = [
            f32::from(tile_size[0]) * underlying.scaling.0,
            f32::from(tile_size[1]) * underlying.scaling.1,
        ];
        let self_size = [self.scaling.0, self.scaling.1];

        let unused_space = [other_size[0] - self_size[0], other_size[1] - self_size[1]];
        let self_pos = [
            other_pos[0] + unused_space[0] / 2.0,
            other_pos[1] + unused_space[1] / 2.0,
        ];

        layer.draw_sprite(
            &self.sprite,
            DrawInstance {
                position: self_pos,
                size: self_size,
                animation_frame,
            },
        );
    }
}

#[derive(Debug)]
pub struct TextureAtlas {
    outside_world: Sprite,
    blue: Sprite,

    chest: EntitySprite,

    assembler: EntitySprite,
    no_power: EntitySprite,
    not_connected: EntitySprite,
    belt: enum_map::EnumMap<Dir, Sprite>,
    inserter: enum_map::EnumMap<Dir, Sprite>,

    player: Sprite,

    items: Box<[Sprite]>,

    beacon: EntitySprite,
    power_pole: EntitySprite,

    lab: EntitySprite,

    dark_square: Sprite,

    default: Sprite,
}

#[cfg(not(feature = "graphics"))]
fn texture_atlas() -> TextureAtlas {
    let black = include_bytes!("temp_assets/outside_world.png");
    let black = image::load_from_memory(black).unwrap();

    let black_dimensions = black.dimensions();
    let black = black.to_rgba8().into_vec();

    let blue = include_bytes!("temp_assets/blue.png");
    let blue = image::load_from_memory(blue).unwrap();

    let blue_dimensions = blue.dimensions();
    let blue = blue.to_rgba8().into_vec();

    let assembler = include_bytes!("temp_assets/assembler.png");
    let assembler = image::load_from_memory(assembler).unwrap();

    let assembler_dimensions = assembler.dimensions();
    let assembler = assembler.to_rgba8().into_vec();

    let belt_north = include_bytes!("temp_assets/belt_north.png");
    let belt_north = image::load_from_memory(belt_north).unwrap();

    let belt_north_dimensions = belt_north.dimensions();
    let belt_north = belt_north.to_rgba8().into_vec();

    let belt_south = include_bytes!("temp_assets/belt_south.png");
    let belt_south = image::load_from_memory(belt_south).unwrap();

    let belt_south_dimensions = belt_south.dimensions();
    let belt_south = belt_south.to_rgba8().into_vec();

    let belt_west = include_bytes!("temp_assets/belt_west.png");
    let belt_west = image::load_from_memory(belt_west).unwrap();

    let belt_west_dimensions = belt_west.dimensions();
    let belt_west = belt_west.to_rgba8().into_vec();

    let belt_east = include_bytes!("temp_assets/belt_east.png");
    let belt_east = image::load_from_memory(belt_east).unwrap();

    let belt_east_dimensions = belt_east.dimensions();
    let belt_east = belt_east.to_rgba8().into_vec();

    let inserter_north = include_bytes!("temp_assets/inserter_north.png");
    let inserter_north = image::load_from_memory(inserter_north).unwrap();

    let inserter_north_dimensions = inserter_north.dimensions();
    let inserter_north = inserter_north.to_rgba8().into_vec();

    let inserter_south = include_bytes!("temp_assets/inserter_south.png");
    let inserter_south = image::load_from_memory(inserter_south).unwrap();

    let inserter_south_dimensions = inserter_south.dimensions();
    let inserter_south = inserter_south.to_rgba8().into_vec();

    let inserter_west = include_bytes!("temp_assets/inserter_west.png");
    let inserter_west = image::load_from_memory(inserter_west).unwrap();

    let inserter_west_dimensions = inserter_west.dimensions();
    let inserter_west = inserter_west.to_rgba8().into_vec();

    let inserter_east = include_bytes!("temp_assets/inserter_east.png");
    let inserter_east = image::load_from_memory(inserter_east).unwrap();

    let inserter_east_dimensions = inserter_east.dimensions();
    let inserter_east = inserter_east.to_rgba8().into_vec();

    let plate = include_bytes!("temp_assets/plate.png");
    let plate = image::load_from_memory(plate).unwrap();

    let plate_dimensions = plate.dimensions();
    let plate = plate.to_rgba8().into_vec();

    let player = include_bytes!("temp_assets/player.png");
    let player = image::load_from_memory(player).unwrap();

    let player_dimensions = player.dimensions();
    let player = player.to_rgba8().into_vec();

    let not_connected = include_bytes!("temp_assets/not_connected.png");
    let not_connected = image::load_from_memory(not_connected).unwrap();

    let not_connected_dimensions = not_connected.dimensions();
    let not_connected = not_connected.to_rgba8().into_vec();

    let no_power = include_bytes!("temp_assets/no_power.png");
    let no_power = image::load_from_memory(no_power).unwrap();

    let no_power_dimensions = no_power.dimensions();
    let no_power = no_power.to_rgba8().into_vec();

    let beacon = include_bytes!("temp_assets/beacon.png");
    let beacon = image::load_from_memory(beacon).unwrap();

    let beacon_dimensions = beacon.dimensions();
    let beacon = beacon.to_rgba8().into_vec();

    let dark_square = include_bytes!("temp_assets/dark_square.png");
    let dark_square = image::load_from_memory(dark_square).unwrap();

    let dark_square_dimensions = dark_square.dimensions();
    let dark_square = dark_square.to_rgba8().into_vec();

    TextureAtlas {
        outside_world: Sprite::new(Texture::new(1, black.clone(), black_dimensions)),
        blue: Sprite::new(Texture::new(1, blue, blue_dimensions)),

        not_connected: EntitySprite::new_scaled(
            Sprite::new(Texture::new(1, not_connected, not_connected_dimensions)),
            3.0,
        ),

        no_power: EntitySprite::new_scaled(
            Sprite::new(Texture::new(1, no_power, no_power_dimensions)),
            3.0,
        ),

        assembler: EntitySprite::new_tiling(Sprite::new(Texture::new(
            1,
            assembler.clone(),
            assembler_dimensions,
        ))),
        chest: EntitySprite::new_tiling(Sprite::new(Texture::new(1, black, black_dimensions))),

        items: vec![
            Sprite::new(Texture::new(1, plate.clone(), plate_dimensions)),
            Sprite::new(Texture::new(1, plate, plate_dimensions)),
        ]
        .into_boxed_slice(),

        player: Sprite::new(Texture::new(1, player, player_dimensions)),
        belt: enum_map::EnumMap::from_array([
            Sprite::new(Texture::new(1, belt_north.clone(), belt_north_dimensions)),
            Sprite::new(Texture::new(1, belt_east, belt_east_dimensions)),
            Sprite::new(Texture::new(1, belt_south, belt_south_dimensions)),
            Sprite::new(Texture::new(1, belt_west, belt_west_dimensions)),
        ]),

        inserter: enum_map::EnumMap::from_array([
            Sprite::new(Texture::new(1, inserter_north, inserter_north_dimensions)),
            Sprite::new(Texture::new(1, inserter_east, inserter_east_dimensions)),
            Sprite::new(Texture::new(1, inserter_south, inserter_south_dimensions)),
            Sprite::new(Texture::new(1, inserter_west, inserter_west_dimensions)),
        ]),

        beacon: EntitySprite::new_tiling(Sprite::new(Texture::new(1, beacon, beacon_dimensions))),
        power_pole: EntitySprite::new_tiling(Sprite::new(Texture::new(
            1,
            assembler,
            assembler_dimensions,
        ))),

        lab: EntitySprite::new_tiling(Sprite::new(Texture::new(
            1,
            belt_north,
            belt_north_dimensions,
        ))),

        dark_square: Sprite::new(Texture::new(1, dark_square, dark_square_dimensions)),

        default: Sprite::new(Texture::default()),
    }
}

#[cfg(feature = "graphics")]
fn texture_atlas() -> TextureAtlas {
    let black = include_bytes!("temp_assets/outside_world.png");
    let black = image::load_from_memory(black).unwrap();

    let black_dimensions = black.dimensions();
    let black = black.to_rgba8().into_vec();

    let blue = include_bytes!("temp_assets/blue.png");
    let blue = image::load_from_memory(blue).unwrap();

    let blue_dimensions = blue.dimensions();
    let blue = blue.to_rgba8().into_vec();

    let assembler = include_bytes!("temp_assets/krastorio/furnace.png");
    let assembler = image::load_from_memory(assembler).unwrap();

    let assembler_dimensions = assembler.dimensions();
    let assembler = assembler.to_rgba8().into_vec();

    let power_pole = include_bytes!("temp_assets/krastorio/PowerPole.png");
    let power_pole = image::load_from_memory(power_pole).unwrap();

    let power_pole_dimensions = power_pole.dimensions();
    let power_pole = power_pole.to_rgba8().into_vec();

    let lab = include_bytes!("temp_assets/krastorio/advanced-lab.png");
    let lab = image::load_from_memory(lab).unwrap();

    let lab_dimensions = lab.dimensions();
    let lab = lab.to_rgba8().into_vec();

    let chest = include_bytes!("temp_assets/krastorio/chest.png");
    let chest: image::DynamicImage = image::load_from_memory(chest).unwrap();

    let chest_dimensions = chest.dimensions();
    let chest = chest.to_rgba8().into_vec();

    let belt_north = include_bytes!("temp_assets/belt_north.png");
    let belt_north = image::load_from_memory(belt_north).unwrap();

    let belt_north_dimensions = belt_north.dimensions();
    let belt_north = belt_north.to_rgba8().into_vec();

    let belt_south = include_bytes!("temp_assets/belt_south.png");
    let belt_south = image::load_from_memory(belt_south).unwrap();

    let belt_south_dimensions = belt_south.dimensions();
    let belt_south = belt_south.to_rgba8().into_vec();

    let belt_west = include_bytes!("temp_assets/belt_west.png");
    let belt_west = image::load_from_memory(belt_west).unwrap();

    let belt_west_dimensions = belt_west.dimensions();
    let belt_west = belt_west.to_rgba8().into_vec();

    let belt_east = include_bytes!("temp_assets/belt_east.png");
    let belt_east = image::load_from_memory(belt_east).unwrap();

    let belt_east_dimensions = belt_east.dimensions();
    let belt_east = belt_east.to_rgba8().into_vec();

    let inserter_north = include_bytes!("temp_assets/inserter_north.png");
    let inserter_north = image::load_from_memory(inserter_north).unwrap();

    let inserter_north_dimensions = inserter_north.dimensions();
    let inserter_north = inserter_north.to_rgba8().into_vec();

    let inserter_south = include_bytes!("temp_assets/inserter_south.png");
    let inserter_south = image::load_from_memory(inserter_south).unwrap();

    let inserter_south_dimensions = inserter_south.dimensions();
    let inserter_south = inserter_south.to_rgba8().into_vec();

    let inserter_west = include_bytes!("temp_assets/inserter_west.png");
    let inserter_west = image::load_from_memory(inserter_west).unwrap();

    let inserter_west_dimensions = inserter_west.dimensions();
    let inserter_west = inserter_west.to_rgba8().into_vec();

    let inserter_east = include_bytes!("temp_assets/inserter_east.png");
    let inserter_east = image::load_from_memory(inserter_east).unwrap();

    let inserter_east_dimensions = inserter_east.dimensions();
    let inserter_east = inserter_east.to_rgba8().into_vec();

    let plate = include_bytes!("temp_assets/plate.png");
    let plate = image::load_from_memory(plate).unwrap();

    let plate_dimensions = plate.dimensions();
    let plate = plate.to_rgba8().into_vec();

    let player = include_bytes!("temp_assets/player.png");
    let player = image::load_from_memory(player).unwrap();

    let player_dimensions = player.dimensions();
    let player = player.to_rgba8().into_vec();

    let not_connected = include_bytes!("temp_assets/not_connected.png");
    let not_connected = image::load_from_memory(not_connected).unwrap();

    let not_connected_dimensions = not_connected.dimensions();
    let not_connected = not_connected.to_rgba8().into_vec();

    let no_power = include_bytes!("temp_assets/no_power.png");
    let no_power = image::load_from_memory(no_power).unwrap();

    let no_power_dimensions = no_power.dimensions();
    let no_power = no_power.to_rgba8().into_vec();

    let beacon = include_bytes!("temp_assets/beacon.png");
    let beacon = image::load_from_memory(beacon).unwrap();

    let beacon_dimensions = beacon.dimensions();
    let beacon = beacon.to_rgba8().into_vec();

    let dark_square = include_bytes!("temp_assets/dark_square.png");
    let dark_square = image::load_from_memory(dark_square).unwrap();

    let dark_square_dimensions = dark_square.dimensions();
    let dark_square = dark_square.to_rgba8().into_vec();

    TextureAtlas {
        outside_world: Sprite::new(Texture::new(1, black.clone(), black_dimensions)),
        blue: Sprite::new(Texture::new(1, blue, blue_dimensions)),

        not_connected: EntitySprite::new_scaled(
            Sprite::new(Texture::new(1, not_connected, not_connected_dimensions)),
            3.0,
        ),

        no_power: EntitySprite::new_scaled(
            Sprite::new(Texture::new(1, no_power, no_power_dimensions)),
            3.0,
        ),

        assembler: EntitySprite::new_tiling(Sprite::new(Texture::new(
            7 * 4,
            assembler,
            assembler_dimensions,
        ))),
        chest: EntitySprite::new_tiling(Sprite::new(Texture::new(1, chest, chest_dimensions))),

        items: vec![
            Sprite::new(Texture::new(1, plate.clone(), plate_dimensions)),
            Sprite::new(Texture::new(1, plate, plate_dimensions)),
        ]
        .into_boxed_slice(),

        player: Sprite::new(Texture::new(1, player, player_dimensions)),
        belt: enum_map::EnumMap::from_array([
            Sprite::new(Texture::new(1, belt_north, belt_north_dimensions)),
            Sprite::new(Texture::new(1, belt_east, belt_east_dimensions)),
            Sprite::new(Texture::new(1, belt_south, belt_south_dimensions)),
            Sprite::new(Texture::new(1, belt_west, belt_west_dimensions)),
        ]),

        inserter: enum_map::EnumMap::from_array([
            Sprite::new(Texture::new(1, inserter_north, inserter_north_dimensions)),
            Sprite::new(Texture::new(1, inserter_east, inserter_east_dimensions)),
            Sprite::new(Texture::new(1, inserter_south, inserter_south_dimensions)),
            Sprite::new(Texture::new(1, inserter_west, inserter_west_dimensions)),
        ]),

        beacon: EntitySprite::new_tiling(Sprite::new(Texture::new(1, beacon, beacon_dimensions))),
        power_pole: EntitySprite::new_tall(
            Sprite::new(Texture::new(1, power_pole, power_pole_dimensions)),
            1.0 / 2.0,
        ),

        lab: EntitySprite::new_tiling(Sprite::new(Texture::new(1, lab, lab_dimensions))),

        dark_square: Sprite::new(Texture::new(1, dark_square, dark_square_dimensions)),

        default: Sprite::new(Texture::default()),
    }
}
