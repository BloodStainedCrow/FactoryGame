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

macro_rules! sprite_from_path {
    ($path:literal, $number_anim_frames:expr) => {{
        let sprite = include_bytes!($path);
        let sprite = image::load_from_memory(sprite).unwrap();

        let sprite_dimensions = sprite.dimensions();
        let sprite = sprite.to_rgba8().into_vec();

        Sprite::new(Texture::new($number_anim_frames, sprite, sprite_dimensions))
    }};
}

macro_rules! entity_sprite_from_path_scaled {
    ($path:literal, $number_anim_frames:expr, $size:expr) => {{
        let sprite = include_bytes!($path);
        let sprite = image::load_from_memory(sprite).unwrap();

        let sprite_dimensions = sprite.dimensions();
        let sprite = sprite.to_rgba8().into_vec();

        EntitySprite::new_scaled(
            Sprite::new(Texture::new($number_anim_frames, sprite, sprite_dimensions)),
            $size,
        )
    }};
}

macro_rules! entity_sprite_from_path_tiling {
    ($path:literal, $number_anim_frames:expr) => {{
        let sprite = include_bytes!($path);
        let sprite = image::load_from_memory(sprite).unwrap();

        let sprite_dimensions = sprite.dimensions();
        let sprite = sprite.to_rgba8().into_vec();

        EntitySprite::new_tiling(Sprite::new(Texture::new(
            $number_anim_frames,
            sprite,
            sprite_dimensions,
        )))
    }};
}

macro_rules! entity_sprite_from_path_tall {
    ($path:literal, $number_anim_frames:expr, $ar:expr) => {{
        let sprite = include_bytes!($path);
        let sprite = image::load_from_memory(sprite).unwrap();

        let sprite_dimensions = sprite.dimensions();
        let sprite = sprite.to_rgba8().into_vec();

        EntitySprite::new_tall(
            Sprite::new(Texture::new($number_anim_frames, sprite, sprite_dimensions)),
            $ar,
        )
    }};
}

#[cfg(not(feature = "graphics"))]
fn texture_atlas() -> TextureAtlas {
    TextureAtlas {
        outside_world: sprite_from_path!("temp_assets/outside_world.png", 1),
        blue: sprite_from_path!("temp_assets/blue.png", 1),

        not_connected: entity_sprite_from_path_scaled!("temp_assets/not_connected.png", 1, 3.0),

        no_power: entity_sprite_from_path_scaled!("temp_assets/no_power.png", 1, 3.0),

        assembler: entity_sprite_from_path_tiling!("temp_assets/assembler.png", 1),
        chest: entity_sprite_from_path_tiling!("temp_assets/outside_world.png", 1),

        items: vec![sprite_from_path!("temp_assets/plate.png", 1); 200].into_boxed_slice(),

        player: sprite_from_path!("temp_assets/player.png", 1),
        belt: enum_map::EnumMap::from_array([
            sprite_from_path!("temp_assets/belt_north.png", 1),
            sprite_from_path!("temp_assets/belt_east.png", 1),
            sprite_from_path!("temp_assets/belt_south.png", 1),
            sprite_from_path!("temp_assets/belt_west.png", 1),
        ]),

        inserter: enum_map::EnumMap::from_array([
            sprite_from_path!("temp_assets/inserter_north.png", 1),
            sprite_from_path!("temp_assets/inserter_east.png", 1),
            sprite_from_path!("temp_assets/inserter_south.png", 1),
            sprite_from_path!("temp_assets/inserter_west.png", 1),
        ]),

        beacon: entity_sprite_from_path_tiling!("temp_assets/beacon.png", 1),
        power_pole: entity_sprite_from_path_tiling!("temp_assets/assembler.png", 1),

        lab: entity_sprite_from_path_tiling!("temp_assets/belt_north.png", 1),

        dark_square: sprite_from_path!("temp_assets/dark_square.png", 1),

        default: Sprite::new(Texture::default()),
    }
}

#[cfg(feature = "graphics")]
fn texture_atlas() -> TextureAtlas {
    TextureAtlas {
        outside_world: sprite_from_path!("temp_assets/outside_world.png", 1),
        blue: sprite_from_path!("temp_assets/blue.png", 1),

        not_connected: entity_sprite_from_path_scaled!("temp_assets/not_connected.png", 1, 3.0),

        no_power: entity_sprite_from_path_scaled!("temp_assets/no_power.png", 1, 3.0),

        assembler: entity_sprite_from_path_tiling!("temp_assets/krastorio/furnace.png", 7 * 4),
        chest: entity_sprite_from_path_tiling!("temp_assets/krastorio/chest.png", 1),

        items: vec![
            sprite_from_path!("temp_assets/krastorio/enriched-iron.png", 1),
            sprite_from_path!("temp_assets/krastorio/enriched-copper.png", 1),
            sprite_from_path!("temp_assets/krastorio/iron-plate.png", 1),
            sprite_from_path!("temp_assets/krastorio/copper-plate.png", 1),
            sprite_from_path!("temp_assets/krastorio/iron-gear-wheel.png", 1),
            sprite_from_path!("temp_assets/krastorio/automation-tech-card.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/krastorio/electronic-circuit.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
        ]
        .into_boxed_slice(),

        player: sprite_from_path!("temp_assets/player.png", 1),
        belt: enum_map::EnumMap::from_array([
            sprite_from_path!("temp_assets/belt_north.png", 1),
            sprite_from_path!("temp_assets/belt_east.png", 1),
            sprite_from_path!("temp_assets/belt_south.png", 1),
            sprite_from_path!("temp_assets/belt_west.png", 1),
        ]),

        inserter: enum_map::EnumMap::from_array([
            sprite_from_path!("temp_assets/inserter_north.png", 1),
            sprite_from_path!("temp_assets/inserter_east.png", 1),
            sprite_from_path!("temp_assets/inserter_south.png", 1),
            sprite_from_path!("temp_assets/inserter_west.png", 1),
        ]),

        beacon: entity_sprite_from_path_tiling!("temp_assets/beacon.png", 1),
        power_pole: entity_sprite_from_path_tall!(
            "temp_assets/krastorio/PowerPole.png",
            1,
            1.0 / 2.0
        ),

        lab: entity_sprite_from_path_tiling!("temp_assets/krastorio/advanced-lab.png", 1),

        dark_square: sprite_from_path!("temp_assets/dark_square.png", 1),

        default: Sprite::new(Texture::default()),
    }
}
