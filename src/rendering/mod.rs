use image::GenericImageView;
use tilelib::types::{DrawInstance, Layer, Sprite, Texture};

use crate::frontend::world::tile::{Dir, UndergroundDir};

pub mod app_state;
pub mod eframe_app;
pub mod render_world;
pub mod window;

pub mod map_view;

use enum_map::{Enum, EnumArray};

#[derive(Debug, Clone)]
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

    const fn new_scaled_centered(sprite: Sprite, size: f32) -> Self {
        Self {
            sprite,
            aspect_ratio: 1.0,
            offset: (-(size - 1.0) / 2.0, -(size - 1.0) / 2.0),
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

#[derive(Debug, Clone, Copy, Enum)]
enum BeltSide {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy)]
struct Corner {
    to_dir: Dir,
    from_dir: BeltSide,
}

impl Enum for Corner {
    const LENGTH: usize = Dir::LENGTH * BeltSide::LENGTH;

    fn from_usize(value: usize) -> Self {
        let (to, from) = (value / BeltSide::LENGTH, value % BeltSide::LENGTH);

        Self {
            to_dir: Dir::from_usize(to),
            from_dir: BeltSide::from_usize(from),
        }
    }

    fn into_usize(self) -> usize {
        let Self { from_dir, to_dir } = self;

        from_dir.into_usize() + to_dir.into_usize() * BeltSide::LENGTH
    }
}

impl<V> EnumArray<V> for Corner {
    type Array = [V; Self::LENGTH];
}

#[derive(Debug)]
pub struct TextureAtlas {
    outside_world: Sprite,
    blue: Sprite,

    chest: EntitySprite,

    assembler: EntitySprite,
    no_power: EntitySprite,
    not_connected: EntitySprite,
    belt: enum_map::EnumMap<Dir, EntitySprite>,
    belt_corners: enum_map::EnumMap<Corner, EntitySprite>,
    inserter: enum_map::EnumMap<Dir, Sprite>,

    player: Sprite,

    items: Box<[Sprite]>,

    beacon: EntitySprite,
    power_pole: EntitySprite,

    lab: EntitySprite,

    pub dark_square: Sprite,

    underground: enum_map::EnumMap<Dir, enum_map::EnumMap<UndergroundDir, EntitySprite>>,

    default: Sprite,
}

macro_rules! sprite_from_path {
    ($path:literal, $number_anim_frames:expr_2021) => {{
        let sprite = include_bytes!($path);
        let sprite = image::load_from_memory(sprite).unwrap();

        let sprite_dimensions = sprite.dimensions();
        let sprite = sprite.to_rgba8().into_vec();

        Sprite::new(Texture::new($number_anim_frames, sprite, sprite_dimensions))
    }};
}

macro_rules! entity_sprite_from_path_scaled {
    ($path:literal, $number_anim_frames:expr_2021, $size:expr_2021) => {{
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

macro_rules! entity_sprite_from_path_scaled_centered {
    ($path:literal, $number_anim_frames:expr_2021, $size:expr_2021) => {{
        let sprite = include_bytes!($path);
        let sprite = image::load_from_memory(sprite).unwrap();

        let sprite_dimensions = sprite.dimensions();
        let sprite = sprite.to_rgba8().into_vec();

        EntitySprite::new_scaled_centered(
            Sprite::new(Texture::new($number_anim_frames, sprite, sprite_dimensions)),
            $size,
        )
    }};
}

macro_rules! entity_sprite_from_path_tiling {
    ($path:literal, $number_anim_frames:expr_2021) => {{
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
    ($path:literal, $number_anim_frames:expr_2021, $ar:expr_2021) => {{
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
    let belts: enum_map::EnumMap<Dir, EntitySprite> = enum_map::EnumMap::from_array([
        entity_sprite_from_path_tiling!("temp_assets/belt_north.png", 1),
        entity_sprite_from_path_tiling!("temp_assets/belt_east.png", 1),
        entity_sprite_from_path_tiling!("temp_assets/belt_south.png", 1),
        entity_sprite_from_path_tiling!("temp_assets/belt_west.png", 1),
    ]);

    let undergrounds = enum_map::EnumMap::from_fn(|dir| {
        enum_map::EnumMap::from_array([belts[dir].clone(), belts[dir].clone()])
    });

    TextureAtlas {
        outside_world: sprite_from_path!("temp_assets/outside_world.png", 1),
        blue: sprite_from_path!("temp_assets/blue.png", 1),

        not_connected: entity_sprite_from_path_scaled!("temp_assets/not_connected.png", 1, 3.0),

        no_power: entity_sprite_from_path_scaled!("temp_assets/no_power.png", 1, 3.0),

        assembler: entity_sprite_from_path_tiling!("temp_assets/assembler.png", 1),
        chest: entity_sprite_from_path_tiling!("temp_assets/outside_world.png", 1),

        items: vec![sprite_from_path!("temp_assets/plate.png", 1); 200].into_boxed_slice(),

        player: sprite_from_path!("temp_assets/player.png", 1),
        belt: belts,

        belt_corners: enum_map::EnumMap::from_array([
            entity_sprite_from_path_tiling!("temp_assets/belt_north.png", 1),
            entity_sprite_from_path_tiling!("temp_assets/belt_north.png", 1),
            entity_sprite_from_path_tiling!("temp_assets/belt_east.png", 1),
            entity_sprite_from_path_tiling!("temp_assets/belt_east.png", 1),
            entity_sprite_from_path_tiling!("temp_assets/belt_south.png", 1),
            entity_sprite_from_path_tiling!("temp_assets/belt_south.png", 1),
            entity_sprite_from_path_tiling!("temp_assets/belt_west.png", 1),
            entity_sprite_from_path_tiling!("temp_assets/belt_west.png", 1),
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

        underground: undergrounds,

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
            sprite_from_path!("temp_assets/krastorio/logistic-tech-card.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/krastorio/chemical-tech-card.png", 1),
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
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
            sprite_from_path!("temp_assets/plate.png", 1),
        ]
        .into_boxed_slice(),

        player: sprite_from_path!("temp_assets/player.png", 1),
        belt: enum_map::EnumMap::from_array([
            entity_sprite_from_path_scaled_centered!(
                "temp_assets/krastorio/belt/north.png",
                32,
                2.0
            ),
            entity_sprite_from_path_scaled_centered!(
                "temp_assets/krastorio/belt/east.png",
                32,
                2.0
            ),
            entity_sprite_from_path_scaled_centered!(
                "temp_assets/krastorio/belt/south.png",
                32,
                2.0
            ),
            entity_sprite_from_path_scaled_centered!(
                "temp_assets/krastorio/belt/west.png",
                32,
                2.0
            ),
        ]),

        belt_corners: enum_map::EnumMap::from_array([
            entity_sprite_from_path_scaled_centered!(
                "temp_assets/krastorio/belt/west-north.png",
                32,
                2.0
            ),
            entity_sprite_from_path_scaled_centered!(
                "temp_assets/krastorio/belt/east-north.png",
                32,
                2.0
            ),
            entity_sprite_from_path_scaled_centered!(
                "temp_assets/krastorio/belt/north-east.png",
                32,
                2.0
            ),
            entity_sprite_from_path_scaled_centered!(
                "temp_assets/krastorio/belt/south-east.png",
                32,
                2.0
            ),
            entity_sprite_from_path_scaled_centered!(
                "temp_assets/krastorio/belt/east-south.png",
                32,
                2.0
            ),
            entity_sprite_from_path_scaled_centered!(
                "temp_assets/krastorio/belt/west-south.png",
                32,
                2.0
            ),
            entity_sprite_from_path_scaled_centered!(
                "temp_assets/krastorio/belt/south-west.png",
                32,
                2.0
            ),
            entity_sprite_from_path_scaled_centered!(
                "temp_assets/krastorio/belt/north-west.png",
                32,
                2.0
            ),
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

        underground: enum_map::EnumMap::from_array([
            enum_map::EnumMap::from_array([
                entity_sprite_from_path_scaled_centered!(
                    "temp_assets/krastorio/underground/north-entrance.png",
                    1,
                    3.0
                ),
                entity_sprite_from_path_scaled_centered!(
                    "temp_assets/krastorio/underground/north-exit.png",
                    1,
                    3.0
                ),
            ]),
            enum_map::EnumMap::from_array([
                entity_sprite_from_path_scaled_centered!(
                    "temp_assets/krastorio/underground/east-entrance.png",
                    1,
                    3.0
                ),
                entity_sprite_from_path_scaled_centered!(
                    "temp_assets/krastorio/underground/east-exit.png",
                    1,
                    3.0
                ),
            ]),
            enum_map::EnumMap::from_array([
                entity_sprite_from_path_scaled_centered!(
                    "temp_assets/krastorio/underground/south-entrance.png",
                    1,
                    3.0
                ),
                entity_sprite_from_path_scaled_centered!(
                    "temp_assets/krastorio/underground/south-exit.png",
                    1,
                    3.0
                ),
            ]),
            enum_map::EnumMap::from_array([
                entity_sprite_from_path_scaled_centered!(
                    "temp_assets/krastorio/underground/west-entrance.png",
                    1,
                    3.0
                ),
                entity_sprite_from_path_scaled_centered!(
                    "temp_assets/krastorio/underground/west-exit.png",
                    1,
                    3.0
                ),
            ]),
        ]),

        default: Sprite::new(Texture::default()),
    }
}
