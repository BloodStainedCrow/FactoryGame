use image::GenericImageView;
use tilelib::types::{Sprite, Texture};

use crate::frontend::world::tile::Dir;

pub mod app_state;
pub mod eframe_app;
mod render_world;
pub mod window;

#[derive(Debug)]
pub struct TextureAtlas {
    outside_world: Sprite,
    blue: Sprite,

    assembler: Sprite,
    no_power: Sprite,
    not_connected: Sprite,
    belt: enum_map::EnumMap<Dir, Sprite>,
    inserter: enum_map::EnumMap<Dir, Sprite>,

    player: Sprite,

    plate: Sprite,

    default: Sprite,
}

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

    TextureAtlas {
        outside_world: Sprite::new(Texture::new(1, black, black_dimensions)),
        assembler: Sprite::new(Texture::new(1, assembler, assembler_dimensions)),
        blue: Sprite::new(Texture::new(1, blue, blue_dimensions)),

        not_connected: Sprite::new(Texture::new(1, not_connected, not_connected_dimensions)),

        no_power: Sprite::new(Texture::new(1, no_power, no_power_dimensions)),

        plate: Sprite::new(Texture::new(1, plate, plate_dimensions)),

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

        default: Sprite::new(Texture::default()),
    }
}
