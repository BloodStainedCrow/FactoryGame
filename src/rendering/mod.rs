use tilelib::types::Sprite;

use crate::frontend::world::tile::Dir;

pub mod app_state;
mod render_world;
pub mod window;

pub struct TextureAtlas {
    outside_world: Sprite,
    blue: Sprite,

    assembler: Sprite,
    belt: enum_map::EnumMap<Dir, Sprite>,
    inserter: enum_map::EnumMap<Dir, Sprite>,

    plate: Sprite,

    default: Sprite,
}
