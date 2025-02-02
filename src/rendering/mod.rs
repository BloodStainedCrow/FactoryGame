use tilelib::types::Sprite;

pub mod app_state;
mod render_world;
pub mod window;

pub struct TextureAtlas {
    outside_world: Sprite,
    blue: Sprite,

    assembler: Sprite,
    belt_north: Sprite,

    plate: Sprite,

    default: Sprite,
}
