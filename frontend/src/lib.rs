#![feature(never_type)]

use data::{
    entity::{GlobalTy, power_pole::PowerPoleTy},
    spacial::{BoundingBox, Extent, Flipped, Position, Rotation},
};
use world::surface::{Surface, SurfaceCreationOptions};

pub mod query;

// TODO: This should prob not be default
#[derive(Debug, Default, Clone, Copy)]
pub struct SurfaceId(u32);

// TODO: Should this be here?
#[derive(Debug, Clone)]
pub struct GameState {
    surfaces: Vec<world::surface::Surface>,
    tech_state: (),
    player_states: (),
    // etc
}

// FIXME: This should not exist
impl Default for GameState {
    fn default() -> Self {
        let mut ret = Self {
            surfaces: vec![Surface::new(&SurfaceCreationOptions {
                generated_area: BoundingBox::new(
                    Position { x: 0, y: 0 },
                    Extent {
                        width: 10_000,
                        height: 10_000,
                    },
                ),
            })],
            tech_state: Default::default(),
            player_states: Default::default(),
        };

        ret.surfaces[0].add_power_pole(
            PowerPoleTy::try_from(GlobalTy::from(0)).unwrap(),
            Position { x: 0, y: 0 },
            Rotation::North,
            Flipped::unflipped(),
        );

        ret
    }
}
