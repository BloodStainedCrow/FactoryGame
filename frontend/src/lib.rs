use std::time::Instant;

use data::spacial::{BoundingBox, Extent, Offset, Position};
use world::surface::{Surface, SurfaceCreationOptions};

use crate::blueprint::{Blueprint, string::BlueprintString};

mod action;
pub mod blueprint;
pub mod query;

pub use action::{ActionKind, ApplyActionError};

// TODO: This should prob not be default
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct SurfaceId(u32);

// TODO: Should this be here?
#[derive(Debug, Clone)]
pub struct GameState {
    surfaces: Vec<world::surface::Surface>,
    tech_state: (),
    player_states: (),
    // etc
}

impl GameState {
    #[must_use]
    pub fn new(surfaces: Vec<Surface>) -> Self {
        Self {
            surfaces,
            tech_state: (),
            player_states: (),
        }
    }
}

// FIXME: This should not exist
impl Default for GameState {
    fn default() -> Self {
        let mut ret = Self {
            surfaces: vec![Surface::new(&SurfaceCreationOptions {
                generated_area: BoundingBox::new(
                    Position { x: 0, y: 0 },
                    Extent {
                        width: 45_000,
                        height: 10_000,
                    },
                ),
            })],
            tech_state: Default::default(),
            player_states: Default::default(),
        };

        let bp = Blueprint::try_from(&BlueprintString(
            include_str!("../../test_blueprints/murphy/megabase_new_blueprint_format.bp")
                .to_string(),
        ))
        .expect("Failed to import Blueprint");

        // let str: BlueprintString = bp.clone().into();

        // dbg!(&str);

        // let round_trip: Blueprint = (&str).try_into().unwrap();

        // assert_eq!(bp, round_trip);

        let start = Instant::now();
        let mut positions = vec![];

        for x_offs in (0..40_000).step_by(5_000) {
            for action in bp
                .get_actions()
                .map(|a| a.offset_by(Offset { x_offs, y_offs: 0 }))
            {
                if let Some(pos) = action.get_building_position() {
                    positions.push(pos);
                }
                ret.apply_action(&action)
                    .unwrap_or_else(|e| panic!("Failed to apply action {action:?}. Error: {e}"));
            }
        }
        dbg!(start.elapsed());
        let start_remove = Instant::now();

        for pos in positions {
            ret.apply_action(&ActionKind::RemoveBuilding {
                surface_id: SurfaceId::default(),
                position: pos,
            })
            .expect("Failed to remove building");
        }

        dbg!(start_remove.elapsed());

        ret
    }
}
