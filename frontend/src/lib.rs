use data::spacial::{BoundingBox, Extent, Position};
use world::surface::{Surface, SurfaceCreationOptions};

use crate::blueprint::{Blueprint, string::BlueprintString};

mod action;
pub mod blueprint;
pub mod query;

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

// FIXME: This should not exist
impl Default for GameState {
    fn default() -> Self {
        let mut ret = Self {
            surfaces: vec![Surface::new(&SurfaceCreationOptions {
                generated_area: BoundingBox::new(
                    Position { x: 0, y: 0 },
                    Extent {
                        width: 5_000,
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

        for action in bp.get_actions() {
            ret.apply_action(action)
                .expect(&format!("Failed to apply action {:?}", action));
        }

        ret
    }
}
