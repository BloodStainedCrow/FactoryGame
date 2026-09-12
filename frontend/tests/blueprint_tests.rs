use std::path::Path;

use data::spacial::{BoundingBox, Extent, Position};
use frontend::GameState;
use frontend::blueprint::{Blueprint, string::BlueprintString};
use world::surface::{Surface, SurfaceCreationOptions};

datatest_stable::harness! {
    { test = should_be_accepted, root = "../test_blueprints/fuzzing/accepted", pattern = r"^.*\.bp$" },
    { test = should_be_rejected, root = "../test_blueprints/fuzzing/rejected", pattern = r"^.*\.bp$" },
}

fn fresh_game_state() -> GameState {
    GameState::new(vec![Surface::new(&SurfaceCreationOptions {
        generated_area: BoundingBox::new(
            Position { x: -500, y: -500 },
            Extent {
                width: 1000,
                height: 1000,
            },
        ),
    })])
}

fn parse_blueprint(path: &Path, contents: String) -> datatest_stable::Result<Blueprint> {
    Blueprint::try_from(&BlueprintString(contents))
        .map_err(|err| format!("{path:?}: could not parse blueprint: {err:?}").into())
}

fn should_be_accepted(path: &Path, contents: String) -> datatest_stable::Result<()> {
    let blueprint = parse_blueprint(path, contents)?;
    let mut state = fresh_game_state();

    if blueprint.apply_to(&mut state).is_err() {
        return Err(format!("{path:?}: blueprint was rejected").into());
    }

    Ok(())
}

fn should_be_rejected(path: &Path, contents: String) -> datatest_stable::Result<()> {
    let blueprint = parse_blueprint(path, contents)?;
    let mut state = fresh_game_state();

    if blueprint.apply_to(&mut state).is_ok() {
        return Err(format!(
            "{path:?}: expected the blueprint to be rejected, but it was accepted"
        )
        .into());
    }

    Ok(())
}
