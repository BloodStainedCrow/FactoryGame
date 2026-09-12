use std::path::Path;

use data::spacial::{BoundingBox, Extent, Position};
use frontend::blueprint::{Blueprint, string::BlueprintString};
use frontend::{ActionKind, GameState};
use proptest::{
    prelude::*,
    test_runner::{Config, FileFailurePersistence, TestError, TestRunner},
};
use world::surface::{Surface, SurfaceCreationOptions};

datatest_stable::harness! {
    { test = should_not_be_applicable_to_modset, root = "../test_blueprints/fuzzing/not_apply", pattern = r"^.*\.bp$" },
    { test = should_be_accepted, root = "../test_blueprints/fuzzing/accepted", pattern = r"^.*\.bp$" },
    // TODO: Currently no rejected blueprints
    // { test = should_be_rejected, root = "../test_blueprints/fuzzing/rejected", pattern = r"^.*\.bp$" },
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

fn check_orders(
    path: &Path,
    actions: &[ActionKind],
    expect_accepted: bool,
) -> datatest_stable::Result<()> {
    let strategy = Just((0..actions.len()).collect::<Vec<usize>>()).prop_shuffle();
    let mut runner = TestRunner::new(Config {
        failure_persistence: Some(Box::new(FileFailurePersistence::Direct(
            "../proptest-regressions/blueprint_tests.txt",
        ))),
        ..Config::default()
    });

    let result = runner.run(&strategy, |order| {
        let mut state = fresh_game_state();
        let reordered = Blueprint::from(
            order
                .iter()
                .map(|&index| actions[index].clone())
                .collect::<Vec<_>>(),
        );

        let accepted = reordered.apply_to(&mut state).is_ok();
        if accepted != expect_accepted {
            return Err(TestCaseError::fail(format!(
                "actions in order {order:?} were {}",
                if accepted { "accepted" } else { "rejected" }
            )));
        }

        Ok(())
    });

    result.map_err(|error| -> Box<dyn std::error::Error> {
        match error {
            TestError::Fail(reason, order) => {
                format!("{path:?}: order {order:?} failed: {reason}").into()
            },
            TestError::Abort(reason) => format!("{path:?}: {reason}").into(),
        }
    })?;

    Ok(())
}

fn should_not_be_applicable_to_modset(
    path: &Path,
    contents: String,
) -> datatest_stable::Result<()> {
    let res = parse_blueprint(path, contents);

    match res {
        Ok(_) => Err(format!("Parsed but should be rejected by modset").into()),
        Err(e) => Ok(()),
    }
}

fn should_be_accepted(path: &Path, contents: String) -> datatest_stable::Result<()> {
    let blueprint = parse_blueprint(path, contents)?;
    let actions: Vec<_> = blueprint.get_actions().cloned().collect();

    check_orders(path, &actions, true)
}

fn should_be_rejected(path: &Path, contents: String) -> datatest_stable::Result<()> {
    let blueprint = parse_blueprint(path, contents)?;
    let actions: Vec<_> = blueprint.get_actions().cloned().collect();

    check_orders(path, &actions, false)
}
