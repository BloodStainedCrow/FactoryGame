use criterion::{Criterion, criterion_group, criterion_main};
use data::spacial::{BoundingBox, Extent, Position};
use frontend::{
    GameState,
    blueprint::{Blueprint, string::BlueprintString},
};
use std::{hint::black_box, sync::LazyLock};
use world::surface::{Surface, SurfaceCreationOptions};

const MEGABASE_STR: LazyLock<BlueprintString> = LazyLock::new(|| {
    BlueprintString(
        include_str!("../../test_blueprints/murphy/megabase_new_blueprint_format.bp").to_string(),
    )
});

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("deserialize megabase", |b| {
        b.iter(|| {
            let bp: Blueprint = (&*MEGABASE_STR).try_into().unwrap();

            black_box(bp);
        })
    });

    let bp = Blueprint::try_from(&BlueprintString(
        include_str!("../../test_blueprints/murphy/megabase_new_blueprint_format.bp").to_string(),
    ))
    .expect("Failed to import Blueprint");

    c.bench_function("apply megabase", |b| {
        let mut game_state = GameState::new(vec![Surface::new(&SurfaceCreationOptions {
            generated_area: BoundingBox::new(
                Position { x: 0, y: 0 },
                Extent {
                    width: 5_000,
                    height: 10_000,
                },
            ),
        })]);
        b.iter_batched(
            || game_state.clone(),
            |mut game_state| {
                for action in bp.get_actions() {
                    game_state
                        .apply_action(&action)
                        .expect("Failed to apply megabase action");
                }
            },
            criterion::BatchSize::LargeInput,
        );
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
