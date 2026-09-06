use criterion::{Criterion, criterion_group, criterion_main};
use frontend::{
    GameState,
    blueprint::{Blueprint, string::BlueprintString},
};
use std::{hint::black_box, sync::LazyLock};

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

    c.bench_function("create gamestate", |b| {
        b.iter(|| {
            black_box(GameState::default());
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
