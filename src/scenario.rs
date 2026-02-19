use std::ops::RangeInclusive;

use rand::Rng;
use rand_xoshiro::rand_core::SeedableRng;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) struct ScenarioInfo {
    // The extent of the world.
    // Used for ribbon worlds
    world_x_range: RangeInclusive<i32>,
    world_y_range: RangeInclusive<i32>,

    // The player will spawn in a random position in this rect
    player_spawn_area: [RangeInclusive<i32>; 2],

    pre_placed_structures: ScenarioStructureList,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) struct Seed([u8; 16]);

impl ScenarioInfo {
    fn instantiate(
        &self,
        seed: Seed,
        pre_placed_structure_count: Option<usize>,
    ) -> ScenarioInfoInstantiation<'_> {
        let mut random = rand_xoshiro::Xoroshiro128Plus::from_seed(seed.0);

        let x = random.random_range(self.player_spawn_area[0].clone());
        let y = random.random_range(self.player_spawn_area[1].clone());

        ScenarioInfoInstantiation {
            seed,

            world_x_range: self.world_x_range.clone(),
            world_y_range: self.world_y_range.clone(),
            player_spawn_area: [x, y],

            pre_placed_structure_count,
            pre_placed_structures: &self.pre_placed_structures,
        }
    }
}

#[derive(Debug, Clone, serde::Serialize)]
pub(crate) struct ScenarioInfoInstantiation<'a> {
    seed: Seed,

    world_x_range: RangeInclusive<i32>,
    world_y_range: RangeInclusive<i32>,

    player_spawn_area: [i32; 2],

    pre_placed_structure_count: Option<usize>,
    pre_placed_structures: &'a ScenarioStructureList,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) struct ScenarioStructureList {
    structures: Vec<ScenarioStructureDescription>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) enum ScenarioStructureKind {
    Oneshot {
        structure: ScenarioStructureDescription,
    },
    Tiled {
        structure: ScenarioStructureDescription,
        tiling_info: (),

        variable_count: Option<RangeInclusive<usize>>,
    },
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) struct ScenarioStructureDescription {}

pub(crate) const EMPTY_LAB_WORLD: ScenarioInfo = ScenarioInfo {
    world_x_range: -1_000_000..=1_000_000,
    world_y_range: -1_000_000..=1_000_000,

    player_spawn_area: [0..=0, 0..=0],

    pre_placed_structures: ScenarioStructureList { structures: vec![] },
};
