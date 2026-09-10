/// This is the blueprint format of the pre-refactor codebase. This is here only so I can reuse (and reexport) my test blueprints.
use std::{collections::BTreeMap, io::BufReader, num::NonZero, u32};

use base64::engine::general_purpose::STANDARD;
use data::{
    EntityIdentifier,
    entity::{
        GlobalTy, assember::AssemblerTy, belt::BeltTy, chest::ChestTy, inserter::InserterTy,
        power_pole::PowerPoleTy,
    },
    spacial::{Direction, Flipped, Position, Rotation},
};
use log::error;

use crate::{
    action::{ActionKind, BuildingInfo, BuildingKind, ForceKind},
    blueprint::versions::VersionedBlueprint,
};

#[derive(Debug)]
pub struct Blueprint {
    actions: Vec<BlueprintAction>,
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum BlueprintAction {
    PlaceEntity(BlueprintPlaceEntity),

    SetRecipe {
        pos: Position,
        recipe: String,
    },

    OverrideInserterMovetime {
        pos: Position,
        new_movetime: Option<NonZero<u16>>,
    },

    AddModules {
        pos: Position,
        modules: Vec<String>,
    },

    SetChestSlotLimit {
        pos: Position,
        num_slots: u8,
    },

    PlaceOre {
        ore: String,
        pos: Position,
        amount: u32,
    },
}

type BeltLenType = u16;

#[derive(
    Debug, Clone, Copy, serde::Deserialize, serde::Serialize, PartialEq, Eq, Hash, PartialOrd, Ord,
)]
struct BeltId {
    pub item: Item<u8>,
    pub index: usize,
}

#[derive(
    Debug, PartialEq, Eq, Hash, Clone, Copy, serde::Serialize, serde::Deserialize, PartialOrd, Ord,
)]
struct Item<ItemIdxType> {
    pub id: ItemIdxType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Deserialize, serde::Serialize)]
enum UndergroundDir {
    Entrance,
    Exit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize)]
enum SplitterSide {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
enum SplitterDistributionMode {
    Fair { next: SplitterSide },
    Priority(SplitterSide),
    // TODO: Filter
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
enum BlueprintPlaceEntity {
    Assembler {
        pos: Position,
        ty: String,
        #[serde(default = "Dir::default")]
        rotation: Dir,
    },
    Inserter {
        pos: Position,
        dir: Dir,
        /// The Item the inserter will move, must fit both the in and output side
        filter: Option<String>,

        movetime: Option<NonZero<u16>>,

        #[serde(default = "default_inserter")]
        ty: String,
    },
    Belt {
        pos: Position,
        direction: Dir,
        ty: String,
        #[serde(default)]
        copied_belt_info: Option<(BeltId, BeltLenType)>,
    },
    Underground {
        pos: Position,
        direction: Dir,
        ty: String,
        underground_dir: UndergroundDir,
        #[serde(default)]
        copied_belt_info: Option<(BeltId, BeltLenType)>,
    },
    PowerPole {
        pos: Position,
        ty: String,
    },
    Splitter {
        pos: Position,
        direction: Dir,
        ty: String,

        in_mode: Option<SplitterDistributionMode>,
        out_mode: Option<SplitterDistributionMode>,
    },
    Chest {
        pos: Position,
        ty: String,
    },
    SolarPanel {
        pos: Position,
        ty: String,
    },
    Accumulator {
        pos: Position,
        ty: String,
    },
    Lab {
        pos: Position,
        ty: String,
    },
    Beacon {
        ty: String,
        pos: Position,
    },
    FluidTank {
        ty: String,
        pos: Position,
        rotation: Dir,
    },
    MiningDrill {
        ty: String,
        pos: Position,
        rotation: Dir,
    },
}

fn default_inserter() -> String {
    "factory_game::bulk_inserter".to_string()
}

#[derive(
    Debug, Clone, Copy, Default, serde::Serialize, serde::Deserialize, PartialEq, Eq, Hash,
)]
enum Dir {
    #[default]
    North,
    East,
    South,
    West,
}

impl Into<Direction> for Dir {
    fn into(self) -> Direction {
        match self {
            Dir::North => Direction::North,
            Dir::East => Direction::East,
            Dir::South => Direction::South,
            Dir::West => Direction::West,
        }
    }
}

impl VersionedBlueprint for Blueprint {
    fn get_version() -> u32 {
        u32::MAX
    }
}

impl<'a> TryFrom<&'a [u8]> for Blueprint {
    type Error = !;

    fn try_from(value: &'a [u8]) -> Result<Self, Self::Error> {
        let blueprint_string = str::from_utf8(value).expect("Not UTF8");

        blueprint_string.try_into()
    }
}

impl Into<super::Blueprint> for Blueprint {
    fn into(self) -> super::Blueprint {
        let Self { actions } = self;

        let mut state = BTreeMap::new();

        for action in actions {
            match action {
                BlueprintAction::PlaceEntity(blueprint_place_entity) => {
                    match blueprint_place_entity {
                        BlueprintPlaceEntity::Assembler { pos, ty, rotation } => state.insert(
                            pos,
                            ActionKind::PlaceBuilding {
                                ghost: true,
                                force: ForceKind::None,
                                building_info: BuildingInfo {
                                    position: pos,
                                    rotation: match rotation {
                                        Dir::North => Rotation::North,
                                        Dir::East => Rotation::East,
                                        Dir::South => Rotation::South,
                                        Dir::West => Rotation::West,
                                    },
                                    flipped: Flipped::unflipped(),
                                    kind: BuildingKind::Assembler {
                                        ty: AssemblerTy::try_from(
                                            GlobalTy::try_from(EntityIdentifier::new_raw(ty))
                                                .expect("Entity does not exist"),
                                        )
                                        .expect("Entity name is not an assembler"),
                                        recipe: None,
                                        modules: vec![],
                                    },
                                },
                            },
                        ),
                        BlueprintPlaceEntity::Inserter {
                            pos,
                            dir,
                            filter,
                            movetime,
                            ty,
                        } => state.insert(
                            pos,
                            ActionKind::PlaceBuilding {
                                ghost: true,
                                force: ForceKind::None,
                                building_info: BuildingInfo {
                                    position: pos,
                                    rotation: match dir {
                                        Dir::North => Rotation::North,
                                        Dir::East => Rotation::East,
                                        Dir::South => Rotation::South,
                                        Dir::West => Rotation::West,
                                    },
                                    flipped: Flipped::unflipped(),
                                    kind: BuildingKind::Inserter {
                                        ty: InserterTy::try_from(
                                            GlobalTy::try_from(EntityIdentifier::new_raw(ty))
                                                .expect("Entity does not exist"),
                                        )
                                        .expect("Entity name is not an inserter"),
                                    },
                                },
                            },
                        ),
                        BlueprintPlaceEntity::Belt {
                            pos,
                            direction,
                            ty,
                            copied_belt_info,
                        } => state.insert(
                            pos,
                            ActionKind::PlaceBuilding {
                                ghost: true,
                                force: ForceKind::None,
                                building_info: BuildingInfo {
                                    position: pos,
                                    rotation: match direction {
                                        Dir::North => Rotation::North,
                                        Dir::East => Rotation::East,
                                        Dir::South => Rotation::South,
                                        Dir::West => Rotation::West,
                                    },
                                    flipped: Flipped::unflipped(),
                                    kind: BuildingKind::Belt {
                                        ty: BeltTy::try_from(
                                            GlobalTy::try_from(EntityIdentifier::new_raw(ty))
                                                .expect("Entity does not exist"),
                                        )
                                        .expect("Entity name is not an belt"),
                                    },
                                },
                            },
                        ),
                        BlueprintPlaceEntity::Underground {
                            pos,
                            direction,
                            ty,
                            underground_dir,
                            copied_belt_info,
                        } => state.insert(
                            pos,
                            ActionKind::PlaceBuilding {
                                ghost: true,
                                force: ForceKind::None,
                                building_info: BuildingInfo {
                                    position: pos,
                                    rotation: match direction {
                                        Dir::North => Rotation::North,
                                        Dir::East => Rotation::East,
                                        Dir::South => Rotation::South,
                                        Dir::West => Rotation::West,
                                    },
                                    flipped: Flipped::unflipped(),
                                    kind: BuildingKind::Belt {
                                        ty: BeltTy::try_from(
                                            GlobalTy::try_from(EntityIdentifier::new_raw(ty))
                                                .expect("Entity does not exist"),
                                        )
                                        .expect("Entity name is not an belt"),
                                    },
                                },
                            },
                        ),
                        BlueprintPlaceEntity::PowerPole { pos, ty } => state.insert(
                            pos,
                            ActionKind::PlaceBuilding {
                                ghost: true,
                                force: ForceKind::None,
                                building_info: BuildingInfo {
                                    position: pos,
                                    rotation: Rotation::North,
                                    flipped: Flipped::unflipped(),
                                    kind: BuildingKind::PowerPole {
                                        ty: PowerPoleTy::try_from(
                                            GlobalTy::try_from(EntityIdentifier::new_raw(ty))
                                                .expect("Entity does not exist"),
                                        )
                                        .expect("Entity name is not an pole"),
                                    },
                                },
                            },
                        ),
                        BlueprintPlaceEntity::Splitter {
                            pos,
                            direction,
                            ty,
                            in_mode,
                            out_mode,
                        } => {
                            // TODO:
                            None
                        },
                        BlueprintPlaceEntity::Chest { pos, ty } => state.insert(
                            pos,
                            ActionKind::PlaceBuilding {
                                ghost: true,
                                force: ForceKind::None,
                                building_info: BuildingInfo {
                                    position: pos,
                                    rotation: Rotation::North,
                                    flipped: Flipped::unflipped(),
                                    kind: BuildingKind::Chest {
                                        ty: ChestTy::try_from(
                                            GlobalTy::try_from(EntityIdentifier::new_raw(ty))
                                                .expect("Entity does not exist"),
                                        )
                                        .expect("Entity name is not an chest"),
                                    },
                                },
                            },
                        ),
                        BlueprintPlaceEntity::SolarPanel { pos, ty } => {
                            // TODO:
                            None
                        },
                        BlueprintPlaceEntity::Accumulator { pos, ty } => {
                            // TODO:
                            None
                        },
                        BlueprintPlaceEntity::Lab { pos, ty } => {
                            // TODO:
                            None
                        },
                        BlueprintPlaceEntity::Beacon { ty, pos } => {
                            // TODO:
                            None
                        },
                        BlueprintPlaceEntity::FluidTank { ty, pos, rotation } => {
                            // TODO:
                            None
                        },
                        BlueprintPlaceEntity::MiningDrill { ty, pos, rotation } => {
                            // TODO:
                            None
                        },
                    };
                },
                BlueprintAction::SetRecipe { pos, recipe } => {
                    let ActionKind::PlaceBuilding {
                        ghost,
                        force,
                        building_info:
                            BuildingInfo {
                                position,
                                rotation,
                                flipped,
                                kind:
                                    BuildingKind::Assembler {
                                        ty,
                                        recipe: _old_recipe,
                                        modules,
                                    },
                            },
                    } = state[&pos].clone()
                    else {
                        unreachable!()
                    };

                    state.insert(
                        pos,
                        ActionKind::PlaceBuilding {
                            ghost,
                            force,
                            building_info: BuildingInfo {
                                position,
                                rotation,
                                flipped,
                                kind: BuildingKind::Assembler {
                                    ty,
                                    recipe: Some(recipe.try_into().expect("Failed to load recipe")),
                                    modules,
                                },
                            },
                        },
                    );
                },
                BlueprintAction::OverrideInserterMovetime { pos, new_movetime } => {
                    // TODO:
                },
                BlueprintAction::AddModules { pos, modules } => {
                    // TODO:
                },
                BlueprintAction::SetChestSlotLimit { pos, num_slots } => {
                    // TODO:
                },
                BlueprintAction::PlaceOre { ore, pos, amount } => {
                    // TODO:
                },
            }
        }

        super::Blueprint {
            actions: state.into_values().collect(),
        }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct BaseEntity {
    pos: Position,
    ty: EntityKind,
    rotation: Dir,
}
type EntityKind = usize;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct BlueprintStringInternal {
    data_strings: Vec<String>,

    solar_panels: Vec<BaseEntity>,
    accumulators: Vec<BaseEntity>,
    belts: Vec<BaseEntity>,
    underground_belts: Vec<(BaseEntity, UndergroundDir)>,
    power_poles: Vec<BaseEntity>,
    beacons: Vec<BaseEntity>,
    chests: Vec<BaseEntity>,
    labs: Vec<BaseEntity>,
    fluid_tanks: Vec<BaseEntity>,
    mining_drills: Vec<BaseEntity>,
    assemblers: Vec<BaseEntity>,

    splitters: Vec<(
        BaseEntity,
        Option<SplitterDistributionMode>,
        Option<SplitterDistributionMode>,
    )>,

    inserters: Vec<(BaseEntity, Option<usize>, Option<NonZero<u16>>)>,

    set_recipe: Vec<(Position, usize)>,
    movetime: Vec<(Position, Option<NonZero<u16>>)>,
    slot_limit: Vec<(Position, u8)>,

    #[serde(skip)]
    ores: Vec<(Position, usize, u32)>,

    module_combinations: Vec<Vec<usize>>,
    modules: Vec<(Position, usize)>,
}

impl<'a> TryFrom<&'a str> for Blueprint {
    type Error = !;
    fn try_from(raw_str: &'a str) -> Result<Self, Self::Error> {
        let reader = base64::read::DecoderReader::new(raw_str.as_bytes(), &STANDARD);
        let dec = BufReader::new(flate2::read::DeflateDecoder::new(reader));

        let Ok(internal) = bincode::serde::decode_from_reader(dec, bincode::config::standard())
        else {
            error!("Blueprint failed to deserialize!");
            return unimplemented!("This exists only for legacy reasons");
        };

        let BlueprintStringInternal {
            data_strings,
            solar_panels,
            accumulators,
            belts,
            underground_belts,
            power_poles,
            beacons,
            chests,
            labs,
            fluid_tanks,
            mining_drills,
            assemblers,
            splitters,
            inserters,
            set_recipe,
            movetime,
            slot_limit,
            module_combinations,
            modules,
            ores,
        } = internal;

        // dbg!(&movetime);

        let actions = assemblers
            .into_iter()
            .map(|BaseEntity { pos, ty, rotation }| {
                BlueprintAction::PlaceEntity(BlueprintPlaceEntity::Assembler {
                    pos,
                    ty: data_strings[ty].clone(),
                    rotation,
                })
            });

        let actions = actions.chain(beacons.into_iter().map(
            |BaseEntity {
                 pos,
                 ty,
                 rotation: _,
             }| {
                BlueprintAction::PlaceEntity(BlueprintPlaceEntity::Beacon {
                    pos,
                    ty: data_strings[ty].clone(),
                })
            },
        ));

        let actions = actions.chain(chests.into_iter().map(
            |BaseEntity {
                 pos,
                 ty,
                 rotation: _,
             }| {
                BlueprintAction::PlaceEntity(BlueprintPlaceEntity::Chest {
                    pos,
                    ty: data_strings[ty].clone(),
                })
            },
        ));

        let actions = actions.chain(solar_panels.into_iter().map(
            |BaseEntity {
                 pos,
                 ty,
                 rotation: _,
             }| {
                BlueprintAction::PlaceEntity(BlueprintPlaceEntity::SolarPanel {
                    pos,
                    ty: data_strings[ty].clone(),
                })
            },
        ));

        let actions = actions.chain(accumulators.into_iter().map(
            |BaseEntity {
                 pos,
                 ty,
                 rotation: _,
             }| {
                BlueprintAction::PlaceEntity(BlueprintPlaceEntity::Accumulator {
                    pos,
                    ty: data_strings[ty].clone(),
                })
            },
        ));

        let actions = actions.chain(belts.into_iter().map(|BaseEntity { pos, ty, rotation }| {
            BlueprintAction::PlaceEntity(BlueprintPlaceEntity::Belt {
                pos,
                ty: data_strings[ty].clone(),
                direction: rotation,
                copied_belt_info: None,
            })
        }));

        let actions = actions.chain(underground_belts.into_iter().map(
            |(BaseEntity { pos, ty, rotation }, underground_dir)| {
                BlueprintAction::PlaceEntity(BlueprintPlaceEntity::Underground {
                    pos,
                    ty: data_strings[ty].clone(),
                    direction: rotation,
                    underground_dir,
                    copied_belt_info: None,
                })
            },
        ));

        let actions = actions.chain(power_poles.into_iter().map(
            |BaseEntity { pos, ty, rotation }| {
                BlueprintAction::PlaceEntity(BlueprintPlaceEntity::PowerPole {
                    pos,
                    ty: data_strings[ty].clone(),
                })
            },
        ));

        let actions = actions.chain(labs.into_iter().map(
            |BaseEntity {
                 pos,
                 ty,
                 rotation: _,
             }| {
                BlueprintAction::PlaceEntity(BlueprintPlaceEntity::Lab {
                    pos,
                    ty: data_strings[ty].clone(),
                })
            },
        ));

        let actions = actions.chain(fluid_tanks.into_iter().map(
            |BaseEntity { pos, ty, rotation }| {
                BlueprintAction::PlaceEntity(BlueprintPlaceEntity::FluidTank {
                    pos,
                    ty: data_strings[ty].clone(),
                    rotation,
                })
            },
        ));

        let actions = actions.chain(mining_drills.into_iter().map(
            |BaseEntity { pos, ty, rotation }| {
                BlueprintAction::PlaceEntity(BlueprintPlaceEntity::MiningDrill {
                    pos,
                    ty: data_strings[ty].clone(),
                    rotation,
                })
            },
        ));

        let actions = actions.chain(splitters.into_iter().map(
            |(BaseEntity { pos, ty, rotation }, in_mode, out_mode)| {
                BlueprintAction::PlaceEntity(BlueprintPlaceEntity::Splitter {
                    pos,
                    ty: data_strings[ty].clone(),
                    direction: rotation,
                    in_mode,
                    out_mode,
                })
            },
        ));

        let actions = actions.chain(inserters.into_iter().map(
            |(BaseEntity { pos, ty, rotation }, filter, movetime)| {
                BlueprintAction::PlaceEntity(BlueprintPlaceEntity::Inserter {
                    pos,
                    ty: data_strings[ty].clone(),
                    dir: rotation,
                    filter: filter.map(|idx| data_strings[idx].clone()),

                    movetime,
                })
            },
        ));

        let actions =
            actions.chain(
                set_recipe
                    .into_iter()
                    .map(|(pos, recipe)| BlueprintAction::SetRecipe {
                        pos,
                        recipe: data_strings[recipe].clone(),
                    }),
            );

        let actions = actions.chain(movetime.into_iter().map(|(pos, new_movetime)| {
            BlueprintAction::OverrideInserterMovetime { pos, new_movetime }
        }));

        let actions = actions.chain(
            slot_limit
                .into_iter()
                .map(|(pos, num_slots)| BlueprintAction::SetChestSlotLimit { pos, num_slots }),
        );

        let actions = actions.chain(modules.into_iter().map(|(pos, modules)| {
            BlueprintAction::AddModules {
                pos,
                modules: module_combinations[modules]
                    .iter()
                    .map(|&idx| data_strings[idx].clone())
                    .collect(),
            }
        }));

        let actions =
            actions.chain(
                ores.into_iter()
                    .map(|(pos, ore, amount)| BlueprintAction::PlaceOre {
                        pos,
                        ore: data_strings[ore].clone(),
                        amount,
                    }),
            );

        Ok(Blueprint {
            actions: actions.collect(),
        })
    }
}
