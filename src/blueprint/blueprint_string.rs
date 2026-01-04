use base64::engine::general_purpose::STANDARD;
use flate2::Compression;
use log::error;

use super::Blueprint;
use super::BlueprintAction;
use super::BlueprintPlaceEntity;
use super::Dir;
use super::UndergroundDir;
use crate::Position;
use crate::belt::splitter::SplitterDistributionMode;
use std::io::BufReader;
use std::num::NonZero;
use std::sync::Arc;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct BlueprintString(pub String);

type EntityKind = usize;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct BaseEntity {
    pos: Position,
    ty: EntityKind,
    rotation: Dir,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct BlueprintStringInternal {
    data_strings: Vec<Arc<str>>,

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

impl TryFrom<BlueprintString> for Blueprint {
    type Error = ();
    fn try_from(value: BlueprintString) -> Result<Self, Self::Error> {
        let raw_str = value.0;

        let reader = base64::read::DecoderReader::new(raw_str.as_bytes(), &STANDARD);
        let dec = BufReader::new(flate2::read::DeflateDecoder::new(reader));

        let Ok(internal) = bincode::serde::decode_from_reader(dec, bincode::config::standard())
        else {
            error!("Blueprint failed to deserialize!");
            return Err(());
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

        let actions = actions.chain(beacons.into_iter().map(|BaseEntity { pos, ty, rotation }| {
            BlueprintAction::PlaceEntity(BlueprintPlaceEntity::Beacon {
                pos,
                ty: data_strings[ty].clone(),
            })
        }));

        let actions = actions.chain(chests.into_iter().map(|BaseEntity { pos, ty, rotation }| {
            BlueprintAction::PlaceEntity(BlueprintPlaceEntity::Chest {
                pos,
                ty: data_strings[ty].clone(),
            })
        }));

        let actions = actions.chain(solar_panels.into_iter().map(
            |BaseEntity { pos, ty, rotation }| {
                BlueprintAction::PlaceEntity(BlueprintPlaceEntity::SolarPanel {
                    pos,
                    ty: data_strings[ty].clone(),
                })
            },
        ));

        let actions = actions.chain(accumulators.into_iter().map(
            |BaseEntity { pos, ty, rotation }| {
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

        let actions = actions.chain(labs.into_iter().map(|BaseEntity { pos, ty, rotation }| {
            BlueprintAction::PlaceEntity(BlueprintPlaceEntity::Lab {
                pos,
                ty: data_strings[ty].clone(),
            })
        }));

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

impl From<Blueprint> for BlueprintString {
    fn from(value: Blueprint) -> Self {
        let mut internal = BlueprintStringInternal {
            data_strings: vec![],
            solar_panels: vec![],
            accumulators: vec![],
            belts: vec![],
            underground_belts: vec![],
            power_poles: vec![],
            beacons: vec![],
            chests: vec![],
            fluid_tanks: vec![],
            labs: vec![],
            mining_drills: vec![],
            assemblers: vec![],

            splitters: vec![],
            inserters: vec![],

            module_combinations: vec![],
            modules: vec![],

            movetime: vec![],
            set_recipe: vec![],
            slot_limit: vec![],

            ores: vec![],
        };

        for action in value.actions {
            match action {
                super::BlueprintAction::PlaceEntity(blueprint_place_entity) => {
                    match blueprint_place_entity {
                        super::BlueprintPlaceEntity::Assembler { pos, ty, rotation } => {
                            internal.assemblers.push(BaseEntity {
                                pos,
                                ty: internal.data_strings.get_index_or_insert(ty.into()),
                                rotation,
                            });
                        },
                        super::BlueprintPlaceEntity::Inserter {
                            pos,
                            dir,
                            filter,
                            ty,
                            movetime,
                        } => {
                            internal.inserters.push((
                                BaseEntity {
                                    pos,
                                    ty: internal.data_strings.get_index_or_insert(ty.into()),
                                    rotation: dir,
                                },
                                filter.map(|item| {
                                    internal.data_strings.get_index_or_insert(item.into())
                                }),
                                movetime,
                            ));
                        },
                        super::BlueprintPlaceEntity::Belt {
                            pos,
                            direction,
                            ty,
                            copied_belt_info: _,
                        } => {
                            internal.belts.push(BaseEntity {
                                pos,
                                ty: internal.data_strings.get_index_or_insert(ty.into()),
                                rotation: direction,
                            });
                        },
                        super::BlueprintPlaceEntity::Underground {
                            pos,
                            direction,
                            ty,
                            underground_dir,
                            copied_belt_info: _,
                        } => {
                            internal.underground_belts.push((
                                BaseEntity {
                                    pos,
                                    ty: internal.data_strings.get_index_or_insert(ty.into()),
                                    rotation: direction,
                                },
                                underground_dir,
                            ));
                        },
                        super::BlueprintPlaceEntity::PowerPole { pos, ty } => {
                            internal.power_poles.push(BaseEntity {
                                pos,
                                ty: internal.data_strings.get_index_or_insert(ty.into()),
                                rotation: Dir::North,
                            });
                        },
                        super::BlueprintPlaceEntity::Splitter {
                            pos,
                            direction,
                            ty,
                            in_mode,
                            out_mode,
                        } => {
                            internal.splitters.push((
                                BaseEntity {
                                    pos,
                                    ty: internal.data_strings.get_index_or_insert(ty.into()),
                                    rotation: direction,
                                },
                                in_mode,
                                out_mode,
                            ));
                        },
                        super::BlueprintPlaceEntity::Chest { pos, ty } => {
                            internal.chests.push(BaseEntity {
                                pos,
                                ty: internal.data_strings.get_index_or_insert(ty.into()),
                                rotation: Dir::North,
                            })
                        },
                        super::BlueprintPlaceEntity::SolarPanel { pos, ty } => {
                            internal.solar_panels.push(BaseEntity {
                                pos,
                                ty: internal.data_strings.get_index_or_insert(ty.into()),
                                rotation: Dir::North,
                            })
                        },
                        super::BlueprintPlaceEntity::Accumulator { pos, ty } => {
                            internal.accumulators.push(BaseEntity {
                                pos,
                                ty: internal.data_strings.get_index_or_insert(ty.into()),
                                rotation: Dir::North,
                            })
                        },
                        super::BlueprintPlaceEntity::Lab { pos, ty } => {
                            internal.labs.push(BaseEntity {
                                pos,
                                ty: internal.data_strings.get_index_or_insert(ty.into()),
                                rotation: Dir::North,
                            })
                        },
                        super::BlueprintPlaceEntity::Beacon { ty, pos } => {
                            internal.beacons.push(BaseEntity {
                                pos,
                                ty: internal.data_strings.get_index_or_insert(ty.into()),
                                rotation: Dir::North,
                            })
                        },
                        super::BlueprintPlaceEntity::FluidTank { ty, pos, rotation } => {
                            internal.fluid_tanks.push(BaseEntity {
                                pos,
                                ty: internal.data_strings.get_index_or_insert(ty.into()),
                                rotation,
                            })
                        },
                        super::BlueprintPlaceEntity::MiningDrill { ty, pos, rotation } => {
                            internal.mining_drills.push(BaseEntity {
                                pos,
                                ty: internal.data_strings.get_index_or_insert(ty.into()),
                                rotation,
                            })
                        },
                    }
                },
                super::BlueprintAction::SetRecipe { pos, recipe } => {
                    internal.set_recipe.push((
                        pos,
                        internal.data_strings.get_index_or_insert(recipe.into()),
                    ));
                },
                super::BlueprintAction::OverrideInserterMovetime { pos, new_movetime } => {
                    internal.movetime.push((pos, new_movetime));
                },
                super::BlueprintAction::AddModules { pos, modules } => {
                    internal.modules.push((
                        pos,
                        internal.module_combinations.get_index_or_insert(
                            modules
                                .into_iter()
                                .map(|module| {
                                    internal.data_strings.get_index_or_insert(module.into())
                                })
                                .collect(),
                        ),
                    ));
                },
                super::BlueprintAction::SetChestSlotLimit { pos, num_slots } => {
                    internal.slot_limit.push((pos, num_slots));
                },
                super::BlueprintAction::PlaceOre { pos, ore, amount } => {
                    internal.ores.push((
                        pos,
                        internal.data_strings.get_index_or_insert(ore.into()),
                        amount,
                    ));
                },
            }
        }

        let mut base64_writer = base64::write::EncoderStringWriter::new(&STANDARD);
        let mut enc = flate2::write::DeflateEncoder::new(&mut base64_writer, Compression::best());
        bincode::serde::encode_into_std_write(internal, &mut enc, bincode::config::standard())
            .expect("Failed to write blueprint string");
        enc.finish().expect("Deflate failed");
        let ret = base64_writer.into_inner();

        BlueprintString(ret)
    }
}

trait VecExt<T> {
    fn get_index_or_insert(&mut self, value: T) -> usize;
}

impl<T: PartialEq> VecExt<T> for Vec<T> {
    fn get_index_or_insert(&mut self, value: T) -> usize {
        if let Some(idx) = self.iter().position(|v| *v == value) {
            return idx;
        } else {
            self.push(value);
            self.len() - 1
        }
    }
}
