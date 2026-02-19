#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;

use std::{iter, ops::RangeInclusive, sync::LazyLock};

#[cfg(feature = "client")]
use crate::progress_info::ProgressInfo;
use crate::{
    app_state::GameState,
    data::DataStore,
    frontend::{action::ActionType, world::Position},
    power::Watt,
    replays::GenerationInformation,
    research::Technology,
};

pub(crate) struct WorldValueStore {
    name_field: String,
    worlds: Vec<Vec<ValueValue>>,
}

impl Default for WorldValueStore {
    fn default() -> Self {
        let v = WORLDS
            .iter()
            .map(|world| {
                world
                    .values
                    .iter()
                    .map(|value| value.default.clone())
                    .collect()
            })
            .collect();

        WorldValueStore {
            name_field: "New World".to_string(),
            worlds: v,
        }
    }
}

#[cfg(feature = "client")]
pub(crate) fn list_example_worlds(
    values: &mut WorldValueStore,
    ui: &mut egui::Ui,
) -> Option<impl FnOnce(ProgressInfo, &DataStore<u8, u8>) -> GameState<u8, u8> + 'static> {
    ui.horizontal(|ui| {
        ui.label("World Name:");
        ui.text_edit_singleline(&mut values.name_field);
    });

    for (idx, (world, world_values)) in WORLDS.iter().zip(values.worlds.iter_mut()).enumerate() {
        let v = ui.horizontal(|ui| {
            ui.label(world.name);
            ui.label(world.description);

            for (desc, value) in world.values.iter().zip(world_values.iter_mut()) {
                ui.vertical(|ui| {
                    ui.label(desc.name);
                    match &desc.kind {
                        ValueKind::Range { allowed, log } => {
                            let ValueValue::Range(value) = value else {
                                unreachable!();
                            };

                            ui.add(
                                egui::Slider::new(value, allowed.clone())
                                    .logarithmic(*log)
                                    .text(desc.name),
                            );
                        },
                        ValueKind::Toggle {} => {
                            let ValueValue::Toggle(value) = value else {
                                unreachable!();
                            };

                            ui.checkbox(value, ());
                        },
                    }
                });
            }

            let allowed = if cfg!(target_arch = "wasm32") {
                (world.allowed_on_wasm)(world_values) == AllowedOnWasm::True
            } else {
                true
            };

            let disabled_str = if cfg!(target_arch = "wasm32") {
                match (world.allowed_on_wasm)(world_values) {
                    AllowedOnWasm::True => "",
                    AllowedOnWasm::False(s) => s.unwrap_or("Not available on WASM"),
                }
            } else {
                ""
            };

            if ui
                .add_enabled(allowed, egui::Button::new("Create"))
                .on_disabled_hover_text(disabled_str)
                .clicked()
            {
                let world_values = world_values.clone();
                let name = values.name_field.clone();
                let fun = world.creation_fn;
                Some(move |progress, data_store: &'_ DataStore<u8, u8>| {
                    (fun)(
                        name,
                        progress,
                        GenerationInformation {
                            example_idx: idx,
                            example_settings: world_values,
                        },
                        data_store,
                    )
                })
            } else {
                None
            }
        });

        if let Some(v) = v.inner {
            return Some(v);
        }

        ui.separator();
    }

    None
}

pub(crate) fn get_builder(
    name: String,
    idx: usize,
    values: Vec<ValueValue>,
) -> impl FnOnce(ProgressInfo, &DataStore<u8, u8>) -> GameState<u8, u8> + 'static {
    let fun = WORLDS
        .get(idx)
        .expect("Example World index out of bounds")
        .creation_fn;
    move |progress, data_store: &'_ DataStore<u8, u8>| {
        (fun)(
            name,
            progress,
            GenerationInformation {
                example_idx: idx,
                example_settings: values,
            },
            data_store,
        )
    }
}

struct ExampleWorld {
    name: &'static str,
    description: &'static str,
    values: Vec<WorldValue>,

    // TODO: I might want to change this to depend on the values
    allowed_on_wasm: fn(&[ValueValue]) -> AllowedOnWasm,
    creation_fn:
        fn(String, ProgressInfo, GenerationInformation, &DataStore<u8, u8>) -> GameState<u8, u8>,
}

#[derive(Debug, PartialEq)]
enum AllowedOnWasm {
    True,
    False(Option<&'static str>),
}

struct WorldValue {
    name: &'static str,
    kind: ValueKind,
    default: ValueValue,
}

enum ValueKind {
    Range {
        allowed: RangeInclusive<usize>,
        log: bool,
    },
    Toggle {},
}

// FIXME: Naming???
#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) enum ValueValue {
    Range(usize),
    Toggle(bool),
}

const WORLDS: LazyLock<[ExampleWorld; 5]> = LazyLock::new(|| {
    [
        ExampleWorld {
            name: "Empty World",
            description: "Completely Empty",
            values: vec![],

            allowed_on_wasm: |_| AllowedOnWasm::True,

            creation_fn: |name, _, gen_info, data_store| GameState::new(name, gen_info, data_store),
        },
        ExampleWorld {
            name: "Megabase",
            description: "A world consisting of a single 40k SPM Megabase designed by Smurphy",
            values: vec![WorldValue {
                name: "Generate Solar Field",
                kind: ValueKind::Toggle {},
                default: ValueValue::Toggle(false),
            }],

            allowed_on_wasm: |_| AllowedOnWasm::True,

            creation_fn: |name, progress, gen_info, data_store| {
                let &[ValueValue::Toggle(use_solar_field)] = &gen_info.example_settings[..] else {
                    unreachable!();
                };

                let gs = GameState::new_with_megabase(
                    name,
                    gen_info,
                    use_solar_field,
                    progress,
                    data_store,
                );

                let techs = 0..data_store.technology_costs.len();

                let cheat_unlocks = techs.map(|id| ActionType::CheatUnlockTechnology {
                    tech: crate::research::Technology {
                        id: id.try_into().unwrap(),
                    },
                });

                let mining_prod_id = data_store
                    .technology_tree
                    .node_indices()
                    .find_map(|node| {
                        let tech = data_store.technology_tree.node_weight(node).unwrap();

                        (tech.name == "Mining Productivity").then_some(node.index())
                    })
                    .unwrap();

                GameState::apply_actions(
                    &mut *gs.simulation_state.lock(),
                    &mut *gs.world.lock(),
                    cheat_unlocks.chain(iter::once(ActionType::AddResearchToQueue {
                        tech: Technology {
                            id: mining_prod_id.try_into().unwrap(),
                        },
                    })),
                    data_store,
                );

                gs
            },
        },
        ExampleWorld {
            name: "Gigabase",
            description: "A world consisting of multiple copies of a 40k SPM Megabase",
            values: vec![WorldValue {
                name: "Megabase Count",
                kind: ValueKind::Range {
                    allowed: 0..=1000,
                    log: true,
                },
                default: ValueValue::Range(40),
            }],

            allowed_on_wasm: |_| {
                AllowedOnWasm::False(Some(
                    "WASM does not support enough memory to run a gigabase, consider switching to native",
                ))
            },

            creation_fn: |name, progress, gen_info, data_store| {
                let &[ValueValue::Range(count)] = &gen_info.example_settings[..] else {
                    unreachable!();
                };

                let gs = GameState::new_with_gigabase(
                    name,
                    gen_info,
                    count.try_into().unwrap(),
                    progress,
                    data_store,
                );

                let techs = 0..data_store.technology_costs.len();

                let cheat_unlocks = techs.map(|id| ActionType::CheatUnlockTechnology {
                    tech: crate::research::Technology {
                        id: id.try_into().unwrap(),
                    },
                });

                let mining_prod_id = data_store
                    .technology_tree
                    .node_indices()
                    .find_map(|node| {
                        let tech = data_store.technology_tree.node_weight(node).unwrap();

                        (tech.name == "Mining Productivity").then_some(node.index())
                    })
                    .unwrap();

                GameState::apply_actions(
                    &mut *gs.simulation_state.lock(),
                    &mut *gs.world.lock(),
                    cheat_unlocks.chain(iter::once(ActionType::AddResearchToQueue {
                        tech: Technology {
                            id: mining_prod_id.try_into().unwrap(),
                        },
                    })),
                    data_store,
                );

                gs
            },
        },
        ExampleWorld {
            name: "Trip around the world",
            description: "A small ring around the world",
            values: vec![],
            allowed_on_wasm: |_| AllowedOnWasm::True,
            creation_fn: |name, progress, gen_info, data_store| {
                GameState::new_with_world_train_ride(name, gen_info, progress, data_store)
            },
        },
        ExampleWorld {
            name: "Solar Field",
            description: "A world containing a Solar Field",
            values: vec![WorldValue {
                name: "Panel Count",
                kind: ValueKind::Range {
                    allowed: 1..=1_000_000_000,
                    log: true,
                },
                default: ValueValue::Range(1_000_000),
            }],
            allowed_on_wasm: |values| {
                let [ValueValue::Range(count)] = values else {
                    unreachable!();
                };

                // Testing said ~100 bytes per solar panel
                let expected_size = (*count) as u64 * 100 + 1_000_000_000;

                // Wasm only has 4GB of RAM so limit to ~3GB
                if expected_size > 3_000_000_000 {
                    AllowedOnWasm::False(Some(
                        "Generated World Size would exceed maximum memory on WASM, consider reducing panel count",
                    ))
                } else {
                    AllowedOnWasm::True
                }
            },
            creation_fn: |name, progress, gen_info, data_store| {
                let &[ValueValue::Range(count)] = &gen_info.example_settings[..] else {
                    unreachable!();
                };

                GameState::new_with_tons_of_solar(
                    name,
                    gen_info,
                    Watt(42_000) * count as u64,
                    Position { x: 1_600, y: 1_600 },
                    None,
                    progress,
                    data_store,
                )
            },
        },
    ]
});
