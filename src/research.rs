#[cfg(feature = "client")]
use egui::{Button, Color32, CornerRadius, ProgressBar, SidePanel, Ui};
#[cfg(feature = "client")]
use egui_graphs::{
    DefaultEdgeShape, DefaultNodeShape, Graph, GraphView, LayoutStateTree, LayoutTree,
    SettingsInteraction, SettingsNavigation, SettingsStyle,
};
#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size::GetSize;

use petgraph::Directed;
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;

use std::cmp::min;
use std::collections::{HashMap, HashSet};
use std::u16;

use crate::data;
use crate::data::DataStore;
use crate::item::Indexable;

use crate::IdxTrait;
use crate::frontend::action::ActionType;
use crate::item::Recipe;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, Default, serde::Deserialize, serde::Serialize,
)]
pub struct Technology {
    pub id: u16, //65k Technologies should suffice :)
}
#[derive(Debug)]
pub struct LabTickInfo {
    pub times_labs_used_science: u64,
    pub tech: Technology,
}

pub type ResearchProgress = u16;

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct TechState {
    pub current_technology: Option<Technology>,
    // Map from technologies to how many times they were completed
    pub finished_technologies: HashMap<Technology, u32>,
    pub in_progress_technologies: HashMap<Technology, u64>,
    // current_tech_mod_lookup: (),
    pub recipe_active: Vec<bool>,

    science_overflow_buffer: Box<[u32]>,
}

impl TechState {
    pub fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        let finished_technologies = HashMap::from_iter(
            data_store
                .instantly_finished_technologies
                .iter()
                .map(|index| {
                    (
                        Technology {
                            id: index.index().try_into().unwrap(),
                        },
                        1,
                    )
                }),
        );

        let recipe_active = (0..data_store.recipe_is_intermediate.len())
            .map(|recipe_id| recipe_id.try_into().unwrap())
            .map(|recipe_id| {
                finished_technologies.iter().any(|(tech, _)| {
                    data_store
                        .technology_tree
                        .node_weight(NodeIndex::from(tech.id))
                        .unwrap()
                        .effect
                        .unlocked_recipes
                        .contains(&Recipe { id: recipe_id })
                })
            })
            .collect();

        Self {
            current_technology: None,
            finished_technologies,
            recipe_active,

            in_progress_technologies: HashMap::new(),

            science_overflow_buffer: vec![0; data_store.science_bottle_items.len()]
                .into_boxed_slice(),
        }
    }

    pub fn get_active_recipes(&self) -> &[bool] {
        &self.recipe_active
    }

    #[profiling::function]
    pub fn apply_progress<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        mut tech_progress: u16,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        if tech_progress == 0 {
            return;
        }

        if let Some(current) = &self.current_technology {
            let (tech_cost_units, tech_cost_items) =
                &data_store.technology_costs[current.id as usize];
            let mut tech_cost_units = *tech_cost_units;

            let is_repeating = if let Some(infinite) = &data_store
                .technology_tree
                .node_weight(NodeIndex::new(current.id as usize))
                .unwrap()
                .repeatable
            {
                let times_this_tech_was_finished = self
                    .finished_technologies
                    .get(current)
                    .copied()
                    .unwrap_or(0);

                let tech_cost_increase = match infinite.scaling {
                    data::RepeatableCostScaling::Linear {
                        unit_increase_per_level,
                    } => unit_increase_per_level * u64::from(times_this_tech_was_finished),
                    data::RepeatableCostScaling::Exponential {
                        unit_multiplier_per_level_nom,
                        unit_multiplier_per_level_denom,
                    } => todo!(),
                };

                tech_cost_units += tech_cost_increase;

                true
            } else {
                false
            };

            // Apply science from overflow if possible
            let mut tech_progress_from_overflow = u16::MAX;
            for (overflow_buffer, unit_cost) in self
                .science_overflow_buffer
                .iter_mut()
                .zip(tech_cost_items.iter())
            {
                if *unit_cost > 0 {
                    tech_progress_from_overflow = min(
                        tech_progress_from_overflow,
                        u16::try_from(*overflow_buffer / u32::from(*unit_cost)).unwrap(),
                    );
                }
            }

            if tech_progress_from_overflow > 0 {
                dbg!(tech_progress_from_overflow);
            }

            tech_progress_from_overflow =
                tech_progress_from_overflow.clamp(0, u16::MAX - tech_progress);

            for (overflow_buffer, unit_cost) in self
                .science_overflow_buffer
                .iter_mut()
                .zip(tech_cost_items.iter())
            {
                *overflow_buffer = overflow_buffer
                    .checked_sub(u32::from(*unit_cost) * u32::from(tech_progress_from_overflow))
                    .expect("Overflow buffer underflowed?!?");
            }

            tech_progress += tech_progress_from_overflow;

            // Apply this ticks progress
            let progress = self.in_progress_technologies.entry(*current).or_insert(0);

            *progress += u64::from(tech_progress);

            if *progress >= tech_cost_units {
                // We finished this technology!
                let final_science_used = self.in_progress_technologies.remove(&current).unwrap();
                // We have now finished this technology one more time
                *self.finished_technologies.entry(*current).or_default() += 1;
                for recipe in &data_store
                    .technology_tree
                    .node_weight(NodeIndex::from(current.id))
                    .unwrap()
                    .effect
                    .unlocked_recipes
                {
                    self.recipe_active[recipe.into_usize()] = true;
                }
                if is_repeating {
                    // Just keep researching the same tech (just one level higher)
                } else {
                    self.current_technology = None;
                }

                // Since we only check if a tech is finished at the end of each update, it is possible we produced more science progress in this tick, than was required.
                // To not lose this science (which would mean players sometimes are unable to finish a technology even if they supplied enough science packs),
                // We just store the overflow science and apply it if possible
                let overshoot = final_science_used - tech_cost_units;

                for (overflow_buffer, unit_cost) in self
                    .science_overflow_buffer
                    .iter_mut()
                    .zip(tech_cost_items.iter())
                {
                    *overflow_buffer = (*overflow_buffer).checked_add(u32::try_from(overshoot * u64::from(*unit_cost))
                        .expect("impossible since the most science unit produced this tick are u16::MAX (see funciton args)"))
                            .expect("Science Overflow buffer overflowed (Ironic).");
                }
            }
        } else {
            assert_eq!(
                tech_progress, 0,
                "Labs should not be working without a tech set!"
            );
        }
    }

    #[cfg(feature = "client")]
    pub fn generate_render_graph<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &self,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Graph<data::Technology<RecipeIdxType>, (), Directed, u16, DefaultNodeShape, DefaultEdgeShape>
    {
        egui_graphs::to_graph_custom::<_, _, _, _, DefaultNodeShape, DefaultEdgeShape>(
            &data_store.technology_tree,
            |node| {
                let is_repeatable = node.payload().repeatable.clone();
                let is_done = if is_repeatable.is_some() {
                    false
                } else {
                    self.finished_technologies
                        .get(&Technology {
                            id: node.id().index().try_into().unwrap(),
                        })
                        .copied()
                        .unwrap_or(0)
                        > 0
                };

                if is_done {
                    node.set_color(Color32::GREEN);
                } else if data_store
                    .technology_tree
                    .edges_directed(node.id(), petgraph::Direction::Incoming)
                    .all(|edge| {
                        self.finished_technologies
                            .get(&Technology {
                                id: edge.source().index().try_into().unwrap(),
                            })
                            .copied()
                            .unwrap_or(0)
                            > 0
                    })
                {
                    node.set_color(Color32::YELLOW);
                } else {
                    node.set_color(Color32::RED);
                }

                if let Some(repeat) = is_repeatable {
                    let times_completed = self
                        .finished_technologies
                        .get(&Technology {
                            id: node.id().index().try_into().unwrap(),
                        })
                        .copied()
                        .unwrap_or(0);

                    node.set_label(format!(
                        "{} {}",
                        &data_store
                            .technology_tree
                            .node_weight(NodeIndex::from(u16::try_from(node.id().index()).unwrap()))
                            .unwrap()
                            .name,
                        times_completed + repeat.level_counter_offset
                    ));
                } else {
                    node.set_label(
                        data_store
                            .technology_tree
                            .node_weight(NodeIndex::from(u16::try_from(node.id().index()).unwrap()))
                            .unwrap()
                            .name
                            .clone(),
                    );
                }
            },
            |_edge| {},
        )
    }

    #[cfg(feature = "client")]
    pub fn render_tech_window<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &self,
        ui: &mut Ui,
        render_graph: &mut Graph<
            data::Technology<RecipeIdxType>,
            (),
            Directed,
            u16,
            DefaultNodeShape,
            DefaultEdgeShape,
        >,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>> + use<ItemIdxType, RecipeIdxType>
    {
        {
            profiling::scope!("Update Tech Tree colors");
            for tech in 0..data_store.technology_costs.len() {
                let node = render_graph.node_mut(NodeIndex::new(tech)).unwrap();

                let is_repeating = node.payload().repeatable.clone();

                let is_done = if is_repeating.is_some() {
                    false
                } else {
                    self.finished_technologies
                        .get(&Technology {
                            id: node.id().index().try_into().unwrap(),
                        })
                        .copied()
                        .unwrap_or(0)
                        > 0
                };

                if let Some(repeat) = is_repeating {
                    let times_completed = self
                        .finished_technologies
                        .get(&Technology {
                            id: node.id().index().try_into().unwrap(),
                        })
                        .copied()
                        .unwrap_or(0);

                    node.set_label(format!(
                        "{} {}",
                        &data_store
                            .technology_tree
                            .node_weight(NodeIndex::from(u16::try_from(node.id().index()).unwrap()))
                            .unwrap()
                            .name,
                        times_completed + repeat.level_counter_offset
                    ));
                }

                if is_done {
                    node.set_color(Color32::GREEN);
                } else if data_store
                    .technology_tree
                    .edges_directed(node.id(), petgraph::Direction::Incoming)
                    .all(|edge| {
                        self.finished_technologies
                            .get(&Technology {
                                id: edge.source().index().try_into().unwrap(),
                            })
                            .copied()
                            .unwrap_or(0)
                            > 0
                    })
                {
                    node.set_color(Color32::YELLOW);
                } else {
                    node.set_color(Color32::RED);
                }
            }
        }

        let mut ret = vec![];

        SidePanel::new(egui::panel::Side::Left, "Current Technology Info Sidepanel").show_inside(
            ui,
            |ui| {
                if let Some(tech) = &self.current_technology {
                    ui.label(
                        &data_store
                            .technology_tree
                            .node_weight(NodeIndex::from(tech.id))
                            .unwrap()
                            .name,
                    );

                    let done = self
                        .in_progress_technologies
                        .get(tech)
                        .copied()
                        .unwrap_or(0);
                    let (tech_cost_units, tech_cost_items) =
                        &data_store.technology_costs[tech.id as usize];
                    let mut tech_cost_units = *tech_cost_units;

                    if let Some(infinite) = &data_store
                        .technology_tree
                        .node_weight(NodeIndex::new(tech.id as usize))
                        .unwrap()
                        .repeatable
                    {
                        let times_this_tech_was_finished =
                            self.finished_technologies.get(tech).copied().unwrap_or(0);

                        let tech_cost_increase = match infinite.scaling {
                            data::RepeatableCostScaling::Linear {
                                unit_increase_per_level,
                            } => unit_increase_per_level * u64::from(times_this_tech_was_finished),
                            data::RepeatableCostScaling::Exponential {
                                unit_multiplier_per_level_nom,
                                unit_multiplier_per_level_denom,
                            } => todo!(),
                        };

                        tech_cost_units += tech_cost_increase;
                    }
                    ui.add(
                        ProgressBar::new((done as f64 / tech_cost_units as f64) as f32)
                            .corner_radius(CornerRadius::ZERO)
                            .text(format!("{}/{}", done, tech_cost_units)),
                    );

                    if ui.button("Cancel").clicked() {
                        ret.push(ActionType::SetActiveResearch { tech: None });
                    } else if ui.button("[CHEAT] Unlock Technology").clicked() {
                        ret.push(ActionType::CheatUnlockTechnology { tech: *tech });
                    }
                }
            },
        );

        SidePanel::new(egui::panel::Side::Right, "Technology Info Sidepanel").show_inside(
            ui,
            |ui| {
                if ui.button("[CHEAT] Unlock all techs").clicked() {
                    for (tech, _tech_info) in render_graph.nodes_iter() {
                        ret.push(ActionType::CheatUnlockTechnology {
                            tech: Technology {
                                id: tech.index().try_into().unwrap(),
                            },
                        });
                    }
                }

                if !render_graph.selected_nodes().is_empty() {
                    let [selected_node] = render_graph.selected_nodes() else {
                        unreachable!("We only allow selecting a single node!");
                    };

                    ui.label(
                        &data_store
                            .technology_tree
                            .node_weight(NodeIndex::from(
                                u16::try_from(selected_node.index()).unwrap(),
                            ))
                            .unwrap()
                            .name,
                    );

                    let is_done = if data_store
                        .technology_tree
                        .node_weight(*selected_node)
                        .unwrap()
                        .repeatable
                        .is_some()
                    {
                        false
                    } else {
                        self.finished_technologies
                            .get(&Technology {
                                id: selected_node.index().try_into().unwrap(),
                            })
                            .copied()
                            .unwrap_or(0)
                            > 0
                    };

                    let possible_to_research = data_store
                        .technology_tree
                        .edges_directed(*selected_node, petgraph::Direction::Incoming)
                        .all(|edge| {
                            self.finished_technologies
                                .get(&Technology {
                                    id: edge.source().index().try_into().unwrap(),
                                })
                                .copied()
                                .unwrap_or(0)
                                > 0
                        });

                    let is_currently_researching = Some(Technology {
                        id: selected_node.index().try_into().unwrap(),
                    }) == self.current_technology;

                    if ui
                        .add_enabled(
                            !is_done && !is_currently_researching && possible_to_research,
                            Button::new("Research"),
                        )
                        .clicked()
                    {
                        ret.push(ActionType::SetActiveResearch {
                            tech: Some(Technology {
                                id: selected_node.index().try_into().unwrap(),
                            }),
                        });
                    }

                    if is_done {
                        if ui.button("[CHEAT] Undo Technology").clicked() {
                            ret.push(ActionType::CheatRelockTechnology {
                                tech: Technology {
                                    id: selected_node.index().try_into().unwrap(),
                                },
                            });
                        }
                    }
                }
            },
        );

        let mut view =
            GraphView::<_, _, _, _, _, _, LayoutStateTree, LayoutTree>::new(render_graph)
                .with_navigations(
                    &SettingsNavigation::new()
                        .with_fit_to_screen_enabled(false)
                        .with_zoom_and_pan_enabled(true),
                )
                .with_interactions(&SettingsInteraction::new().with_node_selection_enabled(true))
                .with_styles(&SettingsStyle::new().with_labels_always(true));

        ui.add(&mut view);

        ret.into_iter()
    }
}
