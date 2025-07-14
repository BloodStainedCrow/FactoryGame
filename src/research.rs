use egui::{Button, CornerRadius, ProgressBar, SidePanel};
use egui::{Color32, Ui};
use egui_graphs::DefaultNodeShape;
use egui_graphs::Graph;
use egui_graphs::GraphView;
use egui_graphs::LayoutHierarchical;
use egui_graphs::LayoutStateHierarchical;
use egui_graphs::SettingsInteraction;
use egui_graphs::SettingsNavigation;
use egui_graphs::{DefaultEdgeShape, SettingsStyle};
use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use petgraph::Directed;

use std::cmp::min;
use std::collections::{HashMap, HashSet};
use std::u16;

use crate::data;
use crate::data::DataStore;
use crate::item::Indexable;

use crate::frontend::action::ActionType;
use crate::item::Recipe;
use crate::IdxTrait;

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

#[derive(Debug, Clone, Default, serde::Deserialize, serde::Serialize)]
pub struct TechState {
    pub current_technology: Option<Technology>,
    pub finished_technologies: HashSet<Technology>,
    pub in_progress_technologies: HashMap<Technology, u64>,
    // current_tech_mod_lookup: (),
    pub recipe_active: Vec<bool>,

    science_overflow_buffer: Box<[u32]>,
}

impl TechState {
    pub fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        let finished_technologies = HashSet::from_iter(
            data_store
                .instantly_finished_technologies
                .iter()
                .map(|index| Technology {
                    id: index.index().try_into().unwrap(),
                }),
        );

        let recipe_active = (0..data_store.recipe_is_intermediate.len())
            .map(|recipe_id| recipe_id.try_into().unwrap())
            .map(|recipe_id| {
                finished_technologies.iter().any(|tech| {
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
            let tech_cost = &data_store.technology_costs[current.id as usize];

            // Apply science from overflow if possible
            let mut tech_progress_from_overflow = u16::MAX;
            for (overflow_buffer, unit_cost) in self
                .science_overflow_buffer
                .iter_mut()
                .zip(tech_cost.1.iter())
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
                .zip(tech_cost.1.iter())
            {
                *overflow_buffer = overflow_buffer
                    .checked_sub(u32::from(*unit_cost) * u32::from(tech_progress_from_overflow))
                    .expect("Overflow buffer underflowed?!?");
            }

            tech_progress += tech_progress_from_overflow;

            // Apply this ticks progress
            let progress = self.in_progress_technologies.entry(*current).or_insert(0);

            *progress += u64::from(tech_progress);

            if *progress >= tech_cost.0 {
                // We finished this technology!
                let final_science_used = self.in_progress_technologies.remove(&current).unwrap();
                self.finished_technologies.insert(*current);
                for recipe in &data_store
                    .technology_tree
                    .node_weight(NodeIndex::from(current.id))
                    .unwrap()
                    .effect
                    .unlocked_recipes
                {
                    self.recipe_active[recipe.into_usize()] = true;
                }
                self.current_technology = None;

                // Since we only check if a tech is finished at the end of each update, it is possible we produced more science progress in this tick, than was required.
                // To not lose this science (which would mean players sometimes are unable to finish a technology even if they supplied enough science packs),
                // We just store the overflow science and apply it if possible
                let overshoot = final_science_used - tech_cost.0;

                for (overflow_buffer, unit_cost) in self
                    .science_overflow_buffer
                    .iter_mut()
                    .zip(tech_cost.1.iter())
                {
                    *overflow_buffer = (*overflow_buffer).checked_add(u32::try_from(overshoot * u64::from(*unit_cost))
                        .expect("impossible since the most science unit produced this tick are u16::MAX (see funciton args)"))
                            .expect("Science Overflow buffer overflowed (Ironic).");
                }

                dbg!(&self.science_overflow_buffer);
            }
        } else {
            assert_eq!(
                tech_progress, 0,
                "Labs should not be working without a tech set!"
            );
        }
    }

    pub fn generate_render_graph<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &self,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Graph<data::Technology<RecipeIdxType>, (), Directed, u16, DefaultNodeShape, DefaultEdgeShape>
    {
        egui_graphs::to_graph_custom::<_, _, _, _, DefaultNodeShape, DefaultEdgeShape>(
            &data_store.technology_tree,
            |node| {
                if self.finished_technologies.contains(&Technology {
                    id: node.id().index().try_into().unwrap(),
                }) {
                    node.set_color(Color32::GREEN);
                } else if data_store
                    .technology_tree
                    .edges_directed(node.id(), petgraph::Direction::Incoming)
                    .all(|edge| {
                        self.finished_technologies.contains(&Technology {
                            id: edge.source().index().try_into().unwrap(),
                        })
                    })
                {
                    node.set_color(Color32::YELLOW);
                } else {
                    node.set_color(Color32::RED);
                }

                node.set_label(
                    data_store
                        .technology_tree
                        .node_weight(NodeIndex::from(u16::try_from(node.id().index()).unwrap()))
                        .unwrap()
                        .name
                        .clone(),
                );
            },
            |_edge| {},
        )
    }

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
    ) -> impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>> {
        {
            profiling::scope!("Update Tech Tree colors");
            for tech in 0..data_store.technology_costs.len() {
                let node = render_graph.node_mut(NodeIndex::new(tech)).unwrap();

                if self.finished_technologies.contains(&Technology {
                    id: node.id().index().try_into().unwrap(),
                }) {
                    node.set_color(Color32::GREEN);
                } else if data_store
                    .technology_tree
                    .edges_directed(node.id(), petgraph::Direction::Incoming)
                    .all(|edge| {
                        self.finished_technologies.contains(&Technology {
                            id: edge.source().index().try_into().unwrap(),
                        })
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
                    let cost = data_store.technology_costs[tech.id as usize].0;
                    ui.add(
                        ProgressBar::new((done as f64 / cost as f64) as f32)
                            .corner_radius(CornerRadius::ZERO)
                            .text(format!("{}/{}", done, cost)),
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

                    let already_researched = self.finished_technologies.contains(&Technology {
                        id: selected_node.index().try_into().unwrap(),
                    });

                    let possible_to_research = data_store
                        .technology_tree
                        .edges_directed(*selected_node, petgraph::Direction::Incoming)
                        .all(|edge| {
                            self.finished_technologies.contains(&Technology {
                                id: edge.source().index().try_into().unwrap(),
                            })
                        });

                    let is_currently_researching = Some(Technology {
                        id: selected_node.index().try_into().unwrap(),
                    }) == self.current_technology;

                    if ui
                        .add_enabled(
                            !already_researched
                                && !is_currently_researching
                                && possible_to_research,
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
                }
            },
        );

        let mut view =
            GraphView::<_, _, _, _, _, _, LayoutStateHierarchical, LayoutHierarchical>::new(
                render_graph,
            )
            .with_navigations(&SettingsNavigation::new().with_zoom_and_pan_enabled(true))
            .with_interactions(&SettingsInteraction::new().with_node_selection_enabled(true))
            .with_styles(&SettingsStyle::new().with_labels_always(true));

        ui.add(&mut view);

        ret.into_iter()
    }
}
