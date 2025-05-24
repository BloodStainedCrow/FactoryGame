use std::collections::HashMap;

use itertools::Itertools;
use rayon::iter::{IntoParallelRefMutIterator, ParallelIterator};

use crate::{
    assembler::TIMERTYPE,
    data::{DataStore, ItemRecipeDir},
    frontend::world::{tile::Dir, Position},
    item::{IdxTrait, Item, Recipe, WeakIdxTrait, ITEMCOUNTTYPE},
};

struct CanonicalRepresentation<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    current_tech: Option<usize>,

    assemblers: HashMap<Position, Entity<Assembler<ItemIdxType, RecipeIdxType>>>,
    labs: HashMap<Position, Entity<Lab<ItemIdxType>>>,
    power_poles: HashMap<Position, Entity<PowerPole>>,
}

struct Entity<Kind> {
    pos: Position,
    forward_dir: Dir,
    ty: u8,
    kind: Kind,
}

struct Assembler<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    recipe: Option<Recipe<RecipeIdxType>>,
    ings: HashMap<Item<ItemIdxType>, ITEMCOUNTTYPE>,
    outputs: HashMap<Item<ItemIdxType>, ITEMCOUNTTYPE>,
}

struct Lab<ItemIdxType: WeakIdxTrait> {
    items: HashMap<Item<ItemIdxType>, ITEMCOUNTTYPE>,
}

struct BeltTile<ItemIdxType: WeakIdxTrait> {
    locs: Vec<Option<Item<ItemIdxType>>>,
}

struct PowerPole {}

enum NodeKind<ItemIdxType: WeakIdxTrait> {
    Inserter {
        filter: Option<Item<ItemIdxType>>,
        hand_full: bool,
        hand_position: TIMERTYPE,
    },
    SolarPanel,
    Beacon {
        modules: Vec<usize>,
    },
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>
    CanonicalRepresentation<ItemIdxType, RecipeIdxType>
{
    pub fn update(&mut self, data_store: &DataStore<ItemIdxType, RecipeIdxType>) {
        // From tile to the power pole that poweres it
        let power_pole_tiles: HashMap<Position, Position> = {
            let mut map = HashMap::default();
            for (
                _,
                Entity {
                    pos,
                    forward_dir,
                    ty,
                    kind: PowerPole {},
                },
            ) in self.power_poles.iter().sorted_by(|(a_pos, _), (b_pos, _)| {
                a_pos.y.cmp(&b_pos.y).then(a_pos.x.cmp(&b_pos.x))
            }) {
                // FIXME: Do not ignore rotation

                let power_range = data_store.power_pole_data[usize::from(*ty)].power_range;
                let tile_size = data_store.power_pole_data[usize::from(*ty)].size;
                for x in (pos.x - usize::from(power_range))
                    ..(pos.x + usize::from(tile_size.0) + usize::from(power_range))
                {
                    for y in (pos.y - usize::from(power_range))
                        ..(pos.y + usize::from(tile_size.1) + usize::from(power_range))
                    {
                        map.entry(Position { x, y }).or_insert(*pos);
                    }
                }
            }

            map
        };

        // Update Assemblers
        self.assemblers.par_iter_mut().for_each(
            |(
                _,
                Entity {
                    pos,
                    forward_dir,
                    ty,
                    kind:
                        Assembler {
                            recipe,
                            ings,
                            outputs,
                        },
                },
            )| {
                if let Some(recipe) = recipe {
                    // Check if we have enough items and space to craft
                    let mut enough_resources_and_space_to_work = true;
                    for (dir, item, count) in &data_store.recipe_to_items_and_amounts[recipe] {
                        match dir {
                            ItemRecipeDir::Ing => {
                                if *ings.get(item).unwrap_or(&0) < *count {
                                    enough_resources_and_space_to_work = false;
                                    break;
                                }
                            },
                            ItemRecipeDir::Out => {
                                if *outputs.get(item).unwrap_or(&0) + *count > 100 {
                                    enough_resources_and_space_to_work = false;
                                    break;
                                }
                            },
                        }
                    }

                    if enough_resources_and_space_to_work {
                        // Lets work. We need power and increase the timer

                        // Find the power pole we are connected to:
                        let pole_pos = get_connected_pole_pos(
                            *forward_dir,
                            *pos,
                            data_store.assembler_info[usize::from(*ty)].size,
                            &power_pole_tiles,
                        );

                        if let Some(pole_pos) = pole_pos {
                            // We are connected to a power pole
                            todo!()
                        } else {
                            // Unconnected. Do nothing
                        }
                    } else {
                        // We are idling
                    }
                } else {
                    // No recipe. Do nothing
                }
            },
        );

        let current_tech_costs = self.current_tech.map(|tech| {
            data_store.technology_costs[tech]
                .1
                .iter()
                .copied()
                .zip(&data_store.science_bottle_items)
                .collect::<Vec<_>>()
        });

        // Update Labs
        self.labs.par_iter_mut().for_each(
            |(
                _,
                Entity {
                    pos,
                    forward_dir,
                    ty,
                    kind: Lab { items },
                },
            )| {
                if let Some(current_tech_costs) = &current_tech_costs {
                    let enough_science = current_tech_costs
                        .iter()
                        .all(|(cost, item)| *items.get(item).unwrap_or(&0) >= *cost);

                    if enough_science {
                        // Lets work. We need power and increase the timer

                        // Find the power pole we are connected to:
                        let pole_pos = get_connected_pole_pos(
                            *forward_dir,
                            *pos,
                            data_store.lab_info[usize::from(*ty)].size,
                            &power_pole_tiles,
                        );

                        if let Some(pole_pos) = pole_pos {
                            // We are connected to a power pole
                            todo!()
                        } else {
                            // Unconnected. Do nothing
                        }
                    } else {
                        // We are idling
                    }
                } else {
                    // No tech
                }
            },
        );
    }
}

fn get_connected_pole_pos(
    forward_dir: Dir,
    pos: Position,
    size: (u16, u16),
    power_pole_tiles: &HashMap<Position, Position>,
) -> Option<Position> {
    // Scan in lines from top left, to top right
    // FIXME: Do not ignore rotation
    for y_offs in 0..size.1 {
        for x_offs in 0..size.0 {
            if let Some(pole_pos) = power_pole_tiles.get(&Position {
                x: pos.x.checked_add(x_offs.into()).unwrap(),
                y: pos.y.checked_add(y_offs.into()).unwrap(),
            }) {
                return Some(*pole_pos);
            }
        }
    }

    None
}
