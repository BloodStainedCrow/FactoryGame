use std::{collections::HashSet, hash::Hash};

use itertools::Itertools;
use petgraph::visit::IntoNodeIdentifiers;

use crate::item::Item;

fn do_inserter_updates(update_kinds: impl IntoIterator<Item = UpdateKind>) -> usize {
    let update_kinds = update_kinds.into_iter();
    let mut graph: petgraph::prelude::UnGraph<_, _> =
        petgraph::prelude::UnGraph::with_capacity(update_kinds.size_hint().0, 0);

    for update in update_kinds {
        let needed_resources = update.needed_resources();
        let new_node = graph.add_node((update, needed_resources.clone()));

        for node_id in graph.node_identifiers().collect_vec() {
            if node_id == new_node {
                continue;
            }

            let (_, needed_for_in_graph) = graph.node_weight(node_id).unwrap();

            if !needed_resources.is_disjoint(&needed_for_in_graph) {
                graph.add_edge(new_node, node_id, ());
            }
        }
    }

    let (coloring, num_colors) = petgraph::algo::coloring::dsatur_coloring(&graph);

    num_colors
}

enum UpdateKind {
    SingleItemInserter {
        item: Item<u8>,
        source: SingleItemInserterSource,
        dest: SingleItemInserterDest,
    },
    ManyItemInserter {
        items: Vec<Item<u8>>,
        source: ManyItemInserterSource,
        dest: ManyItemInserterDest,
    },
}

enum SingleItemInserterSource {
    SingleBelt(Belt),
    DoubleBelt([Belt; 2]),
    PureChest,
    SushiChest,
}

impl SingleItemInserterSource {
    fn insert_resources(&self, item: Item<u8>, res: &mut HashSet<Resource>) {
        match self {
            Self::SingleBelt(belt) => match belt {
                Belt::Pure => res.insert(Resource::PureBelts(item)),
                Belt::Sushi => res.insert(Resource::SushiBelts),
            },
            Self::DoubleBelt([a, b]) => {
                match a {
                    Belt::Pure => res.insert(Resource::PureBelts(item)),
                    Belt::Sushi => res.insert(Resource::SushiBelts),
                };
                match b {
                    Belt::Pure => res.insert(Resource::PureBelts(item)),
                    Belt::Sushi => res.insert(Resource::SushiBelts),
                }
            },
            Self::PureChest => res.insert(Resource::PureStorages(item)),
            Self::SushiChest => res.insert(Resource::SushiChests),
        };
    }
}

enum SingleItemInserterDest {
    SingleBelt(Belt),
    PureChest,
    SushiChest,
}

impl SingleItemInserterDest {
    fn insert_resources(&self, item: Item<u8>, res: &mut HashSet<Resource>) {
        match self {
            Self::SingleBelt(belt) => match belt {
                Belt::Pure => res.insert(Resource::PureBelts(item)),
                Belt::Sushi => res.insert(Resource::SushiBelts),
            },
            Self::PureChest => res.insert(Resource::PureStorages(item)),
            Self::SushiChest => res.insert(Resource::SushiChests),
        };
    }
}

enum ManyItemInserterSource {
    SingleBelt(Belt),
    DoubleBelt([Belt; 2]),
    PureChests,
    SushiChest,
}

impl ManyItemInserterSource {
    fn insert_resources(&self, items: &Vec<Item<u8>>, res: &mut HashSet<Resource>) {
        match self {
            Self::SingleBelt(belt) => match belt {
                Belt::Pure => {
                    for item in items {
                        res.insert(Resource::PureBelts(*item));
                    }
                },
                Belt::Sushi => {
                    res.insert(Resource::SushiBelts);
                },
            },
            Self::DoubleBelt([a, b]) => {
                match a {
                    Belt::Pure => {
                        for item in items {
                            res.insert(Resource::PureBelts(*item));
                        }
                    },
                    Belt::Sushi => {
                        res.insert(Resource::SushiBelts);
                    },
                };
                match b {
                    Belt::Pure => {
                        for item in items {
                            res.insert(Resource::PureBelts(*item));
                        }
                    },
                    Belt::Sushi => {
                        res.insert(Resource::SushiBelts);
                    },
                }
            },
            Self::PureChests => {
                for item in items {
                    res.insert(Resource::PureStorages(*item));
                }
            },
            Self::SushiChest => {
                res.insert(Resource::SushiChests);
            },
        }
    }
}

enum ManyItemInserterDest {
    SingleBelt(Belt),
    PureChests,
    SushiChest,
}

impl ManyItemInserterDest {
    fn insert_resources(&self, items: &Vec<Item<u8>>, res: &mut HashSet<Resource>) {
        match self {
            Self::SingleBelt(belt) => match belt {
                Belt::Pure => {
                    for item in items {
                        res.insert(Resource::PureBelts(*item));
                    }
                },
                Belt::Sushi => {
                    res.insert(Resource::SushiBelts);
                },
            },
            Self::PureChests => {
                for item in items {
                    res.insert(Resource::PureBelts(*item));
                }
            },
            Self::SushiChest => {
                res.insert(Resource::SushiChests);
            },
        };
    }
}

enum Belt {
    Pure,
    Sushi,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum Resource {
    PureStorages(Item<u8>),
    PureBelts(Item<u8>),
    SushiBelts,
    SushiChests,
}

impl UpdateKind {
    fn needed_resources(&self) -> HashSet<Resource> {
        let mut ret = HashSet::default();

        match self {
            Self::SingleItemInserter { item, source, dest } => {
                source.insert_resources(*item, &mut ret);
                dest.insert_resources(*item, &mut ret);
            },
            Self::ManyItemInserter {
                items,
                source,
                dest,
            } => {
                source.insert_resources(items, &mut ret);
                dest.insert_resources(items, &mut ret);
            },
        }

        ret
    }
}

#[cfg(test)]
mod test {
    use crate::item::Item;

    use super::*;

    #[test]
    fn single_update() {
        let num_colors = do_inserter_updates(vec![UpdateKind::SingleItemInserter {
            item: Item { id: 0 },
            source: SingleItemInserterSource::PureChest,
            dest: SingleItemInserterDest::SingleBelt(Belt::Pure),
        }]);

        assert_eq!(num_colors, 1);
    }

    #[test]
    fn two_pure_single_item_different() {
        let num_colors = do_inserter_updates(vec![
            UpdateKind::SingleItemInserter {
                item: Item { id: 0 },
                source: SingleItemInserterSource::PureChest,
                dest: SingleItemInserterDest::SingleBelt(Belt::Pure),
            },
            UpdateKind::SingleItemInserter {
                item: Item { id: 1 },
                source: SingleItemInserterSource::PureChest,
                dest: SingleItemInserterDest::SingleBelt(Belt::Pure),
            },
        ]);

        assert_eq!(num_colors, 1);
    }

    #[test]
    fn two_pure_single_item_same_item_storage_belt_independant() {
        let num_colors = do_inserter_updates(vec![
            UpdateKind::SingleItemInserter {
                item: Item { id: 0 },
                source: SingleItemInserterSource::PureChest,
                dest: SingleItemInserterDest::PureChest,
            },
            UpdateKind::SingleItemInserter {
                item: Item { id: 0 },
                source: SingleItemInserterSource::SingleBelt(Belt::Pure),
                dest: SingleItemInserterDest::SingleBelt(Belt::Pure),
            },
        ]);

        assert_eq!(num_colors, 1);
    }

    #[test]
    fn two_pure_single_item_same_item_storage_belt_storage_storage() {
        let num_colors = do_inserter_updates(vec![
            UpdateKind::SingleItemInserter {
                item: Item { id: 0 },
                source: SingleItemInserterSource::PureChest,
                dest: SingleItemInserterDest::PureChest,
            },
            UpdateKind::SingleItemInserter {
                item: Item { id: 0 },
                source: SingleItemInserterSource::PureChest,
                dest: SingleItemInserterDest::SingleBelt(Belt::Pure),
            },
        ]);

        assert_eq!(num_colors, 2);
    }
}
