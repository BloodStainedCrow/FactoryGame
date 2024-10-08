#![feature(test)]
#![feature(portable_simd)]
#![feature(adt_const_params)]

use belt::smart::SmartBelt;
use item::{Iron, ItemStorage};

pub mod assembler;
pub mod belt;
pub mod inserter;
pub mod item;
pub mod producer;

pub fn test() {
    let mut belt = SmartBelt::<Iron>::new(10);

    let mut storages: Vec<ItemStorage<Iron>> = vec![];

    belt.update_inserters(&mut storages);
}

#[cfg(test)]
mod tests {
    use std::num::NonZeroU16;

    use rand::random;
    use test::Bencher;

    use crate::{
        assembler::MultiAssemblerStoreOne,
        belt::{belt::Belt, smart::SmartBelt},
        inserter::{BeltStorageInserter, Dir},
        item::{Iron, IronOre, ItemStorage},
    };
    extern crate test;

    // multi_stores is a placeholder for assemblers for all recipes
    const NUM_RECIPES: usize = 250;
    const NUM_ASSEMBLERS_PER_RECIPE: usize = 10_000;

    const NUM_BELTS: usize = NUM_RECIPES * 2;
    const BELT_LEN: usize = NUM_ASSEMBLERS_PER_RECIPE;
    // Worst Case: Every step is used by an inserter
    const NUM_INSERTERS_PER_BELT: usize = NUM_ASSEMBLERS_PER_RECIPE;

    #[bench]
    fn bench_factory_single_threaded(b: &mut Bencher) {
        // Do a single update "step"
        let mut multi_stores = vec![];

        for _ in 0..NUM_RECIPES {
            multi_stores.push(MultiAssemblerStoreOne::<Iron>::new());
        }

        for store in &mut multi_stores {
            for i in 0..NUM_ASSEMBLERS_PER_RECIPE {
                store.add_assembler();
                *store.get_input_1_mut(i).expect("Hardcoded") =
                    ItemStorage::<IronOre>::new(random());
            }
        }

        let mut belts: Vec<SmartBelt<Iron>> = vec![];

        for _ in 0..NUM_BELTS {
            let mut belt = SmartBelt::<Iron>::new(BELT_LEN);

            for _ in 0..NUM_INSERTERS_PER_BELT {
                // TODO: Use add_inserter
                let mut rand: u8 = random();
                while rand == 0u8 {
                    rand = random();
                }
                belt.inserters.offsets.push(0);
                belt.inserters.out_inserters.push(BeltStorageInserter::<
                    Iron,
                    { Dir::BeltToStorage },
                >::new(
                    NonZeroU16::new(rand.into()).expect("Hardcoded"),
                ));
            }

            belts.push(belt);
        }

        b.iter(|| {
            for store in &mut multi_stores {
                store.update_branchless(64);
            }

            for belt in &mut belts {
                assert!(belt.try_insert_item(belt.get_len() - 1).is_ok());

                belt.update_inserters(multi_stores[0].get_outputs_mut());

                belt.update();
            }
        });
    }

    #[bench]
    fn bench_factory_single_threaded_assembler(b: &mut Bencher) {
        // Do a single update "step"
        let mut multi_stores = vec![];

        for _ in 0..NUM_RECIPES {
            multi_stores.push(MultiAssemblerStoreOne::<Iron>::new());
        }

        for store in &mut multi_stores {
            for i in 0..NUM_ASSEMBLERS_PER_RECIPE {
                store.add_assembler();
                *store.get_input_1_mut(i).expect("Hardcoded") =
                    ItemStorage::<IronOre>::new(random());
            }
        }

        #[allow(clippy::collection_is_never_read)]
        let mut belts: Vec<SmartBelt<Iron>> = vec![];

        for _ in 0..NUM_BELTS {
            let mut belt = SmartBelt::<Iron>::new(BELT_LEN);

            for _ in 0..NUM_INSERTERS_PER_BELT {
                // TODO: Use add_inserter
                let mut rand: u8 = random();
                while rand == 0u8 {
                    rand = random();
                }
                belt.inserters.offsets.push(0);
                belt.inserters.out_inserters.push(BeltStorageInserter::<
                    Iron,
                    { Dir::BeltToStorage },
                >::new(
                    NonZeroU16::new(rand.into()).expect("Hardcoded"),
                ));
            }

            belts.push(belt);
        }

        b.iter(|| {
            for store in &mut multi_stores {
                store.update_branchless(64);
            }
        });
    }

    #[bench]
    fn bench_factory_single_threaded_belt(b: &mut Bencher) {
        // Do a single update "step"
        let mut multi_stores = vec![];

        for _ in 0..NUM_RECIPES {
            multi_stores.push(MultiAssemblerStoreOne::<Iron>::new());
        }

        for store in &mut multi_stores {
            for i in 0..NUM_ASSEMBLERS_PER_RECIPE {
                store.add_assembler();
                *store.get_input_1_mut(i).expect("Hardcoded") =
                    ItemStorage::<IronOre>::new(random());
            }
        }

        let mut belts: Vec<SmartBelt<Iron>> = vec![];

        for _ in 0..NUM_BELTS {
            let mut belt = SmartBelt::<Iron>::new(BELT_LEN);

            for _ in 0..NUM_INSERTERS_PER_BELT {
                // TODO: Use add_inserter
                let mut rand: u8 = random();
                while rand == 0u8 {
                    rand = random();
                }
                belt.inserters.offsets.push(0);
                belt.inserters.out_inserters.push(BeltStorageInserter::<
                    Iron,
                    { Dir::BeltToStorage },
                >::new(
                    NonZeroU16::new(rand.into()).expect("Hardcoded"),
                ));
            }

            belts.push(belt);
        }

        b.iter(|| {
            for belt in &mut belts {
                assert!(belt.try_insert_item(belt.get_len() - 1).is_ok());
                belt.update();
            }
        });
    }

    #[bench]
    fn bench_factory_single_threaded_inserter(b: &mut Bencher) {
        // Do a single update "step"
        let mut multi_stores = vec![];

        for _ in 0..NUM_RECIPES {
            multi_stores.push(MultiAssemblerStoreOne::<Iron>::new());
        }

        for store in &mut multi_stores {
            for i in 0..NUM_ASSEMBLERS_PER_RECIPE {
                store.add_assembler();
                *store.get_input_1_mut(i).expect("Hardcoded") =
                    ItemStorage::<IronOre>::new(random());
            }
        }

        let mut belts: Vec<SmartBelt<Iron>> = vec![];

        for _ in 0..NUM_BELTS {
            let mut belt = SmartBelt::<Iron>::new(BELT_LEN);

            for _ in 0..NUM_INSERTERS_PER_BELT {
                // TODO: Use add_inserter
                let mut rand: u8 = random();
                while rand == 0u8 {
                    rand = random();
                }
                belt.inserters.offsets.push(0);
                belt.inserters.out_inserters.push(BeltStorageInserter::<
                    Iron,
                    { Dir::BeltToStorage },
                >::new(
                    NonZeroU16::new(rand.into()).expect("Hardcoded"),
                ));
            }

            belts.push(belt);
        }

        b.iter(|| {
            for belt in &mut belts {
                assert!(belt.try_insert_item(belt.get_len() - 1).is_ok());
                belt.update_inserters(multi_stores[0].get_outputs_mut());
            }
        });
    }
}
