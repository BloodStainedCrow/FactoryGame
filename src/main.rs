use std::num::NonZeroU16;

use factory::assembler::MultiAssemblerStoreOne;
use factory::inserter::Inserter;
use factory::item::IronOre;
use factory::{
    belt::smart::SmartBelt, item::Iron, item::ItemStorage, producer::MultiProducerStore,
};

use factory::belt::belt::Belt;
use rand::random;

fn main() {
    // multi_stores is a placeholder for assemblers for all recipes
    const NUM_RECIPES: usize = 250;
    const NUM_ASSEMBLERS_PER_RECIPE: usize = 1_000;

    const NUM_BELTS: usize = NUM_RECIPES * 2;
    const BELT_LEN: usize = NUM_ASSEMBLERS_PER_RECIPE;
    // Worst Case: Every step is used by an inserter
    const NUM_INSERTERS_PER_BELT: usize = NUM_ASSEMBLERS_PER_RECIPE;

    // Do a single update "step"
    let mut multi_stores = vec![];

    for _ in 0..NUM_RECIPES {
        multi_stores.push(MultiAssemblerStoreOne::<Iron>::new());
    }

    for store in &mut multi_stores {
        for i in 0..NUM_ASSEMBLERS_PER_RECIPE {
            store.add_assembler();
            *store.get_input_1_mut(i).expect("Hardcoded") = ItemStorage::<IronOre>::new(random());
        }
    }

    let mut belts: Vec<SmartBelt<Iron>> = vec![];

    for _ in 0..NUM_BELTS {
        let mut belt = SmartBelt::<Iron>::new(BELT_LEN);

        for _ in 0..NUM_INSERTERS_PER_BELT {
            // TODO: Use add_inserter
            let mut rand = random();
            while rand == 0u16 {
                rand = random();
            }
            belt.inserters.push((
                0,
                Inserter::<Iron>::new(NonZeroU16::new(rand).expect("Hardcoded")),
            ));
        }

        belts.push(belt);
    }

    loop {
        for store in &mut multi_stores {
            store.update_branchless(64);
        }

        for belt in &mut belts {
            belt.update_inserters(multi_stores[0].get_outputs_mut());

            belt.update();
        }
    }
}
