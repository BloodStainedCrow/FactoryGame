#![feature(test)]

use factory::assembler::MultiAssemblerStoreOne;
use factory::item::IronOre;
use factory::{belt::smart::SmartBelt, item::Iron, producer::MultiProducerStore};

use factory::belt::belt::Belt;

fn main() {
    let mut store = MultiProducerStore::<IronOre>::new();

    let mut belt = SmartBelt::<Iron>::new(100);

    factory::test();

    for _ in 0..10000 {
        store.update(64);

        belt.update();
    }

    let mut multi_store = MultiAssemblerStoreOne::<IronOre, Iron>::new();

    for _ in 0..1_000_000 {
        multi_store.add_assembler();
    }

    multi_store.update_branchless(64);
}
