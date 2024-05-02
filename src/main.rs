#![feature(test)]

use factory::{belt::smart::SmartBelt, item::Iron, producer::MultiProducerStore};

use factory::belt::belt::Belt;

fn main() {
    let mut store = MultiProducerStore::<Iron>::new();

    let mut belt = SmartBelt::<Iron>::new(100);

    factory::test();

    loop {
        store.update();

        belt.update();
    }
}
