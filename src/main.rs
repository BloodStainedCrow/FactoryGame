#![feature(test)]

use std::sync::atomic::AtomicU64;

use factory::{
    assembler::Assembler,
    belt::{in_inserter::InInserter, out_inserter::OutInserter, simple::SimpleBelt},
    item::{Item, RECIPES},
    producer::Producer,
};

use scoped_threadpool::Pool;

fn main() {
    // let mut producers = [Producer::new(Item::Iron)];
    let atomic1 = AtomicU64::new(0);
    let atomic2 = AtomicU64::new(0);

    let atomic3 = AtomicU64::new(1);
    let atomic4 = AtomicU64::new(0);

    let mut assembler = Assembler {
        recipe: RECIPES[0],
        timer: 0,
        ingredient_count: &atomic3,
        result_count: &atomic4,
    };

    let mut producers = [
        Producer {
            item: Item::Iron,
            timer: 0,
            count: &atomic1,
        },
        Producer {
            item: Item::Iron,
            timer: 0,
            count: &atomic2,
        },
    ];

    let mut belt = SimpleBelt::new(80);

    belt.add_out_inserter(OutInserter {
        connected_count: producers[0].count,
        belt_pos: 19,
    });

    belt.add_out_inserter(OutInserter {
        connected_count: producers[1].count,
        belt_pos: 37,
    });

    belt.add_in_inserter(InInserter {
        connected_count: assembler.ingredient_count,
        belt_pos: 0,
    });

    belt.add_out_inserter(OutInserter {
        connected_count: assembler.result_count,
        belt_pos: 79,
    });

    let mut pool = Pool::new(12);

    for _ in 0..1000 {
        pool.scoped(|scoped| {
            for producer in &mut producers {
                scoped.execute(|| producer.update());
            }

            scoped.execute(|| assembler.update());

            scoped.execute(|| belt.update());
        });

        println!("{belt}");
    }

    println!("{assembler:?}");
}
