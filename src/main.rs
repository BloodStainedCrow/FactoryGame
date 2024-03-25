#![feature(test)]

use std::sync::atomic::AtomicU64;

use factory::{
    belt::{inserter::Inserter, optimized::OptimizedBelt},
    item::Item,
    producer::Producer,
};

use scoped_threadpool::Pool;

fn main() {
    // let mut producers = [Producer::new(Item::Iron)];
    let atomic1 = AtomicU64::new(0);
    let atomic2 = AtomicU64::new(0);

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

    let mut belt = OptimizedBelt::new(80);

    belt.add_inserter(Inserter {
        connected_producer_count: producers[0].count,
        belt_pos: 79,
    });

    belt.add_inserter(Inserter {
        connected_producer_count: producers[1].count,
        belt_pos: 77,
    });

    let mut pool = Pool::new(12);

    for _ in 0..4 {
        pool.scoped(|scoped| {
            for producer in &mut producers {
                scoped.execute(|| producer.update());
            }

            scoped.execute(|| belt.update());
        });

        println!("{belt}");
    }

    println!("{producers:?}");
}
