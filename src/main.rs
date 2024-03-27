#![feature(test)]

use std::sync::Arc;

use factory::{
    assembler::Assembler,
    belt::{
        in_inserter::InInserter, out_inserter::OutInserter, simple::SimpleBelt, splitter::Splitter,
    },
    item::{Item, RECIPES},
    producer::Producer,
};

use scoped_threadpool::Pool;

fn main() {
    let mut assembler = Assembler::new(RECIPES[0]);

    let mut producers = [Producer::new(Item::Iron), Producer::new(Item::Iron)];

    let mut belt = SimpleBelt::new(50);

    let mut belt2 = SimpleBelt::new(10);
    let mut belt3 = SimpleBelt::new(40);

    let mut splitter = Splitter::new(&mut belt, &mut [&mut belt2, &mut belt3]);

    // belt.add_out_inserter(OutInserter {
    //     connected_count: Arc::downgrade(&producers[0].count),
    //     belt_pos: 19,
    // });

    // belt.add_out_inserter(OutInserter {
    //     connected_count: Arc::downgrade(&producers[1].count),
    //     belt_pos: 37,
    // });

    belt2.add_in_inserter(InInserter {
        connected_count: Arc::downgrade(&assembler.ingredient_count),
        belt_pos: 0,
    });

    belt.add_out_inserter(OutInserter {
        connected_count: Arc::downgrade(&assembler.result_count),
        belt_pos: 49,
    });

    let mut pool = Pool::new(12);

    for _ in 0..240 {
        pool.scoped(|scoped| {
            for producer in &mut producers {
                scoped.execute(|| producer.update());
            }

            scoped.execute(|| assembler.update());

            scoped.execute(|| belt.update());
            scoped.execute(|| belt2.update());
            scoped.execute(|| belt3.update());

            scoped.execute(|| splitter.update());
        });

        println!("{belt}");
        println!("{belt2}");
        println!("{belt3}");
        println!();
    }

    println!("{assembler:?}");
}
