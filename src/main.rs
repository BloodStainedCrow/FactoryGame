#![feature(test)]

use std::sync::Arc;

use factory::{
    assembler::Assembler,
    belt::{
        in_inserter::InInserter, out_inserter::OutInserter, simple::SimpleBelt, splitter::Splitter,
    },
    item::{Iron, RECIPES},
};

use scoped_threadpool::Pool;

fn main() {
    let mut assembler = Assembler::<Iron, Iron>::new(RECIPES[0]);
    assembler
        .ingredient_storage
        .count
        .store(10, std::sync::atomic::Ordering::Relaxed);

    let mut belt = SimpleBelt::new(50);

    let mut belt2 = SimpleBelt::new(10);
    let mut belt3 = SimpleBelt::new(40);

    let mut splitter = Splitter::new(&mut belt, &mut [&mut belt2, &mut belt3]);

    OutInserter::create_and_add_strict(Arc::downgrade(&assembler.result_storage), &mut belt, 49);
    InInserter::create_and_add_strict(Arc::downgrade(&assembler.ingredient_storage), &mut belt2, 0);

    let mut pool = Pool::new(12);

    for _ in 0..240 {
        pool.scoped(|scoped| {
            // for producer in &mut producers {
            //     scoped.execute(|| producer.update());
            // }

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
