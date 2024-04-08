#![feature(test)]
use std::sync::{atomic::AtomicU64, Arc};

use factory::{
    assembler::Assembler,
    belt::{
        simple::SimpleBelt,
        splitter::Splitter,
        strict::{optimized::OptimizedBelt, out_inserter::OutInserterStrict},
    },
    item::{Iron, Item, RECIPES},
    producer::Producer,
};
use proptest::prelude::*;
use scoped_threadpool::Pool;

extern crate test;
use test::Bencher;

proptest! {}

#[test]
fn test_case_simple_optimized_belt_with_inserters() {
    const EXPECTED: [Option<Item>; 10] = [
        None,
        None,
        None,
        None,
        Some(Item::Iron),
        None,
        Some(Item::Iron),
        Some(Item::Iron),
        Some(Item::Iron),
        None,
    ];

    let atomic1 = AtomicU64::new(0);
    let atomic2 = AtomicU64::new(0);

    let mut producers = [Producer::<Iron>::new(), Producer::<Iron>::new()];

    let mut belt = OptimizedBelt::<Iron>::new(10);

    // belt.add_out_inserter(OutInserterStrict {
    //     connected_count: Arc::downgrade(&producers[0].storage),
    //     belt_pos: 9,
    // });

    // belt.add_out_inserter(OutInserterStrict {
    //     connected_count: Arc::downgrade(&producers[1].storage),
    //     belt_pos: 7,
    // });

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
    println!("{belt:?}");

    for i in 0..10 {
        assert_eq!(belt.get_item_at(i), EXPECTED[i as usize]);
    }
}

#[bench]
fn bench_simple_splitter_loop_multithreaded(b: &mut Bencher) {
    let mut assembler = Assembler::<Iron, Iron>::new(RECIPES[0]);

    let mut producers = [Producer::<Iron>::new(), Producer::<Iron>::new()];

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

    // belt2.add_in_inserter(InInserter {
    //     connected_storage: Arc::downgrade(&assembler.ingredient_storage),
    //     belt_pos: 0,
    // });

    // belt.add_out_inserter(OutInserterStrict {
    //     connected_count: Arc::downgrade(&assembler.result_storage),
    //     belt_pos: 49,
    // });

    let mut pool = Pool::new(12);

    b.iter(|| {
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
    });

    println!("{belt}");
    println!("{belt2}");
    println!("{belt3}");
    println!();
    println!("{assembler:?}");
}

#[bench]
fn bench_simple_splitter_loop_singlethreaded(b: &mut Bencher) {
    let mut assembler = Assembler::<Iron, Iron>::new(RECIPES[0]);

    let mut producers = [Producer::<Iron>::new(), Producer::<Iron>::new()];

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

    // belt2.add_in_inserter(InInserter {
    //     connected_storage: Arc::downgrade(&assembler.ingredient_storage),
    //     belt_pos: 0,
    // });

    // belt.add_out_inserter(OutInserterStrict {
    //     connected_count: Arc::downgrade(&assembler.result_storage),
    //     belt_pos: 49,
    // });

    b.iter(|| {
        for mut producer in &mut producers {
            test::black_box(&mut producer).update();
        }

        test::black_box(&mut assembler).update();

        test::black_box(&mut belt).update();
        test::black_box(&mut belt2).update();
        test::black_box(&mut belt3).update();

        test::black_box(&mut splitter).update();
    });

    println!("{belt}");
    println!("{belt2}");
    println!("{belt3}");
    println!();
    println!("{assembler:?}");
}
