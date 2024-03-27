use std::sync::{atomic::AtomicU64, Arc};

use factory::{
    belt::{optimized::OptimizedBelt, out_inserter::OutInserter},
    item::Item,
    producer::Producer,
};
use proptest::prelude::*;
use scoped_threadpool::Pool;

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

    let mut producers = [
        Producer {
            item: Item::Iron,
            timer: 0,
            count: Arc::new(atomic1),
        },
        Producer {
            item: Item::Iron,
            timer: 0,
            count: Arc::new(atomic2),
        },
    ];

    let mut belt = OptimizedBelt::new(10);

    belt.add_out_inserter(OutInserter {
        connected_count: Arc::downgrade(&producers[0].count),
        belt_pos: 9,
    });

    belt.add_out_inserter(OutInserter {
        connected_count: Arc::downgrade(&producers[1].count),
        belt_pos: 7,
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
    println!("{belt:?}");

    for i in 0..10 {
        assert_eq!(belt.get_item_at(i), EXPECTED[i as usize]);
    }
}
