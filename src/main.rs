use std::{
    num::{NonZero, NonZeroU16},
    thread::sleep,
    time::Duration,
};

use factory::{
    assembler::MultiAssemblerStoreOne,
    belt::{belt::Belt, smart::SmartBelt},
    item::{Iron, ItemStorage},
    power::PowerGridIdentifier,
};

fn main() {
    // let mut store = MultiAssemblerStoreOne::<Iron>::new();
    // let mut grids = [0; PowerGridIdentifier::MAX as usize + 1];

    // for _ in 0..1_000 {
    //     store.add_assembler();
    // }

    // loop {
    //     store.update_branchless(64, &mut grids);
    // }
    loop_me();
}

fn loop_me() {
    const BELT_LEN: usize = 100;
    let mut store = [
        ItemStorage::<Iron>::default(),
        ItemStorage::<Iron>::default(),
    ];

    let mut belt: SmartBelt<Iron> = SmartBelt::new(BELT_LEN);

    belt.add_out_inserter(0, NonZero::new(1).expect("Hardcoded"))
        .expect("Should work");

    belt.add_in_inserter(
        (BELT_LEN - 1).try_into().expect("Hardcoded"),
        NonZero::new(1).expect("Hardcoded"),
    )
    .expect("Should work");

    belt.try_insert_item(0).expect("Should work!");

    loop {
        belt.update();
        belt.update_inserters(&mut store);

        // sleep(Duration::from_micros(16666));
        println!("{}", &belt as &dyn Belt<Iron>);
    }
}

fn test() {
    let power = [12; 1000];
    let mut grids = [0; 256];
    let indices = [2; 1000];

    let iter = power.iter().zip(indices.iter());
}
