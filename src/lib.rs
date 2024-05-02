#![feature(test)]
#![feature(const_trait_impl)]
#![feature(portable_simd)]

use belt::smart::SmartBelt;
use item::{Iron, ItemStorage};

pub mod assembler;
pub mod belt;
pub mod inserter;
pub mod item;
pub mod producer;

pub fn test() {
    let mut belt = SmartBelt::<Iron>::new(10);

    let mut storages: Vec<ItemStorage<Iron>> = vec![];

    belt.update_inserters(&mut storages);
}
