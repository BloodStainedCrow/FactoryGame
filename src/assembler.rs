use std::{
    ops::{Add, Sub},
    simd::{
        cmp::{SimdPartialEq, SimdPartialOrd},
        Simd,
    },
    sync::{atomic::Ordering, Arc},
};

use crate::item::{get_max_stack_size, ItemStorageStrict, ItemTrait, Recipe};

#[derive(Debug)]
pub struct Assembler<IngredientItem: ItemTrait, ResultItem: ItemTrait> {
    pub recipe: Recipe,
    pub timer: i32,
    pub ingredient_storage: Arc<ItemStorageStrict<IngredientItem>>,
    pub result_storage: Arc<ItemStorageStrict<ResultItem>>,
}

impl<IngredientItem: ItemTrait, ResultItem: ItemTrait> Assembler<IngredientItem, ResultItem> {
    #[must_use]
    pub fn new(recipe: Recipe) -> Self {
        Self {
            recipe,
            timer: i32::from(recipe.time),
            ingredient_storage: Arc::new(ItemStorageStrict::<IngredientItem>::default()),
            result_storage: Arc::new(ItemStorageStrict::<ResultItem>::default()),
        }
    }

    pub fn update(&mut self) {
        if self.timer <= 0
            && self.ingredient_storage.count.load(Ordering::Relaxed)
                >= self.recipe.ingredient_amount
            && self.result_storage.count.load(Ordering::Relaxed)
                <= get_max_stack_size(self.recipe.result) - self.recipe.result_amount
        {
            self.ingredient_storage
                .count
                .fetch_sub(self.recipe.ingredient_amount, Ordering::Relaxed);
            self.result_storage
                .count
                .fetch_add(self.recipe.result_amount, Ordering::Relaxed);

            self.timer = i32::from(self.recipe.time);
        }
        self.timer -= 1;
    }
}

struct MultiAssemblerStorage {
    timers: Vec<u16>,
    ingredient_counts: Vec<u16>,
    result_counts: Vec<u16>,
    recipe_timers: Vec<u16>,
    recipe_ingredient_counts: Vec<u16>,
    recipe_result_counts: Vec<u16>,
    result_stack_sizes: Vec<u16>,
}

struct MultiAssemblerStorageStatic<const LEN: usize> {
    timers: Box<[u16; LEN]>,
    ingredient_counts: Box<[u16; LEN]>,
    result_counts: Box<[u16; LEN]>,
    recipe_timers: Box<[u16; LEN]>,
    recipe_ingredient_counts: Box<[u16; LEN]>,
    recipe_result_counts: Box<[u16; LEN]>,
    result_stack_sizes: Box<[u16; LEN]>,
}

#[inline(never)]
pub fn bench_1_000_000_assembler_update_packed_storage() {
    type AssemblerSimd = Simd<u16, 16>;

    let mut assemblers = MultiAssemblerStorage {
        timers: vec![],
        ingredient_counts: vec![],
        result_counts: vec![],
        recipe_timers: vec![],
        recipe_ingredient_counts: vec![],
        recipe_result_counts: vec![],
        result_stack_sizes: vec![],
    };

    for _ in 0..1_000_000 {
        assemblers.timers.push(1);

        assemblers.recipe_timers.push(1);
        assemblers.recipe_ingredient_counts.push(1);
        assemblers.recipe_result_counts.push(1);

        assemblers.ingredient_counts.push(65535);
        assemblers.result_counts.push(0);

        assemblers.result_stack_sizes.push(100);
    }

    let zero = AssemblerSimd::splat(0);
    let one = AssemblerSimd::splat(1);
    for _ in 0..1_000 {
        let bb = &mut assemblers;

        for i in 0..bb.timers.len() / AssemblerSimd::LEN {
            let timer = AssemblerSimd::from_slice(bb.timers.split_at_mut(i * AssemblerSimd::LEN).1);
            let ingredient_counts = AssemblerSimd::from_slice(
                bb.ingredient_counts.split_at_mut(i * AssemblerSimd::LEN).1,
            );
            let product_counts =
                AssemblerSimd::from_slice(bb.result_counts.split_at_mut(i * AssemblerSimd::LEN).1);

            let recipe_ingredient_counts = AssemblerSimd::from_slice(
                bb.recipe_ingredient_counts
                    .split_at_mut(i * AssemblerSimd::LEN)
                    .1,
            );
            let recipe_product_counts = AssemblerSimd::from_slice(
                bb.recipe_result_counts
                    .split_at_mut(i * AssemblerSimd::LEN)
                    .1,
            );
            let stack_sizes = AssemblerSimd::from_slice(
                bb.result_stack_sizes.split_at_mut(i * AssemblerSimd::LEN).1,
            );

            let recipe_timer =
                AssemblerSimd::from_slice(bb.recipe_timers.split_at_mut(i * AssemblerSimd::LEN).1);

            let timer_mask = timer.simd_eq(zero);
            let enough_ingredients_mask = ingredient_counts.simd_ge(recipe_ingredient_counts);
            let enough_space_mask = product_counts
                .add(recipe_product_counts)
                .simd_le(stack_sizes);

            let final_mask = timer_mask & enough_ingredients_mask & enough_space_mask;

            // Update the counts if all conditions pass
            final_mask
                .select(
                    ingredient_counts.sub(recipe_ingredient_counts),
                    ingredient_counts,
                )
                .copy_to_slice(bb.ingredient_counts.split_at_mut(i * AssemblerSimd::LEN).1);

            final_mask
                .select(product_counts.add(recipe_product_counts), product_counts)
                .copy_to_slice(bb.result_counts.split_at_mut(i * AssemblerSimd::LEN).1);

            // Set the timer back up if all conditions passed and we created an item
            final_mask
                .select(recipe_timer, timer)
                .copy_to_slice(bb.timers.split_at_mut(i * AssemblerSimd::LEN).1);

            let timer = AssemblerSimd::from_slice(bb.timers.split_at_mut(i * AssemblerSimd::LEN).1);

            // Decrease timer by one if it is larger than 0
            timer_mask
                .select(timer, timer.sub(one))
                .copy_to_slice(bb.timers.split_at_mut(i * AssemblerSimd::LEN).1);
        }
    }

    println!("{}", assemblers.ingredient_counts[0]);
}

#[cfg(test)]
mod tests {
    use crate::item::{Iron, Item};

    use super::*;
    extern crate test;
    use proptest::proptest;
    use test::Bencher;

    proptest! {}

    #[bench]
    fn bench_single_assembler_update(b: &mut Bencher) {
        let mut assembler = Assembler::<Iron, Iron>::new(Recipe {
            ingredient: Item::Iron,
            ingredient_amount: 1,
            result: Item::Iron,
            result_amount: 1,
            time: 1,
        });

        assembler
            .ingredient_storage
            .count
            .store(1_000_000_000, Ordering::Relaxed);

        b.iter(|| {
            let bb = test::black_box(&mut assembler);

            for _ in 0..1_000 {
                bb.update();
            }
            // bb.result_count.store(0, Ordering::Relaxed);
        });

        println!("{assembler:?}");
    }

    #[bench]
    fn bench_1_000_000_assembler_update_packed_storage(b: &mut Bencher) {
        type AssemblerSimd = Simd<u16, 16>;

        let zero = AssemblerSimd::splat(0);
        let one = AssemblerSimd::splat(1);

        let mut assemblers = MultiAssemblerStorage {
            timers: vec![],
            ingredient_counts: vec![],
            result_counts: vec![],
            recipe_timers: vec![],
            recipe_ingredient_counts: vec![],
            recipe_result_counts: vec![],
            result_stack_sizes: vec![],
        };

        for _ in 0..1_000_000 {
            assemblers.timers.push(0);

            assemblers.recipe_timers.push(9);
            assemblers.recipe_ingredient_counts.push(1);
            assemblers.recipe_result_counts.push(1);

            assemblers.ingredient_counts.push(65535);
            assemblers.result_counts.push(0);

            assemblers.result_stack_sizes.push(2000);
        }

        let mut i = 0;

        b.iter(|| {
            let bb = test::black_box(&mut assemblers);

            i += 1;
            for i in 0..bb.timers.len() / AssemblerSimd::LEN {
                let timer =
                    AssemblerSimd::from_slice(bb.timers.split_at_mut(i * AssemblerSimd::LEN).1);
                let ingredient_counts = AssemblerSimd::from_slice(
                    bb.ingredient_counts.split_at_mut(i * AssemblerSimd::LEN).1,
                );
                let product_counts = AssemblerSimd::from_slice(
                    bb.result_counts.split_at_mut(i * AssemblerSimd::LEN).1,
                );

                let recipe_ingredient_counts = AssemblerSimd::from_slice(
                    bb.recipe_ingredient_counts
                        .split_at_mut(i * AssemblerSimd::LEN)
                        .1,
                );
                let recipe_product_counts = AssemblerSimd::from_slice(
                    bb.recipe_result_counts
                        .split_at_mut(i * AssemblerSimd::LEN)
                        .1,
                );
                let stack_sizes = AssemblerSimd::from_slice(
                    bb.result_stack_sizes.split_at_mut(i * AssemblerSimd::LEN).1,
                );

                let recipe_timer = AssemblerSimd::from_slice(
                    bb.recipe_timers.split_at_mut(i * AssemblerSimd::LEN).1,
                );

                let timer_mask = timer.simd_eq(zero);
                let enough_ingredients_mask = ingredient_counts.simd_ge(recipe_ingredient_counts);
                let enough_space_mask = product_counts
                    .add(recipe_product_counts)
                    .simd_le(stack_sizes);

                let final_mask = timer_mask & enough_ingredients_mask & enough_space_mask;

                // Update the counts if all conditions pass
                final_mask
                    .select(
                        ingredient_counts.sub(recipe_ingredient_counts),
                        ingredient_counts,
                    )
                    .copy_to_slice(bb.ingredient_counts.split_at_mut(i * AssemblerSimd::LEN).1);

                final_mask
                    .select(product_counts.add(recipe_product_counts), product_counts)
                    .copy_to_slice(bb.result_counts.split_at_mut(i * AssemblerSimd::LEN).1);

                // Set the timer back up if all conditions passed and we created an item
                final_mask
                    .select(recipe_timer, timer)
                    .copy_to_slice(bb.timers.split_at_mut(i * AssemblerSimd::LEN).1);

                let timer =
                    AssemblerSimd::from_slice(bb.timers.split_at_mut(i * AssemblerSimd::LEN).1);

                // Decrease timer by one if it is larger than 0
                timer_mask
                    .select(timer, timer.sub(one))
                    .copy_to_slice(bb.timers.split_at_mut(i * AssemblerSimd::LEN).1);
            }
        });

        println!("Did {i} updates");
        println!(
            "First assembler now has {} items produced",
            assemblers.result_counts[0]
        );
    }

    #[allow(clippy::large_stack_frames)]
    #[bench]
    fn bench_1_000_000_assembler_update_packed_storage_static(b: &mut Bencher) {
        const LEN: usize = 1_000_000;
        type AssemblerSimd = Simd<u16, 16>;

        let zero = AssemblerSimd::splat(0);
        let one = AssemblerSimd::splat(1);

        let mut assemblers = MultiAssemblerStorageStatic::<LEN> {
            timers: Box::new([0; LEN]),
            ingredient_counts: Box::new([65535; LEN]),
            result_counts: Box::new([0; LEN]),
            recipe_timers: Box::new([9; LEN]),
            recipe_ingredient_counts: Box::new([1; LEN]),
            recipe_result_counts: Box::new([1; LEN]),
            result_stack_sizes: Box::new([2000; LEN]),
        };

        let mut i = 0;

        b.iter(|| {
            let bb = test::black_box(&mut assemblers);

            i += 1;
            for i in (0..LEN).step_by(AssemblerSimd::LEN) {
                let timer = AssemblerSimd::from_slice(bb.timers.split_at_mut(i).1);
                let ingredient_counts =
                    AssemblerSimd::from_slice(bb.ingredient_counts.split_at_mut(i).1);
                let product_counts = AssemblerSimd::from_slice(bb.result_counts.split_at_mut(i).1);

                let recipe_ingredient_counts =
                    AssemblerSimd::from_slice(bb.recipe_ingredient_counts.split_at_mut(i).1);
                let recipe_product_counts =
                    AssemblerSimd::from_slice(bb.recipe_result_counts.split_at_mut(i).1);
                let stack_sizes =
                    AssemblerSimd::from_slice(bb.result_stack_sizes.split_at_mut(i).1);

                let recipe_timer = AssemblerSimd::from_slice(bb.recipe_timers.split_at_mut(i).1);

                let timer_mask = timer.simd_eq(zero);
                let enough_ingredients_mask = ingredient_counts.simd_ge(recipe_ingredient_counts);
                let enough_space_mask = product_counts
                    .add(recipe_product_counts)
                    .simd_le(stack_sizes);

                let final_mask = timer_mask & enough_ingredients_mask & enough_space_mask;

                // Update the counts if all conditions pass
                final_mask
                    .select(
                        ingredient_counts.sub(recipe_ingredient_counts),
                        ingredient_counts,
                    )
                    .copy_to_slice(bb.ingredient_counts.split_at_mut(i).1);

                final_mask
                    .select(product_counts.add(recipe_product_counts), product_counts)
                    .copy_to_slice(bb.result_counts.split_at_mut(i).1);

                // Set the timer back up if all conditions passed and we created an item
                final_mask
                    .select(recipe_timer, timer)
                    .copy_to_slice(bb.timers.split_at_mut(i).1);

                let timer = AssemblerSimd::from_slice(bb.timers.split_at_mut(i).1);

                // Decrease timer by one if it is larger than 0
                timer_mask
                    .select(timer, timer.sub(one))
                    .copy_to_slice(bb.timers.split_at_mut(i).1);
            }
        });

        println!("Did {i} updates");
        println!(
            "First assembler now has {} items produced",
            assemblers.result_counts[0]
        );
    }

    #[bench]
    fn bench_1_000_000_assembler_update(b: &mut Bencher) {
        let mut assemblers = vec![];

        for _ in 0..1_000_000 {
            assemblers.push(Assembler::<Iron, Iron>::new(Recipe {
                ingredient: Item::Iron,
                ingredient_amount: 1,
                result: Item::Iron,
                result_amount: 1,
                time: 1,
            }));
        }

        for assembler in &assemblers {
            assembler
                .ingredient_storage
                .count
                .store(1_000_000_000, Ordering::Relaxed);
        }

        b.iter(|| {
            let bb = test::black_box(&mut assemblers);

            for assembler in bb {
                assembler.update();
            }
        });

        println!("{:?}", assemblers[0]);
    }
}
