use std::sync::{atomic::Ordering, Arc};

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
    fn bench_multi_assembler_update(b: &mut Bencher) {
        let mut assemblers = [
            Assembler::<Iron, Iron>::new(Recipe {
                ingredient: Item::Iron,
                ingredient_amount: 1,
                result: Item::Iron,
                result_amount: 1,
                time: 1,
            }),
            Assembler::<Iron, Iron>::new(Recipe {
                ingredient: Item::Iron,
                ingredient_amount: 1,
                result: Item::Iron,
                result_amount: 1,
                time: 1,
            }),
        ];

        for assembler in &assemblers {
            assembler
                .ingredient_storage
                .count
                .store(1_000_000_000, Ordering::Relaxed);
        }

        b.iter(|| {
            // let bb = test::black_box(&mut assemblers);

            for _ in 0..(1_000 / assemblers.len()) {
                for assembler in &mut assemblers {
                    assembler.update();
                }
            }
            // bb.result_count.store(0, Ordering::Relaxed);
        });

        for assembler in assemblers {
            println!("{assembler:?}");
        }
    }
}
