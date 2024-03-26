use std::sync::atomic::{AtomicU64, Ordering};

use crate::item::{get_max_stack_size, Recipe};

#[derive(Debug)]
pub struct Assembler<'a> {
    pub recipe: Recipe,
    pub timer: u16,
    pub ingredient_count: &'a AtomicU64,
    pub result_count: &'a AtomicU64,
}

impl Assembler<'_> {
    #[must_use]
    pub fn new(recipe: Recipe) -> Self {
        Self {
            recipe,
            timer: recipe.time,
            ingredient_count: Box::leak(Box::new(AtomicU64::new(20_000_000_000))),
            result_count: Box::leak(Box::new(AtomicU64::new(0))),
        }
    }

    pub fn update(&mut self) {
        if self.timer == 0 {
            if self.ingredient_count.load(Ordering::Relaxed) >= self.recipe.ingredient_amount {
                if self.result_count.load(Ordering::Relaxed)
                    <= get_max_stack_size(self.recipe.result) - self.recipe.result_amount
                {
                    self.ingredient_count
                        .fetch_sub(self.recipe.ingredient_amount, Ordering::Relaxed);
                    self.result_count
                        .fetch_add(self.recipe.result_amount, Ordering::Relaxed);
                }

                self.timer = self.recipe.time - 1;
            }
        } else {
            self.timer -= 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::item::{all_recipes, Item};

    use super::*;
    extern crate test;
    use proptest::proptest;
    use test::Bencher;

    proptest! {
        #[test]
        fn test_constructing_assembler_does_not_panic(recipe in all_recipes()) {
            let _ = Assembler::new(recipe);
        }
    }

    #[bench]
    fn bench_assembler_update(b: &mut Bencher) {
        let mut assembler = Assembler::new(Recipe {
            ingredient: Item::Iron,
            ingredient_amount: 1,
            result: Item::Iron,
            result_amount: 2,
            time: 5,
        });

        b.iter(|| {
            let bb = test::black_box(&mut assembler);

            for _ in 0..5_000 {
                bb.update();
            }
            // bb.count.store(0, Ordering::Relaxed);
        });

        println!("{assembler:?}");
    }
}
