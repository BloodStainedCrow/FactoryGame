#![expect(clippy::cast_possible_truncation)]

use std::{cmp::min, num::NonZero};

use data::item::ItemStack;
use static_assertions::const_assert_eq;

#[derive(Debug, Clone, Copy)]
pub struct SushiSlot {
    content: Option<ItemStack>,
}

const_assert_eq!(
    std::mem::size_of::<SushiSlot>(),
    std::mem::size_of::<ItemStack>()
);

impl SushiSlot {
    fn is_full(self) -> bool {
        self.content.as_ref().is_some_and(|stack| {
            // TODO
            let max_stack_size: NonZero<u16> = NonZero::new(2).unwrap();
            stack.count == max_stack_size
        })
    }
}

#[derive(Debug)]
pub struct SushiChest {
    slots: Box<[SushiSlot]>,
    first_non_full_slot: u32,
    first_slot_with_all_empty_after: u32,
    // TODO: Stack limit override?
}

impl SushiChest {
    pub fn new(num_slots: u32) -> Self {
        Self {
            slots: vec![SushiSlot { content: None }; num_slots as usize].into_boxed_slice(),
            first_non_full_slot: 0,
            first_slot_with_all_empty_after: 0,
        }
    }

    fn assert_invariants(&self) {
        debug_assert!(
            self.slots[0..(self.first_non_full_slot as usize)]
                .iter()
                .all(|slot| { slot.is_full() })
        );

        debug_assert!(
            self.slots[(self.first_slot_with_all_empty_after as usize)..]
                .iter()
                .all(|slot| { slot.content.is_none() })
        );
    }

    fn take_slot(&mut self, slot: u32) -> Option<ItemStack> {
        let ret = Self::take_slot_raw(
            &mut self.slots[slot as usize],
            slot,
            &mut self.first_slot_with_all_empty_after,
            &mut self.first_non_full_slot,
        );

        self.assert_invariants();

        ret
    }

    fn take_slot_raw(
        slot: &mut SushiSlot,
        slot_index: u32,
        first_slot_with_all_empty_after: &mut u32,
        first_non_full_slot: &mut u32,
    ) -> Option<ItemStack> {
        if slot_index + 1 == *first_slot_with_all_empty_after {
            *first_slot_with_all_empty_after = slot_index;
        }

        if slot.is_full() && slot_index < *first_non_full_slot {
            *first_non_full_slot = slot_index;
        }

        slot.content.take()
    }

    pub fn try_add_item_stack(&mut self, mut items: ItemStack) -> Result<(), ItemStack> {
        for (index, slot) in self.slots[(self.first_non_full_slot as usize)..]
            .iter_mut()
            .enumerate()
        {
            // TODO
            let max_stack_size: NonZero<u16> = NonZero::new(2).unwrap();
            if let Some(stack) = &mut slot.content {
                if stack.item == items.item {
                    let taken: u16 = min(
                        u16::from(max_stack_size) - u16::from(stack.count),
                        items.count.into(),
                    );

                    stack.count = stack.count.saturating_add(taken);
                    if index as u32 == self.first_non_full_slot && stack.count == max_stack_size {
                        // This slot is now full
                        self.first_non_full_slot += 1;
                    }

                    if taken == items.count.into() {
                        return Ok(());
                    }
                    items.count = NonZero::try_from(u16::from(items.count) - taken)
                        .expect("Value should not be 0");
                }
            } else {
                let taken = min(max_stack_size, items.count);

                slot.content = Some(ItemStack {
                    item: items.item,
                    count: taken,
                });

                if index as u32 == self.first_slot_with_all_empty_after {
                    // This slot is now no longer empty
                    self.first_slot_with_all_empty_after += 1;
                }

                if index as u32 == self.first_non_full_slot && items.count == max_stack_size {
                    // This slot is now full
                    self.first_non_full_slot += 1;
                }

                if taken == items.count {
                    return Ok(());
                }
                items.count = NonZero::try_from(u16::from(items.count) - u16::from(taken))
                    .expect("Value should not be 0");
            }
        }

        self.assert_invariants();

        Err(items)
    }

    pub fn try_remove_any_item(&mut self, max_count: NonZero<u16>) -> Option<ItemStack> {
        let mut ret: Option<ItemStack> = None;

        for (index, slot) in self.slots[..(self.first_slot_with_all_empty_after as usize)]
            .iter_mut()
            .enumerate()
            // We search backwards
            .rev()
        {
            if let Some(stack) = &mut slot.content {
                if let Some(ret_stack) = &mut ret {
                    if ret_stack.item == stack.item {
                        let needed = u16::from(max_count) - u16::from(ret_stack.count);

                        if u16::from(stack.count) <= needed {
                            let Some(ItemStack { item: _, count }) = Self::take_slot_raw(
                                slot,
                                index as u32,
                                &mut self.first_slot_with_all_empty_after,
                                &mut self.first_non_full_slot,
                            ) else {
                                unreachable!()
                            };

                            ret_stack.count = ret_stack.count.saturating_add(count.into());

                            if ret_stack.count == max_count {
                                return ret;
                            }
                        } else {
                            // Take the needed amount from the stack
                            // TODO:
                            let max_stack_size: NonZero<u16> = NonZero::new(2).unwrap();
                            if stack.count == max_stack_size
                                && (index as u32) < self.first_non_full_slot
                            {
                                self.first_non_full_slot = index as u32;
                            }

                            stack.count = (u16::from(stack.count) - needed).try_into().expect("For this to be zero, stack.count must be equal to needed which is checked before");
                            ret_stack.count = ret_stack.count.saturating_add(needed);

                            return ret;
                        }
                    }
                } else if stack.count < max_count {
                    ret = Self::take_slot_raw(
                        slot,
                        index as u32,
                        &mut self.first_slot_with_all_empty_after,
                        &mut self.first_non_full_slot,
                    );
                } else if stack.count == max_count {
                    return Self::take_slot_raw(
                        slot,
                        index as u32,
                        &mut self.first_slot_with_all_empty_after,
                        &mut self.first_non_full_slot,
                    );
                } else {
                    // The stack in the slot is bigger than the count we want
                    if (index as u32) < self.first_non_full_slot {
                        // This slot is no longer full
                        self.first_non_full_slot = index as u32;
                    }

                    stack.count =
                        NonZero::new(u16::from(stack.count) - u16::from(max_count)).expect("The stack in the slot is bigger than the count we want, so this should not fail");

                    return Some(ItemStack {
                        item: stack.item,
                        count: max_count,
                    });
                }
            }
        }

        self.assert_invariants();

        ret
    }
}

#[cfg(test)]
mod test {
    use proptest::{prop_assert, prop_assert_eq, proptest};

    use super::*;

    use data::item::strategies::{random_item, random_item_stack};

    #[test]
    fn remove_item_on_empty_chest() {
        let mut chest = SushiChest::new(65_535);

        let res = chest.try_remove_any_item(NonZero::new(100).expect("hardcoded"));

        assert!(res.is_none());
    }

    proptest! {
        #[test]
        fn create_chest(slot_count in 0u32..65_535) {
            let _chest = SushiChest::new(slot_count);
        }

        #[test]
        fn add_item(stack in random_item_stack(60_000)) {
            let mut chest = SushiChest::new(65_535);

            let res = chest.try_add_item_stack(stack);

            prop_assert!(res.is_ok());
        }

        #[test]
        fn add_remove_item(item in random_item(), add_count in 1u16..6_000, remove_count in 1u16..6_000) {
            let mut chest = SushiChest::new(65_535);

            let res = chest.try_add_item_stack(ItemStack { item, count: add_count.try_into().expect("range starts at 1") });

            prop_assert!(res.is_ok());

            let res = chest.try_remove_any_item(remove_count.try_into().expect("range starts at 1"));

            prop_assert_eq!(res, Some(ItemStack { item, count: min(add_count, remove_count).try_into().expect("ranges start at 1") }));
        }
    }
}
