use std::{cmp::min, num::NonZero};

use data::item::{Item, ItemCountType, ItemStack, max_stack_size};
use static_assertions::const_assert_eq;

pub type ItemStackIndex = u16;

#[derive(Debug, Clone, Copy)]
struct SushiSlot {
    content: Option<ItemStack>,
}

const_assert_eq!(
    std::mem::size_of::<SushiSlot>(),
    std::mem::size_of::<ItemStack>()
);

impl SushiSlot {
    fn is_full(self, stack_size_override: Option<NonZero<ItemCountType>>) -> bool {
        self.content.as_ref().is_some_and(|stack| {
            let max_stack_size: NonZero<ItemCountType> =
                stack_size_override.unwrap_or_else(|| max_stack_size(stack.item));
            stack.count == max_stack_size
        })
    }
}

#[derive(Debug, Clone)]
pub struct SushiChest {
    slots: Box<[SushiSlot]>,
    first_non_full_slot: ItemStackIndex,
    first_slot_with_all_empty_after: ItemStackIndex,
    automatic_insertion_slot_limit: ItemStackIndex,

    // Alternatively I could store the chest ty and look it up from that (by making floor chests a hardcoded ty)
    // That is probably better?
    stack_size_override: Option<NonZero<ItemCountType>>,
}

impl SushiChest {
    pub fn new(num_slots: ItemStackIndex) -> Self {
        Self {
            slots: vec![SushiSlot { content: None }; num_slots as usize].into_boxed_slice(),
            first_non_full_slot: 0,
            first_slot_with_all_empty_after: 0,
            automatic_insertion_slot_limit: num_slots,
            stack_size_override: None,
        }
    }

    fn assert_invariants(&self) {
        debug_assert!(
            self.slots[0..(self.first_non_full_slot as usize)]
                .iter()
                .all(|slot| { slot.is_full(self.stack_size_override) })
        );

        debug_assert!(
            self.slots[(self.first_slot_with_all_empty_after as usize)..]
                .iter()
                .all(|slot| { slot.content.is_none() })
        );
    }

    fn take_slot(&mut self, slot: ItemStackIndex) -> Option<ItemStack> {
        self.assert_invariants();

        let ret = Self::take_slot_raw(
            &mut self.slots[slot as usize],
            slot,
            &mut self.first_slot_with_all_empty_after,
            &mut self.first_non_full_slot,
            self.stack_size_override,
        );

        self.assert_invariants();

        ret
    }

    fn take_slot_raw(
        slot: &mut SushiSlot,
        slot_index: ItemStackIndex,
        first_slot_with_all_empty_after: &mut ItemStackIndex,
        first_non_full_slot: &mut ItemStackIndex,
        stack_size_override: Option<NonZero<ItemCountType>>,
    ) -> Option<ItemStack> {
        if slot_index + 1 == *first_slot_with_all_empty_after {
            *first_slot_with_all_empty_after = slot_index;
        }

        if slot.is_full(stack_size_override) && slot_index < *first_non_full_slot {
            *first_non_full_slot = slot_index;
        }

        slot.content.take()
    }

    pub fn try_add_item_stack(&mut self, mut items: ItemStack) -> Result<(), ItemStack> {
        self.assert_invariants();

        for (index, slot) in self.slots
            [(self.first_non_full_slot as usize)..(self.automatic_insertion_slot_limit as usize)]
            .iter_mut()
            .enumerate()
        {
            let max_stack_size: NonZero<ItemCountType> = self
                .stack_size_override
                .unwrap_or_else(|| max_stack_size(items.item));
            if let Some(stack) = &mut slot.content {
                if stack.item == items.item {
                    let taken: u16 = min(
                        u16::from(max_stack_size) - u16::from(stack.count),
                        items.count.into(),
                    );

                    stack.count = stack.count.saturating_add(taken);
                    if index as ItemStackIndex == self.first_non_full_slot
                        && stack.count == max_stack_size
                    {
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

                if index as ItemStackIndex == self.first_slot_with_all_empty_after {
                    // This slot is now no longer empty
                    self.first_slot_with_all_empty_after += 1;
                }

                if index as ItemStackIndex == self.first_non_full_slot
                    && items.count == max_stack_size
                {
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

    pub fn try_remove_item(
        &mut self,
        max_count: NonZero<u16>,
        filter: impl Fn(Item) -> bool,
    ) -> Option<ItemStack> {
        self.assert_invariants();

        let mut ret: Option<ItemStack> = None;

        for (index, slot) in self.slots[..(self.first_slot_with_all_empty_after as usize)]
            .iter_mut()
            .enumerate()
            // We search backwards
            .rev()
        {
            if let Some(stack) = &mut slot.content {
                if !(filter)(stack.item) {
                    continue;
                }

                if let Some(ret_stack) = &mut ret {
                    if ret_stack.item == stack.item {
                        let needed = u16::from(max_count) - u16::from(ret_stack.count);

                        if u16::from(stack.count) <= needed {
                            let Some(ItemStack { item: _, count }) = Self::take_slot_raw(
                                slot,
                                index as ItemStackIndex,
                                &mut self.first_slot_with_all_empty_after,
                                &mut self.first_non_full_slot,
                                self.stack_size_override,
                            ) else {
                                unreachable!()
                            };

                            ret_stack.count = ret_stack.count.saturating_add(count.into());

                            if ret_stack.count == max_count {
                                return ret;
                            }
                        } else {
                            // Take the needed amount from the stack
                            let max_stack_size: NonZero<ItemCountType> = self
                                .stack_size_override
                                .unwrap_or_else(|| max_stack_size(stack.item));
                            if stack.count == max_stack_size
                                && (index as ItemStackIndex) < self.first_non_full_slot
                            {
                                self.first_non_full_slot = index as ItemStackIndex;
                            }

                            stack.count = (u16::from(stack.count) - needed).try_into().expect("For this to be zero, stack.count must be equal to needed which is checked before");
                            ret_stack.count = ret_stack.count.saturating_add(needed);

                            return ret;
                        }
                    }
                } else if stack.count < max_count {
                    ret = Self::take_slot_raw(
                        slot,
                        index as ItemStackIndex,
                        &mut self.first_slot_with_all_empty_after,
                        &mut self.first_non_full_slot,
                        self.stack_size_override,
                    );
                } else if stack.count == max_count {
                    return Self::take_slot_raw(
                        slot,
                        index as ItemStackIndex,
                        &mut self.first_slot_with_all_empty_after,
                        &mut self.first_non_full_slot,
                        self.stack_size_override,
                    );
                } else {
                    // The stack in the slot is bigger than the count we want
                    if (index as ItemStackIndex) < self.first_non_full_slot {
                        // This slot is no longer full
                        self.first_non_full_slot = index as ItemStackIndex;
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

        let res = chest.try_remove_item(NonZero::new(100).expect("hardcoded"), |_| true);

        assert!(res.is_none());
    }

    proptest! {
        #[test]
        fn create_chest(slot_count in (0 as ItemStackIndex)..65_535) {
            let _chest = SushiChest::new(slot_count);
        }

        #[test]
        fn add_item(stack in random_item_stack(60_000)) {
            let mut chest = SushiChest::new(65_535);

            let res = chest.try_add_item_stack(stack);

            prop_assert!(res.is_ok());
        }

        #[test]
        fn add_remove_item(item in random_item(), add_count in (1 as ItemCountType)..6_000, remove_count in (1 as ItemCountType)..6_000) {
            let mut chest = SushiChest::new(65_535);

            let res = chest.try_add_item_stack(ItemStack { item, count: add_count.try_into().expect("range starts at 1") });

            prop_assert!(res.is_ok());

            let res = chest.try_remove_item(remove_count.try_into().expect("range starts at 1"), |found| found == item);

            prop_assert_eq!(res, Some(ItemStack { item, count: min(add_count, remove_count).try_into().expect("ranges start at 1") }));
        }
    }
}
