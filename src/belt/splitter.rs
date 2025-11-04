use std::cmp::min;

use itertools::Itertools;

use crate::item::{IdxTrait, Item};

use super::{
    belt::{Belt, ItemInfo},
    MultiBeltStore, SushiBelt,
};

type BeltBeltInserterID = u32;

pub const SPLITTER_BELT_LEN: u16 = 2;

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub enum SplitterSide {
    Left,
    Right,
}

impl From<SplitterSide> for bool {
    fn from(value: SplitterSide) -> Self {
        match value {
            SplitterSide::Left => false,
            SplitterSide::Right => true,
        }
    }
}

impl From<SplitterSide> for usize {
    fn from(value: SplitterSide) -> Self {
        match value {
            SplitterSide::Left => 0,
            SplitterSide::Right => 1,
        }
    }
}

impl SplitterSide {
    fn switch(self) -> Self {
        match self {
            SplitterSide::Left => SplitterSide::Right,
            SplitterSide::Right => SplitterSide::Left,
        }
    }
}

#[derive(Debug, Clone, Copy, serde::Deserialize, serde::Serialize)]
pub enum SplitterDistributionMode {
    Fair { next: SplitterSide },
    Priority(SplitterSide),
    // TODO: Filter
}

impl Default for SplitterDistributionMode {
    fn default() -> Self {
        Self::Fair {
            next: SplitterSide::Left,
        }
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct Splitter {
    pub in_mode: SplitterDistributionMode,
    pub out_mode: SplitterDistributionMode,

    /// 0 is left
    pub input_belts: [usize; 2],
    /// 0 is left
    pub output_belts: [usize; 2],
}

impl Splitter {
    // TODO: Test this
    pub fn update<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        belts: &mut MultiBeltStore<ItemIdxType, RecipeIdxType>,
    ) {
        // FIXME: Handle the case where an input and output are the same belt!
        let [input_1, input_2, output_1, output_2] = belts
            .belts
            .get_disjoint_mut([
                self.input_belts[0],
                self.input_belts[1],
                self.output_belts[0],
                self.output_belts[1],
            ])
            .expect("Inputs or outputs overlap (or something is out of bounds)");
        let mut inputs: [&mut super::smart::SmartBelt<ItemIdxType, RecipeIdxType>; 2] =
            [input_1, input_2];
        let mut outputs = [output_1, output_2];

        let num_items_possible_to_input = inputs
            .iter()
            .filter(|belt| belt.get_front().is_some())
            .count();
        let num_items_possible_to_output = outputs
            .iter()
            .filter(|belt| belt.get_back().is_none())
            .count();

        let num_items = min(num_items_possible_to_input, num_items_possible_to_output);

        match num_items {
            0 => {},
            1 => {
                let should_switch_in = match self.in_mode {
                    SplitterDistributionMode::Fair { next } => next.into(),
                    SplitterDistributionMode::Priority(splitter_side) => splitter_side.into(),
                };

                if should_switch_in {
                    inputs.rotate_left(1);
                }

                let should_switch_out = match self.out_mode {
                    SplitterDistributionMode::Fair { next } => next.into(),
                    SplitterDistributionMode::Priority(splitter_side) => splitter_side.into(),
                };

                if should_switch_out {
                    outputs.rotate_left(1);
                }

                for (i, input) in inputs.iter_mut().enumerate() {
                    let old = input.get_front().is_some();
                    *input.get_front_mut() = false;
                    if old {
                        let original_index = (i + usize::from(should_switch_in)) % 2;
                        if let SplitterDistributionMode::Fair { next } = &mut self.in_mode {
                            if Into::<usize>::into(*next) == original_index {
                                *next = next.switch();
                            }
                        }
                        break;
                    }
                }

                for (i, output) in outputs.iter_mut().enumerate() {
                    let old = output.get_back().is_some();
                    *output.get_back_mut() = true;
                    if !old {
                        let original_index = (i + usize::from(should_switch_in)) % 2;
                        if let SplitterDistributionMode::Fair { next } = &mut self.out_mode {
                            if Into::<usize>::into(*next) == original_index {
                                *next = next.switch();
                            }
                        }
                        break;
                    }
                }
            },
            2 => {
                for input in inputs {
                    debug_assert_eq!(*input.get_front_mut(), true);
                    *input.get_front_mut() = false;
                }

                for output in outputs {
                    debug_assert_eq!(*output.get_back_mut(), false);
                    *output.get_back_mut() = true;
                }
            },
            _ => unreachable!("A Splitter can move at most 2 items every tick"),
        }
    }
}

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct SushiSplitter {
    pub in_mode: SplitterDistributionMode,
    pub out_mode: SplitterDistributionMode,

    /// 0 is left
    pub input_belts: [usize; 2],
    /// 0 is left
    pub output_belts: [usize; 2],
}

impl SushiSplitter {
    // TODO: Test this
    pub fn update<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        belts: &mut [SushiBelt<ItemIdxType, RecipeIdxType>],
    ) {
        // FIXME: Handle the case where an input and output are the same belt!
        let [input_1, input_2, output_1, output_2] = belts
            .get_disjoint_mut([
                self.input_belts[0],
                self.input_belts[1],
                self.output_belts[0],
                self.output_belts[1],
            ])
            .expect("Inputs or outputs overlap (or something is out of bounds)");
        let mut inputs: [&mut SushiBelt<ItemIdxType, RecipeIdxType>; 2] = [input_1, input_2];
        let mut outputs: [&mut SushiBelt<ItemIdxType, RecipeIdxType>; 2] = [output_1, output_2];

        let num_items_possible_to_input = inputs
            .iter()
            .filter(|belt| belt.get_front().is_some())
            .count();
        let num_items_possible_to_output = outputs
            .iter()
            .filter(|belt| belt.get_back().is_none())
            .count();

        let num_items = min(num_items_possible_to_input, num_items_possible_to_output);

        match num_items {
            0 => {},
            1 => {
                let should_switch_in = match self.in_mode {
                    SplitterDistributionMode::Fair { next } => next.into(),
                    SplitterDistributionMode::Priority(splitter_side) => splitter_side.into(),
                };

                if should_switch_in {
                    inputs.rotate_left(1);
                }

                let should_switch_out = match self.out_mode {
                    SplitterDistributionMode::Fair { next } => next.into(),
                    SplitterDistributionMode::Priority(splitter_side) => splitter_side.into(),
                };

                if should_switch_out {
                    outputs.rotate_left(1);
                }

                #[cfg(debug_assertions)]
                let mut removed = false;

                let items: [Item<ItemIdxType>; 1] = inputs
                    .iter_mut()
                    .enumerate()
                    .flat_map(|(i, input)| {
                        if input.get_front().is_some() {
                            let original_index = (i + usize::from(should_switch_in)) % 2;
                            if let SplitterDistributionMode::Fair { next } = &mut self.in_mode {
                                if Into::<usize>::into(*next) == original_index {
                                    *next = next.switch();
                                }
                            }
                        }

                        #[cfg(debug_assertions)]
                        {
                            assert!(!removed);
                            removed = true;
                        }
                        input.remove_item(0)
                    })
                    .take(1)
                    .map(|info| {
                        let ItemInfo::Sushi(item) = info else {
                            unreachable!()
                        };
                        item
                    })
                    .collect_array()
                    .expect("We already checked that has to be at least one item");

                for (i, output) in outputs.iter_mut().enumerate() {
                    if !output.get_back().is_some() {
                        // There is space here
                        output.try_insert_item(output.get_len() - 1, items[0]);
                        let original_index = (i + usize::from(should_switch_in)) % 2;
                        if let SplitterDistributionMode::Fair { next } = &mut self.out_mode {
                            if Into::<usize>::into(*next) == original_index {
                                *next = next.switch();
                            }
                        }
                        break;
                    }
                }
            },
            2 => {
                let items: [Item<ItemIdxType>; 2] = inputs
                    .iter_mut()
                    .enumerate()
                    .map(|(i, input)| input.remove_item(0).unwrap())
                    .map(|info| {
                        let ItemInfo::Sushi(item) = info else {
                            unreachable!()
                        };
                        item
                    })
                    .collect_array()
                    .expect("We already checked that has to be 2 items to pick up");

                // TODO: How do I want to distribute these items?
                // Currently if both input lines are full the items are not shuffled.

                for (output, item) in outputs.iter_mut().zip_eq(items) {
                    debug_assert!(output.get_back().is_none());
                    output
                        .try_insert_item(output.get_len() - 1, item)
                        .expect("There should be space on the belt!");
                }
            },
            _ => unreachable!("A Splitter can move at most 2 items every tick"),
        }
    }
}
