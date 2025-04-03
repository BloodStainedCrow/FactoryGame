use std::cmp::min;

use crate::item::IdxTrait;

use super::{belt::Belt, MultiBeltStore};

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
            .get_many_mut([
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
            _ => unreachable!(),
        }
    }
}
