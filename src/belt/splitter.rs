use std::cmp::min;

use std::cell::UnsafeCell;

use itertools::Itertools;
use serde::ser::SerializeSeq;

use crate::item::{IdxTrait, Item, WeakIdxTrait};

use strum::EnumIter;

pub const SPLITTER_BELT_LEN: u16 = 2;

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize, serde::Serialize, EnumIter)]
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
pub struct PureSplitter {
    pub in_mode: SplitterDistributionMode,
    pub out_mode: SplitterDistributionMode,

    /// 0 is left
    pub inputs: [bool; 2],
    /// 0 is left
    pub outputs: [bool; 2],
}

impl PureSplitter {
    // TODO: Test this
    pub fn update(&mut self) {
        let num_items_possible_to_input = self.inputs.iter().filter(|belt| **belt).count();
        let num_items_possible_to_output = self.outputs.iter().filter(|belt| !**belt).count();

        let num_items = min(num_items_possible_to_input, num_items_possible_to_output);

        match num_items {
            0 => {},
            1 => {
                let should_switch_in = match self.in_mode {
                    SplitterDistributionMode::Fair { next } => next.into(),
                    SplitterDistributionMode::Priority(splitter_side) => splitter_side.into(),
                };

                if should_switch_in {
                    self.inputs.rotate_left(1);
                }

                let should_switch_out: bool = match self.out_mode {
                    SplitterDistributionMode::Fair { next } => next.into(),
                    SplitterDistributionMode::Priority(splitter_side) => splitter_side.into(),
                };

                let mut outputs = self.outputs.each_mut();

                if should_switch_out {
                    outputs.rotate_left(1);
                }

                for (i, input) in self.inputs.iter_mut().enumerate() {
                    let old = *input;
                    *input = false;
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
                    let old = **output;
                    **output = true;
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
                self.inputs = [false, false];
                self.outputs = [true, true];
            },
            _ => unreachable!("A Splitter can move at most 2 items every tick"),
        }
    }
}

#[derive(Debug)]
pub(super) struct SushiSplitter<ItemIdxType: WeakIdxTrait> {
    pub in_mode: SplitterDistributionMode,
    pub out_mode: SplitterDistributionMode,

    /// 0 is left
    pub(super) inputs: [UnsafeCell<Option<Item<ItemIdxType>>>; 2],
    /// 0 is left
    pub(super) outputs: [UnsafeCell<Option<Item<ItemIdxType>>>; 2],
}

// SAFETY:
// Since all accesses to the UnsafeCells are tightly controlled inside the super module, we can share this between threads
unsafe impl<ItemIdxType: WeakIdxTrait + Sync> Sync for SushiSplitter<ItemIdxType> {}

#[derive(serde::Deserialize, serde::Serialize)]
struct SushiSplitterReplacement<ItemIdxType: WeakIdxTrait> {
    pub in_mode: SplitterDistributionMode,
    pub out_mode: SplitterDistributionMode,

    /// 0 is left
    pub inputs: [Option<Item<ItemIdxType>>; 2],
    /// 0 is left
    pub outputs: [Option<Item<ItemIdxType>>; 2],
}

impl<'de, ItemIdxType: WeakIdxTrait> serde::Deserialize<'de> for SushiSplitter<ItemIdxType>
where
    ItemIdxType: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        SushiSplitterReplacement::deserialize(deserializer).map(
            |SushiSplitterReplacement {
                 in_mode,
                 out_mode,
                 inputs,
                 outputs,
             }| SushiSplitter {
                in_mode,
                out_mode,
                inputs: inputs.map(|v| UnsafeCell::new(v)),
                outputs: outputs.map(|v| UnsafeCell::new(v)),
            },
        )
    }
}

impl<ItemIdxType: WeakIdxTrait> SushiSplitter<ItemIdxType> {
    /// The caller must ensure, reads from self cannot race on the unsafe cell (i.e. that this is not called during a belt update)
    pub unsafe fn unsafe_clone(&self) -> Self {
        // SAFETY:
        // This is safe, since the caller is responsible
        unsafe {
            Self {
                in_mode: self.in_mode.clone(),
                out_mode: self.out_mode.clone(),
                inputs: [
                    UnsafeCell::new(*self.inputs[0].get()),
                    UnsafeCell::new(*self.inputs[1].get()),
                ],
                outputs: [
                    UnsafeCell::new(*self.outputs[0].get()),
                    UnsafeCell::new(*self.outputs[1].get()),
                ],
            }
        }
    }

    /// The caller must ensure, reads from self cannot race on the unsafe cell (i.e. that this is not called during a belt update)
    pub unsafe fn unsafe_serialize_elem<S>(
        &self,
        seq: &mut <S as serde::Serializer>::SerializeSeq,
    ) -> Result<(), S::Error>
    where
        S: serde::Serializer,
        ItemIdxType: serde::Serialize,
    {
        // SAFETY:
        // This is safe, since the caller is responsible
        unsafe {
            seq.serialize_element(&SushiSplitterReplacement {
                in_mode: self.in_mode,
                out_mode: self.out_mode,
                inputs: [*self.inputs[0].get(), *self.inputs[1].get()],
                outputs: [*self.outputs[0].get(), *self.outputs[1].get()],
            })
        }
    }
}

impl<ItemIdxType: IdxTrait> SushiSplitter<ItemIdxType> {
    // TODO: Test this
    pub fn update(&mut self) {
        let num_items_possible_to_input = self
            .inputs
            .iter_mut()
            .filter_map(|i| i.get_mut().is_some().then_some(()))
            .count();
        let num_items_possible_to_output = self
            .outputs
            .iter_mut()
            .filter_map(|i| i.get_mut().is_none().then_some(()))
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
                    self.inputs.rotate_left(1);
                }

                let should_switch_out = match self.out_mode {
                    SplitterDistributionMode::Fair { next } => next.into(),
                    SplitterDistributionMode::Priority(splitter_side) => splitter_side.into(),
                };

                #[cfg(debug_assertions)]
                let mut removed = false;

                let items: [Item<ItemIdxType>; 1] = self
                    .inputs
                    .iter_mut()
                    .enumerate()
                    .flat_map(|(i, input)| {
                        if input.get_mut().is_some() {
                            let original_index = (i + usize::from(should_switch_in)) % 2;
                            if let SplitterDistributionMode::Fair { next } = &mut self.in_mode {
                                if Into::<usize>::into(*next) == original_index {
                                    *next = next.switch();
                                }
                            }
                            #[cfg(debug_assertions)]
                            {
                                assert!(!removed);
                                removed = true;
                            }
                        }

                        input.get_mut().take()
                    })
                    .take(1)
                    .collect_array()
                    .expect("We already checked that has to be at least one item");

                let mut outputs = self.outputs.each_mut();

                if should_switch_out {
                    outputs.rotate_left(1);
                }

                for (i, output) in outputs.iter_mut().enumerate() {
                    if output.get_mut().is_none() {
                        // There is space here
                        *output.get_mut() = Some(items[0]);
                        let original_index = (i + usize::from(should_switch_out)) % 2;
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
                let items: [Item<ItemIdxType>; 2] = self
                    .inputs
                    .iter_mut()
                    .map(|input| input.get_mut().take().unwrap())
                    .collect_array()
                    .expect("We already checked that has to be 2 items to pick up");

                // TODO: How do I want to distribute these items?
                // Currently if both input lines are full the items are not shuffled.

                for (output, item) in self.outputs.iter_mut().zip_eq(items) {
                    debug_assert!(output.get_mut().is_none());
                    *output.get_mut() = Some(item);
                }
            },
            _ => unreachable!("A Splitter can move at most 2 items every tick"),
        }
    }
}
