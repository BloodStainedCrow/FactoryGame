use std::cmp::min;

use crate::slot_arenas::fluid_arena::{FluidIndex, FluidSlotType, NO_TOKEN, SingleFluidSlice};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FluidSystemID(pub(crate) u32);

pub type FluidNetworkStorageType = u32;

// TODO: We might want some more SOA stuff here
pub struct FluidNetwork {
    // TODO: Maybe u64? Needs benchmarks
    current_fluid: FluidNetworkStorageType,
    max_fluid: FluidNetworkStorageType,

    // TODO(BSC): Benchmark if switching this to a VecDeque is better (since that should avoid requiring to copy the entire tail)
    input_tokens: Vec<FluidIndex>,
    output_tokens: Vec<FluidIndex>,
}

pub struct FluidNetworkInfo {
    current_fluid: FluidNetworkStorageType,
    max_fluid: FluidNetworkStorageType,
}

impl FluidNetwork {
    // TODO: Can I think of a scheme which avoids a early return update for idle FluidNetworks?
    pub fn update<'a, 'b: 'a>(
        &mut self,
        self_index: FluidSystemID,
        slice: &'b mut SingleFluidSlice<'a>,
    ) {
        // Inputs first
        let mut end_index = 0;

        for token in &self.input_tokens {
            if self.current_fluid == self.max_fluid {
                break;
            }

            let (current, _max, token_slot) = slice.index(*token);

            debug_assert!(
                *current > 0,
                "If a fluid token is in the fluid networks update list, that means we can make progress"
            );

            let space_remaining = self.max_fluid - self.current_fluid;

            let to_move: FluidSlotType = min(
                *current,
                space_remaining.try_into().unwrap_or(FluidSlotType::MAX),
            );

            let fulfilled = to_move == *current;

            self.current_fluid += FluidNetworkStorageType::from(to_move);
            *current -= to_move;

            if fulfilled {
                debug_assert!(*current == 0);
                // FIXME: This assumes that each fluid slot can only be accessed by a single fluid network
                debug_assert!(*token_slot == NO_TOKEN);
                *token_slot = self_index;
                end_index += 1;
            } else {
                break;
            }
        }

        self.input_tokens.drain(0..end_index);

        let mut end_index = 0;

        for token in &self.output_tokens {
            if self.current_fluid == 0 {
                break;
            }

            let (current, max, token_slot) = slice.index(*token);

            let space_remaining = *max - *current;

            debug_assert!(
                space_remaining > 0,
                "If a fluid token is in the fluid networks update list, that means we can make progress"
            );

            let to_move: FluidSlotType = min(
                space_remaining,
                self.current_fluid.try_into().unwrap_or(FluidSlotType::MAX),
            );

            let fulfilled = to_move == space_remaining;

            self.current_fluid -= FluidNetworkStorageType::from(to_move);
            *current += to_move;

            if fulfilled {
                debug_assert!(*current == *max);
                // FIXME: This assumes that each fluid slot can only be accessed by a single fluid network
                debug_assert!(*token_slot == NO_TOKEN);
                *token_slot = self_index;
                end_index += 1;
            } else {
                break;
            }
        }

        self.output_tokens.drain(0..end_index);
    }

    pub const fn get_info(&self) -> FluidNetworkInfo {
        FluidNetworkInfo {
            current_fluid: self.current_fluid,
            max_fluid: self.max_fluid,
        }
    }

    pub fn add_input_token(&mut self, index: FluidIndex) {
        self.input_tokens.push(index);
    }

    pub fn try_remove_input_token(&mut self, index: FluidIndex) -> Result<FluidIndex, ()> {
        let position = self.input_tokens.iter().position(|v| *v == index);

        if let Some(position) = position {
            // TODO: Do I want to use swap_remove here?
            // Doing so would be better for performance, but might break the round robin strategy. But only when the player does actively change the factory, so that might be fine
            Ok(self.input_tokens.remove(position))
        } else {
            Err(())
        }
    }

    pub fn add_output_token(&mut self, index: FluidIndex) {
        self.output_tokens.push(index);
    }

    pub fn try_remove_output_token(&mut self, index: FluidIndex) -> Result<FluidIndex, ()> {
        let position = self.output_tokens.iter().position(|v| *v == index);

        if let Some(position) = position {
            // TODO: Do I want to use swap_remove here?
            // Doing so would be better for performance, but might break the round robin strategy. But only when the player does actively change the factory, so that might be fine
            Ok(self.output_tokens.remove(position))
        } else {
            Err(())
        }
    }
}
