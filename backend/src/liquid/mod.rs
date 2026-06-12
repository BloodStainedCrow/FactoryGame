use std::{cmp::min, iter};

use crate::slot_arenas::fluid_arena::{FluidIndex, NO_TOKEN, SingleFluidSlice};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FluidSystemID(pub u32);

// TODO: We might want some more SOA stuff here
pub struct FluidNetwork {
    // TODO: Maybe u64?
    current_fluid: u32,
    max_fluid: u32,

    // TODO(BSC): Benchmark if switching this to a VecDeque is better (since that should avoid requiring to copy the entire tail)
    input_tokens: Vec<FluidIndex>,
    output_tokens: Vec<FluidIndex>,
}

pub struct FluidNetworkInfo {
    current_fluid: u32,
    max_fluid: u32,
}

impl FluidNetwork {
    // TODO: Can I think of a scheme which avoids a early return update for idle FluidNetworks?
    pub fn update(&mut self, self_index: FluidSystemID, slice: &mut SingleFluidSlice) {
        // Inputs first
        let mut end_index = 0;

        for token in &self.input_tokens {
            if self.current_fluid == self.max_fluid {
                break;
            }

            debug_assert!(
                slice.current[token.0 as usize] > 0,
                "If a fluid token is in the fluid networks update list, that means we can make progress"
            );

            let space_remaining = self.max_fluid - self.current_fluid;

            let to_move: u8 = min(
                slice.current[token.0 as usize],
                space_remaining.try_into().unwrap_or(u8::MAX),
            );

            let fulfilled = to_move == slice.current[token.0 as usize];

            self.current_fluid += u32::from(to_move);
            slice.current[token.0 as usize] -= to_move;

            if fulfilled {
                end_index += 1;
            } else {
                break;
            }
        }

        for done_token in self.input_tokens.splice(0..end_index, iter::empty()) {
            // FIXME: This assumes that each fluid slot can only be accessed by a single fluid network
            debug_assert!(slice.tokens[done_token.0 as usize] == NO_TOKEN);
            slice.tokens[done_token.0 as usize] = self_index;
        }

        let mut end_index = 0;

        for token in &self.output_tokens {
            if self.current_fluid == 0 {
                break;
            }

            let space_remaining = slice.max[token.0 as usize] - slice.current[token.0 as usize];

            debug_assert!(
                space_remaining > 0,
                "If a fluid token is in the fluid networks update list, that means we can make progress"
            );

            let to_move: u8 = min(
                space_remaining,
                self.current_fluid.try_into().unwrap_or(u8::MAX),
            );

            let fulfilled = to_move == space_remaining;

            self.current_fluid -= u32::from(to_move);
            slice.current[token.0 as usize] += to_move;

            if fulfilled {
                end_index += 1;
            } else {
                break;
            }
        }

        for done_token in self.output_tokens.splice(0..end_index, iter::empty()) {
            // FIXME: This assumes that each fluid slot can only be accessed by a single fluid network
            debug_assert!(slice.tokens[done_token.0 as usize] == NO_TOKEN);
            slice.tokens[done_token.0 as usize] = self_index;
        }
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
            Ok(self.output_tokens.remove(position))
        } else {
            Err(())
        }
    }
}
