use crate::liquid::FluidSystemID;

pub const NO_TOKEN: FluidSystemID = FluidSystemID(u32::MAX);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FluidIndex(pub u32);

pub type FluidSlotType = u8;

pub struct SingleFluidSlice<'a> {
    current: &'a mut [FluidSlotType],
    max: &'a [FluidSlotType],
    tokens: &'a mut [FluidSystemID],
}

impl SingleFluidSlice<'_> {
    pub fn index(
        &mut self,
        index: FluidIndex,
    ) -> (&mut FluidSlotType, &FluidSlotType, &mut FluidSystemID) {
        (
            &mut self.current[index.0 as usize],
            &self.max[index.0 as usize],
            &mut self.tokens[index.0 as usize],
        )
    }
}
