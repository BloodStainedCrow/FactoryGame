use crate::liquid::FluidSystemID;

pub const NO_TOKEN: FluidSystemID = FluidSystemID(u32::MAX);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FluidIndex(pub u32);

pub type FluidSlotType = u8;

pub struct SingleFluidSlice<'a> {
    pub current: &'a mut [FluidSlotType],
    pub max: &'a [FluidSlotType],
    pub tokens: &'a mut [FluidSystemID],
}
