#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AssemblerIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InserterIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BeltIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ChestIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PipeIndex(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PowerPoleIndex(pub u32);
