pub mod belt_belt_inserter;
pub mod belt_storage_inserter;
pub mod storage_storage_inserter;

const MOVETIME: u8 = 120;

// TODO: This could be minified using a union or similar,
// But since Inserters are the same sice, whether this is 2 or 1 byte (atleast in a Vec of Structs)
// I will leave this be for now.
#[derive(Debug, Clone, Copy)]
enum InserterState {
    Empty,
    FullAndWaitingForSlot,
    FullAndMovingOut(u8),
    EmptyAndMovingBack(u8),
}
