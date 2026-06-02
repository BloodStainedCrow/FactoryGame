use crate::interface::{
    backend::StaticProducerType,
    middle::{MiddleAssemblerID, MiddleTransportLineID, TransportLineLength},
};

struct Position {
    x: u32,
    y: u32,
}

struct AreaSize {
    width: u32,
    height: u32,
}

struct Offset {
    x: i32,
    y: i32,
}

struct BoundingBox {
    pos: Position,
    size: AreaSize,
}

struct Rotation;

struct BeltType;
struct BeltTileID;

// TODO: Do I want to support belts with more than 2 lanes
// TODO: Do I want to support belts with more than 1 lane at all??
struct BeltTileInfo {
    left: (MiddleTransportLineID, TransportLineLength),
    right: (MiddleTransportLineID, TransportLineLength),
}

trait Frontend: TileFrontend + BeltLogicFrontend {}

// Its important that this does not capture any lifetime!
// Otherwise I cannot do any RAM optimizations anywhere
struct EntityDescription {
    position: Position,
    ty: StaticProducerType,
    rotation: Rotation,
    kind: EntityKindDescription,
}

enum EntityKindDescription {
    StaticPowerProducer {},
    Accumulator {},
    Belt { id: BeltTileID },
    Assembler { id: MiddleAssemblerID },
    // etc
}

struct ChunkRenderInfo;

// TODO: Decide on a Chunk size: https://github.com/BloodStainedCrow/FactoryGame/issues/36.
trait TileFrontend {
    fn has_space(bounding_box: BoundingBox) -> bool;

    fn get_chunks_for_viewing_area(
        viewing_area: BoundingBox,
    ) -> impl Iterator<Item = ChunkRenderInfo>;

    fn get_entity_at(position: Position) -> Option<EntityDescription>;

    // The caller must ensure there is space
    // The caller must handle any interactions which occur due to this entity being added
    // This just adds it to the chunk
    fn add_entity_raw(entity: EntityDescription);

    // The caller must handle any interactions which occur due to this entity being removed
    fn remove_entity_raw(position: Position) -> Option<EntityDescription>;
    // The caller must handle any interactions which occur due to these entities being removed
    fn remove_entities_overlapping_raw(
        bounding_box: BoundingBox,
    ) -> impl Iterator<Item = EntityDescription>;

    // TODO: Some interaction stuff for blueprint and stuff
    // TODO: Floor tiles
    // TODO: placement rules beside overlap (for stuff like water pumps, rails etc)
}

trait BeltLogicFrontend: TileFrontend {
    fn add_belt_tile(position: Position, rotation: Rotation, ty: BeltType) -> !;
    fn remove_belt_tile(position: Position);
}
