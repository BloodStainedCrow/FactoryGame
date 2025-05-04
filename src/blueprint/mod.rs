use std::ops::Range;

use proptest::{
    prelude::{prop, Just, Strategy},
    prop_oneof,
};

use crate::{
    data::DataStore,
    frontend::{
        action::{
            place_entity::{EntityPlaceOptions, PlaceEntityInfo},
            place_tile::{PlaceFloorTileByHandInfo, PlaceFloorTileGhostInfo, PositionInfo},
            set_recipe::SetRecipeInfo,
            ActionType,
        },
        world::{
            tile::{Dir, FloorTile, PlaceEntityType},
            Position,
        },
    },
    item::{IdxTrait, Item, Recipe, WeakIdxTrait},
    rendering::app_state::GameState,
    replays::Replay,
};

// For now blueprint will just be a list of actions
#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct Blueprint<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    actions: Vec<ActionType<ItemIdxType, RecipeIdxType>>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Blueprint<ItemIdxType, RecipeIdxType> {
    pub fn apply(
        &self,
        base_pos: Position,
        game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        // FIXME: currently base_pos is unused!
        game_state.apply_actions(&self.actions, data_store);
    }

    pub fn from_replay<DS: AsRef<DataStore<ItemIdxType, RecipeIdxType>>>(
        replay: &Replay<ItemIdxType, RecipeIdxType, DS>,
    ) -> Self {
        Self {
            actions: replay.actions.iter().map(|ra| ra.action.clone()).collect(),
        }
    }
}

pub fn random_blueprint_strategy<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    len_range: Range<usize>,
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> impl Strategy<Value = Blueprint<ItemIdxType, RecipeIdxType>> {
    prop::collection::vec(random_action(data_store), len_range)
        .prop_map(|actions| Blueprint { actions })
}

pub fn random_action<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> impl Strategy<Value = ActionType<ItemIdxType, RecipeIdxType>> {
    prop_oneof![
        random_position().prop_map(|pos| ActionType::Ping(pos)),
        random_position().prop_map(|pos| ActionType::Position(0, (pos.x as f32, pos.y as f32))),
        random_position().prop_map(|pos| ActionType::PlaceFloorTile(PlaceFloorTileByHandInfo {
            ghost_info: PlaceFloorTileGhostInfo {
                tile: FloorTile::Concrete,
                position: PositionInfo::Single { pos: pos }
            },
            player: ()
        })),
        (random_position(), random_recipe(data_store))
            .prop_map(|(pos, recipe)| ActionType::SetRecipe(SetRecipeInfo { pos, recipe })),
        random_entity_to_place(data_store).prop_map(|ty| {
            ActionType::PlaceEntity(PlaceEntityInfo {
                entities: EntityPlaceOptions::Single(ty),
            })
        }),
        // random_position().prop_map(|pos| ActionType::Remove(pos)),
    ]
}

pub fn random_entity_to_place<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> impl Strategy<Value = PlaceEntityType<ItemIdxType>> {
    prop_oneof![
        random_position().prop_map(|pos| PlaceEntityType::Assembler(pos)),
        random_position().prop_map(|pos| PlaceEntityType::Chest { pos }),
        (random_position(), random_dir()).prop_map(|(pos, dir)| PlaceEntityType::Belt {
            pos,
            direction: dir
        }),
        (random_position(), 0..data_store.power_pole_data.len()).prop_map(|(pos, ty)| {
            PlaceEntityType::PowerPole {
                pos,
                ty: ty.try_into().unwrap(),
            }
        }),
        (random_position(), random_dir(), random_item(data_store)).prop_map(
            |(pos, dir, filter)| {
                PlaceEntityType::Inserter {
                    pos,
                    dir,
                    filter: Some(filter),
                }
            }
        ),
        // (random_position(), random_dir()).prop_map(|(pos, dir)| {
        //     PlaceEntityType::Splitter {
        //         pos,
        //         direction: dir,

        //         // TODO: Test inout modes
        //         in_mode: None,
        //         out_mode: None,
        //     }
        // })
    ]
}

pub fn random_dir() -> impl Strategy<Value = Dir> {
    prop_oneof![
        Just(Dir::North),
        Just(Dir::South),
        Just(Dir::East),
        Just(Dir::West)
    ]
}

pub fn random_recipe<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> impl Strategy<Value = Recipe<RecipeIdxType>> {
    (0..data_store.recipe_timers.len())
        .prop_map(|recipe_idx| RecipeIdxType::try_from(recipe_idx).unwrap())
        .prop_map(|id| Recipe { id })
}

pub fn random_item<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
    data_store: &DataStore<ItemIdxType, RecipeIdxType>,
) -> impl Strategy<Value = Item<ItemIdxType>> {
    (0..data_store.item_names.len())
        .prop_map(|item_idx| ItemIdxType::try_from(item_idx).unwrap())
        .prop_map(|id| Item { id })
}

pub fn random_position() -> impl Strategy<Value = Position> {
    ((1600u32..1616), (1600u32..1616)).prop_map(|(x, y)| Position {
        x: x as usize,
        y: y as usize,
    })
}
