use std::{
    borrow::{Borrow, BorrowMut},
    fs::File,
    io::Write,
    marker::PhantomData,
    time::Instant,
};

use crate::{
    data::DataStore,
    frontend::{action::ActionType, world::tile::World},
    item::{IdxTrait, WeakIdxTrait},
    rendering::app_state::GameState,
    replays::Replay,
};

trait ActionInterface<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>:
    ActionSource<ItemIdxType, RecipeIdxType> + HandledActionConsumer<ItemIdxType, RecipeIdxType>
{
}

impl<T, ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> ActionInterface<ItemIdxType, RecipeIdxType>
    for T
where
    T: ActionSource<ItemIdxType, RecipeIdxType> + HandledActionConsumer<ItemIdxType, RecipeIdxType>,
{
}

pub(super) trait ActionSource<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    fn get<'a, 'b, 'c>(
        &'a self,
        current_tick: u64,
        world: &'b World<ItemIdxType, RecipeIdxType>,
        data_store: &'c DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>>
           + use<'a, 'b, 'c, Self, ItemIdxType, RecipeIdxType>;
}

pub(super) trait HandledActionConsumer<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    fn consume(
        &mut self,
        current_tick: u64,
        actions: impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>>,
    );
}

pub(super) struct GameStateUpdateHandler<
    ItemIdxType: IdxTrait,
    RecipeIdxType: IdxTrait,
    ActionInterfaceType: ActionInterface<ItemIdxType, RecipeIdxType>,
> {
    action_interface: ActionInterfaceType,

    item: PhantomData<ItemIdxType>,
    recipe: PhantomData<RecipeIdxType>,
}

impl<
        ItemIdxType: IdxTrait,
        RecipeIdxType: IdxTrait,
        ActionInterfaceType: ActionInterface<ItemIdxType, RecipeIdxType>,
    > GameStateUpdateHandler<ItemIdxType, RecipeIdxType, ActionInterfaceType>
{
    pub fn new(actions: ActionInterfaceType) -> Self {
        Self {
            action_interface: actions,
            item: PhantomData,
            recipe: PhantomData,
        }
    }

    pub fn update<DataStor: Borrow<DataStore<ItemIdxType, RecipeIdxType>> + serde::Serialize>(
        &mut self,
        game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
        replay: Option<&mut Replay<ItemIdxType, RecipeIdxType, DataStor>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let start = Instant::now();
        let actions_iter = {
            profiling::scope!("Get Actions");

            self.action_interface
                .get(game_state.current_tick, &game_state.world, data_store)
        };

        let actions: Vec<_> = actions_iter.into_iter().collect();

        {
            profiling::scope!("Update Replay");
            if let Some(replay) = replay {
                replay.append_actions(actions.iter().cloned());
                replay.tick();

                #[cfg(debug_assertions)]
                {
                    profiling::scope!("Serialize Replay to disk");
                    // If we are in debug mode, save the replay to a file
                    let mut file = File::create("./last_replay.rep").expect("Could not open file");
                    let ser = bitcode::serialize(replay).unwrap();
                    file.write_all(ser.as_slice())
                        .expect("Could not write to file");
                }
            }
        }

        {
            profiling::scope!("Apply Actions");
            game_state
                .borrow_mut()
                .apply_actions(actions.clone(), data_store);
        }

        {
            profiling::scope!("Send Action Confirmations");
            self.action_interface
                .consume(game_state.current_tick, actions);
        }

        {
            profiling::scope!("GameState Update");
            game_state.borrow_mut().update(data_store);
        }
    }
}
