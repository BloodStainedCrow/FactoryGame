use std::{borrow::Borrow, fs::File, io::Write, marker::PhantomData};

use crate::{
    app_state::{GameState, SimulationState},
    data::DataStore,
    frontend::{action::ActionType, world::tile::World},
    item::{IdxTrait, WeakIdxTrait},
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
    fn get<'a, 'b, 'c, 'd>(
        &'a self,
        current_tick: u64,
        world: &'b World<ItemIdxType, RecipeIdxType>,
        sim_state: &'d SimulationState<ItemIdxType, RecipeIdxType>,
        data_store: &'c DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>>
    + use<'a, 'b, 'c, 'd, Self, ItemIdxType, RecipeIdxType>;
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
        game_state: &GameState<ItemIdxType, RecipeIdxType>,
        replay: Option<&mut Replay<ItemIdxType, RecipeIdxType, DataStor>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let mut simulation_state = game_state.simulation_state.lock();
        {
            profiling::scope!("GameState Update");
            GameState::update(
                &mut *simulation_state,
                &mut *game_state.aux_data.lock(),
                data_store,
            );
        }

        let mut world = game_state.world.lock();
        let current_tick = game_state.aux_data.lock().current_tick;

        let actions_iter = {
            profiling::scope!("Get Actions");

            self.action_interface
                .get(current_tick, &world, &simulation_state, data_store)
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
            GameState::apply_actions(
                &mut simulation_state,
                &mut world,
                actions.clone(),
                data_store,
            );
        }

        {
            profiling::scope!("Send Action Confirmations");
            self.action_interface.consume(current_tick, actions);
        }
    }
}
