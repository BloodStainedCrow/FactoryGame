use std::{
    borrow::BorrowMut,
    marker::PhantomData,
    time::{Duration, Instant},
};

use log::{error, info};

use crate::{
    data::DataStore,
    frontend::{action::ActionType, world::tile::World},
    item::{IdxTrait, WeakIdxTrait},
    rendering::app_state::GameState,
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

    pub fn update(
        &mut self,
        game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let start = Instant::now();
        let actions_iter =
            self.action_interface
                .get(game_state.current_tick, &game_state.world, data_store);
        if start.elapsed() > Duration::from_millis(10) {
            error!("Got action iter {:?}", start.elapsed());
        }
        let actions: Vec<_> = actions_iter.into_iter().collect();
        if start.elapsed() > Duration::from_millis(10) {
            error!("Got actions {:?}", start.elapsed());
        }

        game_state
            .borrow_mut()
            .apply_actions(actions.clone(), data_store);
        if start.elapsed() > Duration::from_millis(10) {
            error!("Actions applied {:?}", start.elapsed());
        }

        self.action_interface
            .consume(game_state.current_tick, actions);
        if start.elapsed() > Duration::from_millis(10) {
            error!("Actions sent {:?}", start.elapsed());
        }

        game_state.borrow_mut().update(data_store);
        if start.elapsed() > Duration::from_millis(10) {
            error!("Update done {:?}", start.elapsed());
        } else {
            info!("Update done {:?}", start.elapsed());
        }
    }
}
