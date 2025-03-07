use std::{borrow::BorrowMut, marker::PhantomData};

use crate::{
    data::DataStore, frontend::action::ActionType, item::IdxTrait, rendering::app_state::GameState,
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

pub(super) trait ActionSource<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    fn get(
        &mut self,
        current_tick: u64,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>, IntoIter: Clone>
           + Clone
           + use<Self, ItemIdxType, RecipeIdxType>;
}

pub(super) trait HandledActionConsumer<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
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
    pub fn update(
        &mut self,
        game_state: &mut GameState<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        let actions = self.action_interface.get(game_state.current_tick);

        game_state
            .borrow_mut()
            .apply_actions(actions.clone(), data_store);

        self.action_interface
            .consume(game_state.current_tick, actions);

        game_state.borrow_mut().update(data_store);
    }
}
