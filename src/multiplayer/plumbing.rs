use std::sync::mpsc::Receiver;

use crate::{frontend::action::ActionType, item::IdxTrait};

use super::server::{ActionSource, HandledActionConsumer};

pub(super) struct Client {}

pub(super) struct Server {}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> ActionSource<ItemIdxType, RecipeIdxType>
    for Client
{
    fn get(
        &mut self,
        current_tick: u64,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>, IntoIter: Clone>
           + Clone
           + use<ItemIdxType, RecipeIdxType> {
        // This will block (?) if we did not yet recieve the actions from the server for this tick
        // TODO: This could introduce hitches which might be noticeable.
        //       This could be solved either by introducing some fixed delay on all actions (i.e. just running the client a couple ticks in the past compared to the server)
        //       Or using a rollback feature, by (i.e.) assuming no actions are run
        // Get the actions from what the server sent us
        vec![todo!()]
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>
    HandledActionConsumer<ItemIdxType, RecipeIdxType> for Client
{
    fn consume(
        &mut self,
        current_tick: u64,
        actions: impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>>,
    ) {
        // Do Nothing
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> ActionSource<ItemIdxType, RecipeIdxType>
    for Server
{
    fn get(
        &mut self,
        current_tick: u64,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>, IntoIter: Clone>
           + Clone
           + use<ItemIdxType, RecipeIdxType> {
        // This is the Server, it will just keep on chugging along and never block
        // Get the actions from what the clients sent us
        vec![todo!()]
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>
    HandledActionConsumer<ItemIdxType, RecipeIdxType> for Server
{
    fn consume(
        &mut self,
        current_tick: u64,
        actions: impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>>,
    ) {
        // Send the actions to the clients
    }
}

pub(super) struct IntegratedServer<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> {
    local_actions: Receiver<ActionType<ItemIdxType, RecipeIdxType>>,
    server: Server,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> ActionSource<ItemIdxType, RecipeIdxType>
    for IntegratedServer<ItemIdxType, RecipeIdxType>
{
    fn get(
        &mut self,
        current_tick: u64,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>, IntoIter: Clone>
           + Clone
           + use<ItemIdxType, RecipeIdxType> {
        self.local_actions
            .get(current_tick)
            .into_iter()
            .chain(self.server.get(current_tick))
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>
    HandledActionConsumer<ItemIdxType, RecipeIdxType>
    for IntegratedServer<ItemIdxType, RecipeIdxType>
{
    fn consume(
        &mut self,
        current_tick: u64,
        actions: impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>>,
    ) {
        // Send the actions to the clients
        self.server.consume(current_tick, actions);
    }
}
