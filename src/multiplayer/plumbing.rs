use std::{
    marker::PhantomData,
    net::TcpStream,
    sync::{mpsc::Receiver, Arc, Mutex},
};

use crate::{
    frontend::action::ActionType,
    item::{IdxTrait, WeakIdxTrait},
};

use super::{
    connection_reciever::ConnectionList,
    server::{ActionSource, HandledActionConsumer},
    ServerInfo,
};

pub(super) struct Client<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub(super) local_actions: Receiver<ActionType<ItemIdxType, RecipeIdxType>>,
    pub(super) server_connection: TcpStream,
}

pub(super) struct Server<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    client_connections: ConnectionList,

    item: PhantomData<ItemIdxType>,
    recipe: PhantomData<RecipeIdxType>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Server<ItemIdxType, RecipeIdxType> {
    pub fn new(info: ServerInfo) -> Self {
        Self {
            client_connections: info.connections,
            item: PhantomData,
            recipe: PhantomData,
        }
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> ActionSource<ItemIdxType, RecipeIdxType>
    for Client<ItemIdxType, RecipeIdxType>
{
    fn get(
        &self,
        current_tick: u64,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>, IntoIter: Clone>
           + Clone
           + use<ItemIdxType, RecipeIdxType> {
        // This will block (?) if we did not yet recieve the actions from the server for this tick
        // TODO: This could introduce hitches which might be noticeable.
        //       This could be solved either by introducing some fixed delay on all actions (i.e. just running the client a couple ticks in the past compared to the server)
        //       Or using a rollback feature, by (i.e.) assuming no actions are run
        // Get the actions from what the server sent us
        let local_actions = self
            .local_actions
            .get(current_tick)
            .into_iter()
            .collect::<Vec<_>>();
        postcard::to_io(&local_actions, &self.server_connection).expect("tcp send failed");
        let mut buffer = Vec::with_capacity(100);
        let (recieved_actions, v): (Vec<_>, _) =
            postcard::from_io((&self.server_connection, &mut buffer)).unwrap();

        recieved_actions
    }
}

impl<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait>
    HandledActionConsumer<ItemIdxType, RecipeIdxType> for Client<ItemIdxType, RecipeIdxType>
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
    for Server<ItemIdxType, RecipeIdxType>
{
    fn get(
        &self,
        current_tick: u64,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>, IntoIter: Clone>
           + Clone
           + use<ItemIdxType, RecipeIdxType> {
        // This is the Server, it will just keep on chugging along and never block
        // Get the actions from what the clients sent us

        let mut buffer = Vec::with_capacity(100);
        let recieved_actions: Vec<ActionType<_, _>> = self
            .client_connections
            .lock()
            .unwrap()
            .iter()
            .flat_map(|conn| {
                postcard::from_io::<Vec<ActionType<_, _>>, _>((conn, &mut buffer))
                    .unwrap()
                    .0
            })
            .collect();

        recieved_actions
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>
    HandledActionConsumer<ItemIdxType, RecipeIdxType> for Server<ItemIdxType, RecipeIdxType>
{
    fn consume(
        &mut self,
        current_tick: u64,
        actions: impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>>,
    ) {
        let actions: Vec<_> = actions.into_iter().collect();
        // Send the actions to the clients
        for conn in self.client_connections.lock().unwrap().iter() {
            postcard::to_io(&actions, conn).expect("tcp send failed");
        }
    }
}

pub(super) struct IntegratedServer<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub(super) local_actions: Receiver<ActionType<ItemIdxType, RecipeIdxType>>,
    pub(super) server: Server<ItemIdxType, RecipeIdxType>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> ActionSource<ItemIdxType, RecipeIdxType>
    for IntegratedServer<ItemIdxType, RecipeIdxType>
{
    fn get(
        &self,
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
