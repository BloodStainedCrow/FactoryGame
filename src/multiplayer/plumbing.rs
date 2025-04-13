use std::{
    io::Read,
    marker::PhantomData,
    mem,
    net::TcpStream,
    sync::{mpsc::Receiver, Arc, Mutex},
    time::{Duration, Instant},
};

use eframe::wgpu::hal::auxil::db;
use log::{error, warn};

use crate::{
    data::DataStore,
    frontend::{
        action::{action_state_machine::ActionStateMachine, ActionType},
        input::Input,
        world::tile::World,
    },
    item::{IdxTrait, WeakIdxTrait},
};

use super::{
    connection_reciever::ConnectionList,
    server::{ActionSource, HandledActionConsumer},
    ServerInfo,
};

pub(super) struct Client<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub(super) local_input: Receiver<Input>,
    pub(super) local_actions: Arc<Mutex<ActionStateMachine<ItemIdxType, RecipeIdxType>>>,
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
        world: &World<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>> + use<ItemIdxType, RecipeIdxType>
    {
        // This will block (?) if we did not yet recieve the actions from the server for this tick
        // TODO: This could introduce hitches which might be noticeable.
        //       This could be solved either by introducing some fixed delay on all actions (i.e. just running the client a couple ticks in the past compared to the server)
        //       Or using a rollback feature, by (i.e.) assuming no actions are run
        // Get the actions from what the server sent us
        let pre_lock = Instant::now();

        let mut state_machine = self.local_actions.lock().unwrap();

        let mut local_actions: Vec<_> = state_machine
            .handle_inputs(&self.local_input, world, data_store)
            .into_iter()
            .collect();
        dbg!(pre_lock.elapsed());
        local_actions.extend(state_machine.once_per_update_actions(world));

        mem::drop(state_machine);

        postcard::to_io(&local_actions, &self.server_connection).expect("tcp send failed");
        let mut buffer = vec![0; 1000];
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
        world: &World<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>> + use<ItemIdxType, RecipeIdxType>
    {
        let start = Instant::now();
        // This is the Server, it will just keep on chugging along and never block
        // Get the actions from what the clients sent us
        // FIXME: Puke this is awful
        let mut buffer = vec![0; 10000];
        if start.elapsed() > Duration::from_millis(1) {
            error!("buffer {:?}", start.elapsed());
        }
        let recieved_actions: Vec<ActionType<_, _>> = self
            .client_connections
            .lock()
            .unwrap()
            .iter()
            .flat_map(|mut conn| {
                let start = Instant::now();
                let ret = if let Ok(len) = conn.peek(&mut buffer) {
                    if len > 0 {
                        if let Ok((v, rest)) = postcard::take_from_bytes(&buffer[0..len]) {
                            let rest_len = rest.len();
                            conn.read_exact(&mut buffer[0..(len - rest_len)])
                                .expect("Read failed");
                            v
                        } else {
                            vec![]
                        }
                    } else {
                        warn!("No data recieved");
                        vec![]
                    }
                } else {
                    warn!("read failed");
                    vec![]
                };

                dbg!(start.elapsed());
                ret
            })
            .collect();
        if start.elapsed() > Duration::from_millis(1) {
            error!("recieved_actions {:?}", start.elapsed());
        }

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
    pub(super) local_actions: Arc<Mutex<ActionStateMachine<ItemIdxType, RecipeIdxType>>>,
    pub(super) local_input: Receiver<Input>,
    pub(super) server: Server<ItemIdxType, RecipeIdxType>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> ActionSource<ItemIdxType, RecipeIdxType>
    for IntegratedServer<ItemIdxType, RecipeIdxType>
{
    fn get<'a, 'b, 'c>(
        &'a self,
        current_tick: u64,
        world: &'b World<ItemIdxType, RecipeIdxType>,
        data_store: &'c DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>>
           + use<'a, 'b, 'c, ItemIdxType, RecipeIdxType> {
        let start = Instant::now();
        let mut state_machine = self.local_actions.lock().unwrap();
        if start.elapsed() > Duration::from_millis(1) {
            error!("Post lock {:?}", start.elapsed());
        }

        let mut v: Vec<_> = state_machine
            .handle_inputs(&self.local_input, world, data_store)
            .into_iter()
            .collect();
        if start.elapsed() > Duration::from_millis(1) {
            error!("Handle inputs {:?}", start.elapsed());
        }
        v.extend(state_machine.once_per_update_actions(&world));

        mem::drop(state_machine);

        if start.elapsed() > Duration::from_millis(1) {
            error!("once_per_update_actions {:?}", start.elapsed());
        }

        let ret = self
            .server
            .get(current_tick, world, data_store)
            .into_iter()
            .chain(v);
        if start.elapsed() > Duration::from_millis(1) {
            error!("server.get {:?}", start.elapsed());
        }
        ret
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
