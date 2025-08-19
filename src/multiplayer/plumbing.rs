use std::{
    io::{Read, Write},
    marker::PhantomData,
    mem,
    net::TcpStream,
    sync::{Arc, mpsc::Receiver},
    time::{Duration, Instant},
};

use parking_lot::Mutex;

use log::{error, warn};

#[cfg(feature = "client")]
use crate::frontend::action::action_state_machine::ActionStateMachine;
use crate::{
    data::DataStore,
    frontend::{action::ActionType, input::Input, world::tile::World},
    item::{IdxTrait, WeakIdxTrait},
};

use super::{
    ServerInfo,
    connection_reciever_tcp::ConnectionList,
    server::{ActionSource, HandledActionConsumer},
};

#[cfg(feature = "client")]
pub(super) struct Client<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub(super) local_input: Receiver<Input>,
    pub(super) local_actions: Arc<Mutex<ActionStateMachine<ItemIdxType, RecipeIdxType>>>,
    pub(super) server_connection: TcpStream,
    pub(super) ui_actions: Receiver<ActionType<ItemIdxType, RecipeIdxType>>,
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

#[cfg(feature = "client")]
impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> ActionSource<ItemIdxType, RecipeIdxType>
    for Client<ItemIdxType, RecipeIdxType>
{
    fn get<'a>(
        &'a self,
        current_tick: u64,
        world: &World<ItemIdxType, RecipeIdxType>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>> + use<'a, ItemIdxType, RecipeIdxType>
    {
        // This will block (?) if we did not yet recieve the actions from the server for this tick
        // TODO: This could introduce hitches which might be noticeable.
        //       This could be solved either by introducing some fixed delay on all actions (i.e. just running the client a couple ticks in the past compared to the server)
        //       Or using a rollback feature, by (i.e.) assuming no actions are run
        // Get the actions from what the server sent us
        let pre_lock = Instant::now();

        let mut state_machine = self.local_actions.lock();

        let mut local_actions: Vec<_> = state_machine
            .handle_inputs(&self.local_input, world, data_store)
            .into_iter()
            .collect();
        local_actions.extend(state_machine.once_per_update_actions(world, data_store));

        mem::drop(state_machine);

        for action in &local_actions {
            postcard::to_io(&action, &self.server_connection).expect("tcp send failed");
        }
        let mut buffer = vec![0; 1_000_000];
        let (recieved_actions, v): (Vec<_>, _) =
            postcard::from_io((&self.server_connection, &mut buffer)).unwrap();

        recieved_actions
            .into_iter()
            .chain(self.ui_actions.try_iter())
    }
}

#[cfg(feature = "client")]
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
    ) -> impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>> + use<ItemIdxType, RecipeIdxType>
    {
        const RECV_BUFFER_LEN: usize = 10_000;
        let start = Instant::now();
        // This is the Server, it will just keep on chugging along and never block
        // Get the actions from what the clients sent us
        // FIXME: Puke this is awful
        let mut buffer = vec![0; RECV_BUFFER_LEN];
        if start.elapsed() > Duration::from_millis(10) {
            error!("buffer {:?}", start.elapsed());
        }
        let recieved_actions: Vec<ActionType<_, _>> = self
            .client_connections
            .lock()
            .iter()
            .flat_map(|mut conn| {
                let start = Instant::now();
                let mut ret = vec![];

                match conn.peek(&mut buffer) {
                    Ok(len) => {
                        let mut written_buffer = &buffer[0..len];

                        loop {
                            if let Ok((v, rest)) = postcard::take_from_bytes(written_buffer) {
                                let consumed_len = written_buffer.len() - rest.len();
                                written_buffer = rest;
                                std::io::copy(
                                    &mut std::io::Read::by_ref(&mut conn).take(consumed_len as u64),
                                    &mut std::io::sink(),
                                )
                                .expect("Discarding used bytes failed");
                                ret.push(v);
                            } else {
                                if written_buffer.len() == RECV_BUFFER_LEN {
                                    error!("RECV_BUFFER_LEN exhausted!");
                                }
                                break;
                            }
                        }
                    },
                    Err(e) => match e.kind() {
                        std::io::ErrorKind::WouldBlock => {
                            // No data to read
                        },

                        e => todo!("{:?}", e),
                    },
                }

                dbg!(start.elapsed());
                dbg!(ret.len());
                ret
            })
            .collect();
        if start.elapsed() > Duration::from_millis(10) {
            error!("recieved_actions {:?}", start.elapsed());
        }

        recieved_actions.into_iter()
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
        for conn in self.client_connections.lock().iter() {
            for action in &actions {
                postcard::to_io(&action, conn).expect("tcp send failed");
            }
        }
    }
}

#[cfg(feature = "client")]
pub(super) struct IntegratedServer<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub(super) local_actions: Arc<Mutex<ActionStateMachine<ItemIdxType, RecipeIdxType>>>,
    pub(super) local_input: Receiver<Input>,
    pub(super) server: Server<ItemIdxType, RecipeIdxType>,
    pub(super) ui_actions: Receiver<ActionType<ItemIdxType, RecipeIdxType>>,
}

#[cfg(feature = "client")]
impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> ActionSource<ItemIdxType, RecipeIdxType>
    for IntegratedServer<ItemIdxType, RecipeIdxType>
{
    fn get<'a, 'b, 'c>(
        &'a self,
        current_tick: u64,
        world: &'b World<ItemIdxType, RecipeIdxType>,
        data_store: &'c DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>>
    + use<'a, 'b, 'c, ItemIdxType, RecipeIdxType> {
        let start = Instant::now();
        let mut state_machine = self.local_actions.lock();
        if start.elapsed() > Duration::from_millis(10) {
            error!("Post lock {:?}", start.elapsed());
        }

        let mut v: Vec<_> = state_machine
            .handle_inputs(&self.local_input, world, data_store)
            .into_iter()
            .collect();
        if start.elapsed() > Duration::from_millis(10) {
            error!("Handle inputs {:?}", start.elapsed());
        }
        v.extend(state_machine.once_per_update_actions(&world, &data_store));

        mem::drop(state_machine);

        if start.elapsed() > Duration::from_millis(10) {
            error!("once_per_update_actions {:?}", start.elapsed());
        }

        let ret = self
            .server
            .get(current_tick, world, data_store)
            .into_iter()
            .chain(v);
        if start.elapsed() > Duration::from_millis(10) {
            error!("server.get {:?}", start.elapsed());
        }

        ret.into_iter().chain(self.ui_actions.try_iter())
    }
}

#[cfg(feature = "client")]
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
