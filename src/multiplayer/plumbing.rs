use std::{
    io::{Read, Write},
    marker::PhantomData,
    mem,
    net::TcpStream,
    sync::{Arc, mpsc::Receiver},
    time::{Duration, Instant},
};

use crate::{
    app_state::{AuxillaryData, SimulationState},
    multiplayer::PlayerIDInformation,
};
use flate2::Compression;
use log::error;
use parking_lot::Mutex;

#[cfg(feature = "client")]
use crate::frontend::action::action_state_machine::ActionStateMachine;
use crate::{
    data::DataStore,
    frontend::{action::ActionType, input::Input, world::tile::World},
    item::{IdxTrait, WeakIdxTrait},
};

use super::{
    ServerInfo,
    server::{ActionSource, HandledActionConsumer},
};

#[cfg(feature = "client")]
pub(crate) struct Client<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub(super) local_input: Receiver<Input>,
    pub(super) local_actions: Arc<Mutex<ActionStateMachine<ItemIdxType, RecipeIdxType>>>,
    pub(super) server_connection: TcpStream,
    pub(super) ui_actions: Receiver<ActionType<ItemIdxType, RecipeIdxType>>,
}

pub(crate) struct Server<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    client_connections: Mutex<Vec<TcpStream>>,
    new_connections: Receiver<TcpStream>,
    accepted_connections: Mutex<Vec<(PlayerIDInformation, TcpStream)>>,

    item: PhantomData<ItemIdxType>,
    recipe: PhantomData<RecipeIdxType>,
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> Server<ItemIdxType, RecipeIdxType> {
    pub fn new(info: ServerInfo) -> Self {
        Self {
            client_connections: Mutex::new(info.connections),
            new_connections: info.new_connection_recv,
            accepted_connections: Mutex::new(vec![]),

            item: PhantomData,
            recipe: PhantomData,
        }
    }
}

#[cfg(feature = "client")]
impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> ActionSource<ItemIdxType, RecipeIdxType>
    for Client<ItemIdxType, RecipeIdxType>
{
    fn get<'a, 'b, 'c, 'd, 'e>(
        &'a self,
        _current_tick: u64,
        world: &'b World<ItemIdxType, RecipeIdxType>,
        sim_state: &'d SimulationState<ItemIdxType, RecipeIdxType>,
        _aux_data: &'e AuxillaryData,
        data_store: &'c DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>>
    + use<'a, 'b, 'c, 'd, ItemIdxType, RecipeIdxType> {
        // This will block (?) if we did not yet recieve the actions from the server for this tick
        // TODO: This could introduce hitches which might be noticeable.
        //       This could be solved either by introducing some fixed delay on all actions (i.e. just running the client a couple ticks in the past compared to the server)
        //       Or using a rollback feature, by (i.e.) assuming no actions are run
        // Get the actions from what the server sent us

        let mut state_machine = self.local_actions.lock();

        let mut local_actions: Vec<_> = state_machine
            .handle_inputs(self.local_input.try_iter(), world, sim_state, data_store)
            .into_iter()
            .collect();

        local_actions.extend(state_machine.once_per_update_actions(world, data_store));

        local_actions.extend(self.ui_actions.try_iter());

        mem::drop(state_machine);

        for action in &local_actions {
            postcard::to_io(&action, &self.server_connection).expect("tcp send failed");
        }
        // FIXME: This sucks
        let mut buffer: Vec<u8> = vec![0; 1_000_000];

        let flavor =
            postcard::de_flavors::io::io::IOReader::new(&self.server_connection, &mut buffer);

        let mut deserializer = postcard::Deserializer::from_flavor(flavor);

        let recieved_actions: Vec<_> = match serde_path_to_error::deserialize(&mut deserializer) {
            Ok(actions) => actions,
            Err(err) => {
                let path = err.path().to_string();
                log::warn!("Failed to recieve actions. Path \"{}\" failed!", path);
                vec![]
            },
        };

        recieved_actions.into_iter()
    }
}

#[cfg(feature = "client")]
impl<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait>
    HandledActionConsumer<ItemIdxType, RecipeIdxType> for Client<ItemIdxType, RecipeIdxType>
{
    fn consume(
        &mut self,
        _current_tick: u64,
        _actions: impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>>,
    ) {
        // Do Nothing
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> ActionSource<ItemIdxType, RecipeIdxType>
    for Server<ItemIdxType, RecipeIdxType>
{
    fn get<'a, 'b, 'c, 'd, 'e>(
        &'a self,
        _current_tick: u64,
        world: &'b World<ItemIdxType, RecipeIdxType>,
        sim_state: &'d SimulationState<ItemIdxType, RecipeIdxType>,
        aux_data: &'e AuxillaryData,
        _data_store: &'c DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>>
    + use<'a, 'b, 'c, 'd, ItemIdxType, RecipeIdxType> {
        log::trace!("Server::Get");
        const RECV_BUFFER_LEN: usize = 10_000;
        let start = Instant::now();
        // This is the Server, it will just keep on chugging along and never block
        // Get the actions from what the clients sent us
        // FIXME: Puke this is awful
        let mut buffer = vec![0; RECV_BUFFER_LEN];
        if start.elapsed() > Duration::from_millis(10) {
            error!("buffer {:?}", start.elapsed());
        }
        let mut recieved_actions: Vec<ActionType<_, _>> = vec![];

        self.client_connections.lock().retain(|mut conn| {
            let mut ret = vec![];

            let keep = match conn.peek(&mut buffer) {
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

                    true
                },
                Err(e) => match e.kind() {
                    std::io::ErrorKind::WouldBlock => {
                        // No data to read
                        true
                    },

                    err => {
                        log::warn!("Dropping Connection: {:?}", err);
                        false
                    },
                },
            };

            recieved_actions.extend(ret);

            keep
        });
        if start.elapsed() > Duration::from_millis(10) {
            error!("recieved_actions {:?}", start.elapsed());
        }

        let spawn_player_actions = {
            profiling::scope!("Accept new connections");

            for (id_info, mut new_conn) in self.accepted_connections.lock().drain(..) {
                assert_eq!(
                    self.client_connections
                        .lock()
                        .iter()
                        .find_map(|running_conn| (running_conn.peer_addr().unwrap()
                            == new_conn.peer_addr().unwrap())
                        .then_some(new_conn.peer_addr().unwrap())),
                    None
                );

                log::info!("Sending GameState to new connection");
                if let Err(err) = new_conn.set_nonblocking(false) {
                    log::warn!("Failed setting connction to blocking: {:?}", err);
                    continue;
                }

                let mut compressed =
                    flate2::write::ZlibEncoder::new(&mut new_conn, Compression::best());

                if let Err(err) = postcard::to_io(&id_info, &mut compressed) {
                    // Sending failed, do not add this conn
                    log::warn!("Failed sending id_info: {:?}", err);
                    continue;
                }
                if let Err(err) = postcard::to_io(sim_state, &mut compressed) {
                    // Sending failed, do not add this conn
                    log::warn!("Failed sending sim_state: {:?}", err);
                    continue;
                }
                if let Err(err) = postcard::to_io(world, &mut compressed) {
                    // Sending failed, do not add this conn
                    log::warn!("Failed sending world: {:?}", err);
                    continue;
                }
                if let Err(err) = postcard::to_io(aux_data, &mut compressed) {
                    // Sending failed, do not add this conn
                    log::warn!("Failed sending aux_data: {:?}", err);
                    continue;
                }

                std::mem::drop(compressed);

                if let Err(err) = new_conn.set_nonblocking(true) {
                    log::warn!("Failed setting connction to blocking: {:?}", err);
                    continue;
                }

                if let Err(err) = new_conn.flush() {
                    log::warn!("Flushing failed: {:?}", err);
                    continue;
                }

                self.client_connections.lock().push(new_conn);
            }

            let mut actions = vec![];

            for new_conn in self.new_connections.try_iter() {
                let next_id = (world.players.len() + actions.len())
                    .try_into()
                    .expect("Max of u16::MAX Player allowed");

                actions.push(ActionType::SpawnPlayer {});

                self.accepted_connections
                    .lock()
                    .push((PlayerIDInformation { player_id: next_id }, new_conn));
            }

            actions
        };

        recieved_actions.into_iter().chain(spawn_player_actions)
    }
}

impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>
    HandledActionConsumer<ItemIdxType, RecipeIdxType> for Server<ItemIdxType, RecipeIdxType>
{
    fn consume(
        &mut self,
        _current_tick: u64,
        actions: impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>>,
    ) {
        let actions: Vec<_> = actions.into_iter().collect();
        // Send the actions to the clients
        self.client_connections.lock().retain(|mut conn| {
            let mut keep = if let Err(err) = postcard::to_io(&actions, conn) {
                log::warn!("Dropping Connection: {:?}", err);
                false
            } else {
                true
            };
            keep &= conn.flush().is_ok();

            // let keep = serde_json::to_writer(conn, &actions).is_ok();

            keep
        });
    }
}

#[cfg(feature = "client")]
pub(crate) struct IntegratedServer<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    pub(super) local_actions: Arc<Mutex<ActionStateMachine<ItemIdxType, RecipeIdxType>>>,
    pub(super) local_input: Receiver<Input>,
    pub(super) server: Server<ItemIdxType, RecipeIdxType>,
    pub(super) ui_actions: Receiver<ActionType<ItemIdxType, RecipeIdxType>>,
}

#[cfg(feature = "client")]
impl<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait> ActionSource<ItemIdxType, RecipeIdxType>
    for IntegratedServer<ItemIdxType, RecipeIdxType>
{
    fn get<'a, 'b, 'c, 'd, 'e>(
        &'a self,
        current_tick: u64,
        world: &'b World<ItemIdxType, RecipeIdxType>,
        sim_state: &'d SimulationState<ItemIdxType, RecipeIdxType>,
        aux_data: &'e AuxillaryData,
        data_store: &'c DataStore<ItemIdxType, RecipeIdxType>,
    ) -> impl Iterator<Item = ActionType<ItemIdxType, RecipeIdxType>>
    + use<'a, 'b, 'c, 'd, ItemIdxType, RecipeIdxType> {
        let start = Instant::now();
        let mut state_machine = self.local_actions.lock();
        if start.elapsed() > Duration::from_millis(10) {
            error!("Post lock {:?}", start.elapsed());
        }

        let mut v: Vec<_> = state_machine
            .handle_inputs(self.local_input.try_iter(), world, sim_state, data_store)
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
            .get(current_tick, world, sim_state, aux_data, data_store)
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
