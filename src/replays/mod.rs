use std::borrow::Borrow;
use std::future::Future;
use std::ops::ControlFlow;

use std::sync::Arc;

use parking_lot::Mutex;
use std::mem;

use std::path::PathBuf;

use genawaiter::rc::{r#gen, Gen};
use genawaiter::GeneratorState::Complete;
use genawaiter::GeneratorState::Yielded;
use itertools::Itertools;

use crate::{
    data::DataStore,
    frontend::action::ActionType,
    item::{IdxTrait, WeakIdxTrait},
    rendering::app_state::GameState,
};

// TODO: Keyframe support
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Replay<
    ItemIdxType: WeakIdxTrait,
    RecipeIdxType: WeakIdxTrait,
    DataStor: Borrow<DataStore<ItemIdxType, RecipeIdxType>>,
> {
    /// Compressed binary representation of the starting GameState
    starting_state: Box<[u8]>,
    pub actions: Vec<ReplayAction<ItemIdxType, RecipeIdxType>>,

    pub data_store: DataStor,

    current_timestep: u64,

    end_timestep: Option<u64>,

    storage_location: Option<PathBuf>,

    is_dummy: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ReplayAction<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    timestamp: u64,
    pub action: ActionType<ItemIdxType, RecipeIdxType>,
}

impl<
        ItemIdxType: IdxTrait,
        RecipeIdxType: IdxTrait,
        DataStor: Borrow<DataStore<ItemIdxType, RecipeIdxType>>,
    > Replay<ItemIdxType, RecipeIdxType, DataStor>
{
    pub fn new(
        game_state: &GameState<ItemIdxType, RecipeIdxType>,
        storage_location: Option<PathBuf>,
        data_store: DataStor,
    ) -> Self {
        let game_state_bytes = bitcode::serialize(game_state).unwrap();
        Self {
            starting_state: game_state_bytes.into_boxed_slice(),
            actions: vec![],
            data_store,
            current_timestep: 0,
            end_timestep: None,

            storage_location,

            is_dummy: false,
        }
    }

    pub fn new_dummy(data_store: DataStor) -> Self {
        Self {
            starting_state: Box::new([]),
            actions: vec![],
            data_store,
            current_timestep: 0,
            end_timestep: None,

            storage_location: None,

            is_dummy: true,
        }
    }

    pub fn tick(&mut self) {
        self.current_timestep = self
            .current_timestep
            .checked_add(1)
            .expect("Replay running longer than u64::MAX ticks!");
    }

    pub fn append_actions(
        &mut self,
        actions: impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>>,
    ) {
        if self.is_dummy {
            return;
        }
        self.actions
            .extend(actions.into_iter().map(|a| ReplayAction {
                timestamp: self.current_timestep,
                action: a,
            }));
    }

    pub fn finish(&mut self) {
        self.end_timestep = Some(self.current_timestep);
    }

    pub fn run(
        self,
    ) -> ReplayViewer<
        (GameState<ItemIdxType, RecipeIdxType>, DataStor),
        impl Future<Output = (GameState<ItemIdxType, RecipeIdxType>, DataStor)>,
    > {
        assert!(!self.is_dummy);
        ReplayViewer {
            generator: r#gen!({
                let data_store = self.data_store;

                let mut actions = self.actions.into_iter().peekable();
                let mut current_timestep = 0;

                let mut game_state: GameState<ItemIdxType, RecipeIdxType> =
                    bitcode::deserialize(&*self.starting_state).unwrap();

                // Free up the memory, so we do not store two copies of the GameState
                mem::drop(self.starting_state);

                loop {
                    let this_ticks_actions = actions
                        .by_ref()
                        .peeking_take_while(|a| a.timestamp == current_timestep)
                        .map(|ra| ra.action);

                    game_state.apply_actions(this_ticks_actions, data_store.borrow());

                    game_state.update(data_store.borrow());

                    // let game_state_opt: Option<GameState<ItemIdxType, RecipeIdxType>> =
                    //     yield_!(game_state);

                    // game_state = game_state_opt.unwrap();

                    if Some(current_timestep) == self.end_timestep {
                        break;
                    } else {
                        current_timestep += 1;
                    }
                }

                (game_state, data_store)
            }),
        }
    }

    pub fn run_with(
        self,
        game_state_out: Arc<Mutex<GameState<ItemIdxType, RecipeIdxType>>>,
        on_tick: impl Fn(),
    ) {
        dbg!(&self.end_timestep);

        let data_store = self.data_store;

        let mut actions = self.actions.into_iter().peekable();
        let mut current_timestep = 0;

        let game_state: GameState<ItemIdxType, RecipeIdxType> =
            bitcode::deserialize(&*self.starting_state).unwrap();

        // Free up the memory, so we do not store two copies of the GameState
        mem::drop(self.starting_state);

        *(game_state_out.lock()) = game_state;

        loop {
            let this_ticks_actions: Vec<_> = actions
                .by_ref()
                .peeking_take_while(|a| a.timestamp == current_timestep)
                .map(|ra| ra.action)
                .collect();

            let mut game_state = game_state_out.lock();

            game_state.apply_actions(this_ticks_actions, data_store.borrow());

            game_state.update(data_store.borrow());

            on_tick();

            // let game_state_opt: Option<GameState<ItemIdxType, RecipeIdxType>> =
            //     yield_!(game_state);

            // game_state = game_state_opt.unwrap();

            if Some(current_timestep) == self.end_timestep {
                break;
            } else {
                current_timestep += 1;
            }
        }
    }

    // fn save(&self) -> Result<(), ()>
    // where
    //     DataStor: serde::Serialize,
    // {
    //     match &self.storage_location {
    //         Some(path) => {
    //             // Ensure the folder exists
    //             create_dir_all(path).unwrap();

    //             let

    //             let start = Instant::now();
    //             // If we are in debug mode, save the replay to a file
    //             let mut file = File::create(path).expect("Could not open file");
    //             let ser = bitcode::serialize(self).unwrap();
    //             dbg!(start.elapsed());
    //             file.write_all(ser.as_slice())
    //                 .expect("Could not write to file");
    //             dbg!(start.elapsed());
    //             Ok(())
    //         },
    //         None => Err(()),
    //     }
    // }
}

pub struct ReplayViewer<V, F: Future<Output = V>> {
    generator: Gen<V, Option<V>, F>,
}

impl<V, F: Future<Output = V>> ReplayViewer<V, F> {
    pub fn with(mut self, mut every_step: impl FnMut(&V) -> ControlFlow<(), ()>) -> V {
        let mut gs = self.generator.resume_with(None);

        while let Yielded(v) = gs {
            match every_step(&v) {
                ControlFlow::Continue(()) => {},
                ControlFlow::Break(()) => return v,
            }

            gs = self.generator.resume_with(Some(v));
        }

        let Complete(v) = gs else { unreachable!() };

        let _ = every_step(&v);

        v
    }
}

pub fn run_till_finished<V>(_: &V) -> ControlFlow<(), ()> {
    ControlFlow::Continue(())
}
