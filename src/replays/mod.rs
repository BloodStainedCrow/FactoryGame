use std::future::Future;
use std::ops::ControlFlow;

use genawaiter::rc::{r#gen, Gen};
use genawaiter::yield_;
use genawaiter::GeneratorState::Complete;
use genawaiter::GeneratorState::Yielded;

use crate::{
    data::DataStore, frontend::action::ActionType, item::{IdxTrait, WeakIdxTrait}, rendering::app_state::GameState,
};

// TODO: Keyframe support
#[derive(Debug, Clone)]
pub struct Replay<
    ItemIdxType: IdxTrait,
    RecipeIdxType: IdxTrait,
    DataStor: AsRef<DataStore<ItemIdxType, RecipeIdxType>>,
> {
    starting_state: GameState<ItemIdxType, RecipeIdxType>,
    actions: Vec<ReplayAction<ItemIdxType, RecipeIdxType>>,

    data_store: DataStor,

    current_timestep: u64,

    end_timestep: Option<u64>,
}

#[derive(Debug, Clone)]
struct ReplayAction<ItemIdxType: WeakIdxTrait, RecipeIdxType: WeakIdxTrait> {
    timestamp: u64,
    action: ActionType<ItemIdxType, RecipeIdxType>,
}

impl<
        ItemIdxType: IdxTrait,
        RecipeIdxType: IdxTrait,
        DataStor: AsRef<DataStore<ItemIdxType, RecipeIdxType>>,
    > Replay<ItemIdxType, RecipeIdxType, DataStor>
{
    pub fn new(game_state: GameState<ItemIdxType, RecipeIdxType>, data_store: DataStor) -> Self {
        Self {
            starting_state: game_state,
            actions: vec![],
            data_store,
            current_timestep: 0,
            end_timestep: None,
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
        GameState<ItemIdxType, RecipeIdxType>,
        impl Future<Output = GameState<ItemIdxType, RecipeIdxType>>,
    > {
        ReplayViewer {
            generator: r#gen!({
                let data_store = self.data_store;

                let mut game_state: GameState<ItemIdxType, RecipeIdxType> = self.starting_state;
                let mut actions = self.actions.into_iter().peekable();
                let mut current_timestep = 0;

                loop {
                    let this_ticks_actions = actions
                        .by_ref()
                        .take_while(|a| a.timestamp == current_timestep)
                        .map(|ra| ra.action);

                    game_state.apply_actions(this_ticks_actions, data_store.as_ref());

                    game_state.update(data_store.as_ref());

                    let game_state_opt: Option<GameState<ItemIdxType, RecipeIdxType>> =
                        yield_!(game_state);

                    game_state = game_state_opt.unwrap();

                    if Some(current_timestep) == self.end_timestep {
                        break;
                    } else {
                        current_timestep += 1;
                    }
                }

                game_state
            }),
        }
    }
}

pub struct ReplayViewer<V, F: Future<Output = V>> {
    generator: Gen<V, Option<V>, F>,
}

impl<V, F: Future<Output = V>> ReplayViewer<V, F> {
    pub fn with(mut self, every_step: impl Fn(&V) -> ControlFlow<(), ()>) -> V {
        let mut gs = self.generator.resume_with(None);

        while let Yielded(v) = gs {
            match every_step(&v) {
                ControlFlow::Continue(()) => {},
                ControlFlow::Break(()) => return v,
            }

            gs = self.generator.resume_with(Some(v));
        }

        let Complete(v) = gs else { unreachable!() };

        every_step(&v);

        v
    }
}

pub fn run_till_finished<V>(_: &V) -> ControlFlow<(), ()> {
    ControlFlow::Continue(())
}
