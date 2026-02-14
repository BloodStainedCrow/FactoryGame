#[cfg(feature = "client")]
use egui_show_info_derive::ShowInfo;
#[cfg(feature = "client")]
use get_size2::GetSize;

use std::sync::Arc;

use itertools::Itertools;
use log::warn;

mod replay_action;

use crate::example_worlds::{self, ValueValue};
use crate::progress_info::ProgressInfo;
use crate::replays::replay_action::{ReplayAction, ReplayActionError};
use crate::{app_state::GameState, data::DataStore, frontend::action::ActionType, item::IdxTrait};
use crate::{built_info, get_version};

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) struct ProgramInformation {
    // The git rev when this was recorded
    game_version: String,
    git_dirty: bool,
    mod_sha: String,
    mod_list: Vec<()>,
}
impl ProgramInformation {
    pub(crate) fn new<A: IdxTrait, B: IdxTrait>(data_store: &DataStore<A, B>) -> Self {
        Self {
            game_version: get_version().to_string(),
            git_dirty: built_info::GIT_DIRTY.unwrap_or(false),
            mod_sha: data_store.checksum.clone(),
            mod_list: vec![],
        }
    }
}

#[cfg_attr(feature = "client", derive(ShowInfo), derive(GetSize))]
#[derive(Debug, Default, Clone, serde::Serialize, serde::Deserialize)]
pub struct GenerationInformation {
    // The example world (and settings) which were used
    pub example_idx: usize,
    pub example_settings: Vec<ValueValue>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Replay {
    program_info: ProgramInformation,
    generation_info: GenerationInformation,

    actions: Vec<ReplayTimedAction>,

    current_timestep: u64,
    end_timestep: Option<u64>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct ReplayTimedAction {
    timestamp: u64,
    action: ReplayAction,
}

impl Replay {
    pub(crate) fn new<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        generation_info: GenerationInformation,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) -> Self {
        Self {
            program_info: ProgramInformation::new(data_store),
            generation_info,
            actions: vec![],
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

    pub fn append_actions<ItemIdxType: IdxTrait, RecipeIdxType: IdxTrait>(
        &mut self,
        actions: impl IntoIterator<Item = ActionType<ItemIdxType, RecipeIdxType>>,
        data_store: &DataStore<ItemIdxType, RecipeIdxType>,
    ) {
        self.actions
            .extend(actions.into_iter().map(|a| ReplayTimedAction {
                timestamp: self.current_timestep,
                action: ReplayAction::from_action(a, data_store),
            }));
    }

    pub fn finish(&mut self) {
        self.end_timestep = Some(self.current_timestep);
    }

    pub fn run(
        self,
        game_state: Arc<GameState<u8, u8>>,
        mut on_tick: impl FnMut(&GameState<u8, u8>),
        data_store: &DataStore<u8, u8>,
    ) -> Result<Arc<GameState<u8, u8>>, ReplayActionError> {
        let mut actions = self.actions.into_iter().peekable();
        let mut current_timestep = 0;

        if data_store.checksum != self.program_info.mod_sha {
            warn!("Mod SHA mismatch between replay recording and playback. Sync issues may appear");
        }

        if self.program_info.game_version != get_version() && !cfg!(test) {
            warn!(
                "Game version mismatch between replay recording and playback. Sync issues may appear"
            );
        }

        let GameState {
            world,
            simulation_state,
            aux_data,
        } = &*game_state;

        let new_game_state: GameState<u8, u8> = example_worlds::get_builder(
            "REPLAY_WORLD".to_string(),
            self.generation_info.example_idx,
            self.generation_info.example_settings,
        )(ProgressInfo::new(), data_store);

        {
            *simulation_state.lock() = new_game_state.simulation_state.mutex.into_inner();
            *world.lock() = new_game_state.world.mutex.into_inner();
            *aux_data.lock() = new_game_state.aux_data.mutex.into_inner();
        }

        loop {
            let this_ticks_actions: Vec<_> = actions
                .by_ref()
                .peeking_take_while(|a| a.timestamp == current_timestep)
                .map(|ra| ra.action.to_action(data_store))
                .try_collect()?;

            {
                let mut sim_state = game_state.simulation_state.lock();
                let mut world = game_state.world.lock();

                GameState::apply_actions(
                    &mut *sim_state,
                    &mut *world,
                    this_ticks_actions,
                    data_store,
                );

                GameState::update(
                    &mut *sim_state,
                    &mut *game_state.aux_data.lock(),
                    data_store,
                );
            }

            on_tick(&game_state);

            if Some(current_timestep) == self.end_timestep {
                break Ok(game_state);
            } else {
                current_timestep += 1;
            }
        }
    }
}

pub fn run_till_finished(_: &GameState<u8, u8>) {}
