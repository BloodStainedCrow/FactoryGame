pub mod render;

use crate::{GameState, query::render::WorldRenderQueryEngine};

pub trait WorldQueryEngine: WorldRenderQueryEngine {}

impl WorldQueryEngine for GameState {}
