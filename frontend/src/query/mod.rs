pub mod render;

use crate::query::render::WorldRenderQueryEngine;

pub trait WorldQueryEngine: WorldRenderQueryEngine {}
