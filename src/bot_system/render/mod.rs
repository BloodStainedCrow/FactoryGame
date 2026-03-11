use tilelib::types::RawRenderer;

use crate::bot_system::BotRenderInfo;

pub(crate) mod cpu_vec;
pub(crate) mod fake_render_dedicated;

#[derive(Debug)]
pub(crate) enum AddBotError {
    OutOfSlots,
}

pub(crate) trait BotRender: serde::Serialize + for<'a> serde::Deserialize<'a> {
    type Renderer;

    fn new(num_slots: usize) -> Self;

    fn add_flying_bots(
        &mut self,
        bot_kind: usize,
        new_bots: impl IntoIterator<Item = BotRenderInfo>,
        current_time: f32,
    ) -> Result<(), AddBotError>;

    fn render(
        &self,
        renderer: &mut Self::Renderer,
        camera_pos: (f32, f32),
        num_tiles_across_screen_horizontal: f32,
        num_tiles_across_screen_vertical: f32,
        current_time: f32,
    );
    fn render_map_view(
        &self,
        renderer: &mut Self::Renderer,
        camera_pos: (f32, f32),
        num_tiles_across_screen_horizontal: f32,
        num_tiles_across_screen_vertical: f32,
        current_time: f32,
    );
}
