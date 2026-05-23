use tilelib::types::RawRenderer;

use crate::bot_system::BotRenderInfo;

mod cpu_vec;
mod fake_render_dedicated;
mod full_gpu_render;

pub(crate) type BotRenderStore = full_gpu_render::FullGPURender;

#[derive(Debug)]
pub(crate) enum AddBotError {
    OutOfSlots,
}

pub(crate) trait BotRender<Renderer>:
    serde::Serialize + for<'a> serde::Deserialize<'a>
{
    fn new(num_slots: usize) -> Self;

    fn add_flying_bots(
        &mut self,
        bot_kind: usize,
        new_bots: impl IntoIterator<Item = BotRenderInfo>,
        current_time: f32,
    ) -> Result<(), AddBotError>;

    fn render<const N: usize>(
        &mut self,
        renderer: &mut [&mut Renderer; N],
        camera_pos: (f32, f32),
        num_tiles_across_screen_horizontal: f32,
        num_tiles_across_screen_vertical: f32,
        current_time: f32,
    );
    fn render_map_view<const N: usize>(
        &mut self,
        renderer: &mut [&mut Renderer; N],
        camera_pos: (f32, f32),
        num_tiles_across_screen_horizontal: f32,
        num_tiles_across_screen_vertical: f32,
        current_time: f32,
    );
}
