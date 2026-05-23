use crate::bot_system::BotRenderInfo;

impl super::BotRender<!> for (usize, Box<[BotRenderInfo]>) {
    fn new(num_slots: usize) -> Self {
        (0, vec![BotRenderInfo::default()].into_boxed_slice())
    }

    fn add_flying_bots(
        &mut self,
        bot_kind: usize,
        new_bots: impl IntoIterator<Item = BotRenderInfo>,
        current_time: f32,
    ) -> Result<(), super::AddBotError> {
        assert!(self.0 <= self.1.len());

        // TODO: Is there a better function for doing this
        for render_info in new_bots {
            debug_assert!(self.1[self.0].end_time() < current_time);
            self.1[self.0] = render_info;
            self.0 += 1;
            self.0 %= self.1.len();
        }

        Ok(())
    }

    fn render<const N: usize>(
        &mut self,
        renderer: &mut [&mut !; N],
        camera_pos: (f32, f32),
        num_tiles_across_screen_horizontal: f32,
        num_tiles_across_screen_vertical: f32,
        current_time: f32,
    ) {
        unimplemented!()
    }

    fn render_map_view<const N: usize>(
        &mut self,
        renderer: &mut [&mut !; N],
        camera_pos: (f32, f32),
        num_tiles_across_screen_horizontal: f32,
        num_tiles_across_screen_vertical: f32,
        current_time: f32,
    ) {
        unimplemented!()
    }
}
