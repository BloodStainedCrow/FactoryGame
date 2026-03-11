use tilelib::types::DrawInstance;

use crate::{bot_system::BotRenderInfo, rendering::BOT_SPRITE};

#[cfg_attr(
    feature = "show-info",
    derive(egui_show_info_derive::ShowInfo),
    derive(get_size2::GetSize)
)]
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub(crate) struct CPUBotRenderer {
    next_insertion: usize,
    slots: Box<[BotRenderInfo]>,
}

impl super::BotRender for CPUBotRenderer {
    type Renderer = tilelib::types::Layer;

    fn new(num_slots: usize) -> Self {
        Self {
            next_insertion: 0,
            slots: vec![BotRenderInfo::default(); num_slots].into_boxed_slice(),
        }
    }

    fn add_flying_bots(
        &mut self,
        bot_kind: usize,
        new_bots: impl IntoIterator<Item = BotRenderInfo>,
        current_time: f32,
    ) -> Result<(), super::AddBotError> {
        assert!(dbg!(self.next_insertion) <= self.slots.len());

        // TODO: Is there a better function for doing this
        for render_info in new_bots {
            assert!(self.slots[self.next_insertion].end_time() < current_time);
            self.slots[self.next_insertion] = render_info;
            self.next_insertion += 1;
            self.next_insertion %= self.slots.len();
        }

        Ok(())
    }

    #[profiling::function]
    fn render(
        &self,
        renderer: &mut Self::Renderer,
        camera_pos: (f32, f32),
        num_tiles_across_screen_horizontal: f32,
        num_tiles_across_screen_vertical: f32,
        current_time: f32,
    ) {
        self.render_map_view(
            renderer,
            camera_pos,
            num_tiles_across_screen_horizontal,
            num_tiles_across_screen_vertical,
            current_time,
        );
    }

    #[profiling::function]
    fn render_map_view(
        &self,
        renderer: &mut Self::Renderer,
        camera_pos: (f32, f32),
        num_tiles_across_screen_horizontal: f32,
        num_tiles_across_screen_vertical: f32,
        current_time: f32,
    ) {
        let mut count = 0;
        for render_info in &self.slots {
            if render_info.end_time() > current_time {
                count += 1;
                renderer.draw_sprite(
                    &BOT_SPRITE,
                    render_info.get_instance(
                        camera_pos,
                        num_tiles_across_screen_horizontal,
                        num_tiles_across_screen_vertical,
                        current_time,
                    ),
                );
            } else {
                // This particle has already expired
            }
        }

        if count > 0 {
            dbg!(count);
        }
    }
}

impl BotRenderInfo {
    fn get_instance(
        self,
        camera_pos: (f32, f32),
        num_tiles_across_screen_horizontal: f32,
        num_tiles_across_screen_vertical: f32,
        current_time: f32,
    ) -> DrawInstance {
        match self {
            BotRenderInfo::StraightLine {
                sprite,
                start_time,
                end_time,
                start_pos,
                end_pos,
            } => {
                debug_assert!(current_time >= start_time);
                debug_assert!(current_time <= end_time);
                let perc = (1.0 - (end_time - current_time) / (end_time - start_time));
                let pos = [
                    start_pos.0 + (end_pos.0 - start_pos.0) * perc,
                    start_pos.1 + (end_pos.1 - start_pos.1) * perc,
                ];
                DrawInstance {
                    position: [
                        pos[0] - camera_pos.0 + num_tiles_across_screen_horizontal / 2.0,
                        pos[1] - camera_pos.1 + num_tiles_across_screen_vertical / 2.0,
                    ],
                    size: [1.0; 2],
                    animation_frame: 0,
                }
            },
            BotRenderInfo::VShape {
                sprite,
                start_time,
                mid_time,
                end_time,
                start_pos,
                mid_pos,
                end_pos,
            } => {
                debug_assert!(current_time >= start_time);
                debug_assert!(current_time <= end_time);
                let is_first = current_time <= mid_time;

                let (start_pos, end_pos, start_time, end_time) = if is_first {
                    (start_pos, mid_pos, start_time, mid_time)
                } else {
                    (mid_pos, end_pos, mid_time, end_time)
                };

                let perc = (1.0 - (end_time - current_time) / (end_time - start_time));
                let pos = [
                    start_pos.0 + (end_pos.0 - start_pos.0) * perc,
                    start_pos.1 + (end_pos.1 - start_pos.1) * perc,
                ];
                DrawInstance {
                    position: [
                        pos[0] - camera_pos.0 + num_tiles_across_screen_horizontal / 2.0,
                        pos[1] - camera_pos.1 + num_tiles_across_screen_vertical / 2.0,
                    ],
                    size: [1.0; 2],
                    animation_frame: 0,
                }
            },
            BotRenderInfo::WaitThenVShape {
                sprite,
                wait_start_time,
                start_time,
                mid_time,
                end_time,
                start_pos,
                mid_pos,
                end_pos,
            } => todo!(),
        }
    }
}
