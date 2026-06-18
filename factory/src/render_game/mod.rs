use data::spacial::{BoundingBox, Extent, Position};
use frontend::{SurfaceId, query::WorldQueryEngine};
use tilelib::types::{DrawInstance, Layer, RendererTrait, Sprite, Texture};

pub const WIDTH_PER_LEVEL: u16 = 16;

#[derive(Debug, Clone, Copy)]
pub struct RenderInfo {
    pub surface: SurfaceId,
    pub map_view: bool,
    pub zoom_level: f32,
    pub center: [f32; 2],
}

pub fn render_game(
    renderer: &mut impl RendererTrait,
    query_engine: &impl WorldQueryEngine,
    render_info: RenderInfo,
) {
    let aspect_ratio = renderer.get_aspect_ratio();

    let num_tiles_across_screen_horizontal =
        f32::from(WIDTH_PER_LEVEL) * 1.5f32.powf(render_info.zoom_level);
    let num_tiles_across_screen_vertical = num_tiles_across_screen_horizontal / aspect_ratio;
    let tile_size: f32 = 1.0 / num_tiles_across_screen_horizontal;

    // let floor_tile_layer = Layer::square_tile_grid(tile_size, aspect_ratio);
    let mut entity_layer = Layer::square_tile_grid(tile_size, aspect_ratio);

    let top_left: [f32; 2] = [
        render_info.center[0] - num_tiles_across_screen_horizontal / 2.0,
        render_info.center[1] - num_tiles_across_screen_vertical / 2.0,
    ];

    let sprite = Sprite::new(Texture::default());

    for entity in query_engine.get_entity_render_infos_for_area(
        render_info.surface,
        BoundingBox::new(
            Position {
                x: top_left[0] as i32,
                y: top_left[1] as i32,
            },
            Extent {
                width: num_tiles_across_screen_horizontal as u32 + 1,
                height: num_tiles_across_screen_vertical as u32 + 1,
            },
        )
        .extend_evenly(1),
    ) {
        entity_layer.draw_sprite(
            // TODO:
            &sprite,
            DrawInstance {
                position: [
                    entity.position.x as f32 - top_left[0],
                    entity.position.y as f32 - top_left[1],
                ],
                size: [entity.size.width as f32, entity.size.height as f32],
                // TODO:
                animation_frame: 0,
            },
        );
    }

    renderer.draw(&entity_layer);
}
