@group(0) @binding(0) var<storage, read> input: array<BotFlightData>;
@group(0) @binding(1) var<uniform> current_time: f32;
@group(0) @binding(2) var<uniform> camera_pos: vec2<f32>;
@group(0) @binding(3) var output_texture: texture_storage_2d<rgba8unorm, write>;
@group(0) @binding(4) var<uniform> size: vec2<f32>;

struct BotFlightData {
    @location(0) start_pos: vec2<f32>,
    @location(1) mid_pos: vec2<f32>,
    @location(2) end_pos: vec2<f32>,
    @location(3) wait_start_time: f32,
    @location(4) start_time: f32,
    @location(5) mid_time: f32,
    @location(6) end_time: f32,
};


// Tells wgpu that this function is a valid compute pipeline entry_point
@compute
// Specifies the "dimension" of this work group
// This is the maximum guaranteed by wgpu
@workgroup_size(256, 1, 1)
fn put_pixel(
    // global_invocation_id specifies our position in the invocation grid
    @builtin(global_invocation_id) global_invocation_id: vec3<u32>
) {
    let index = global_invocation_id.x;
    let total = arrayLength(&input);

    // workgroup_size may not be a multiple of the array size so
    // we need to exit out a thread that would index out of bounds.
    if (index >= total) {
        return;
    }

    let data = input[index];

    let should_be_visible = current_time >= data.wait_start_time && current_time <= data.end_time;

    if should_be_visible {
        // FIXME: Handle div by ZERO NaNs.

        // This is greater one if we are not in the first half
        // This is negative if we are waiting
        let first_half_time = (current_time - data.start_time) / (data.mid_time - data.start_time);

        // This is negative if we are not in the second half
        let second_half_time = (current_time - data.mid_time) / (data.end_time - data.mid_time);

        let bot_position: vec2<f32> = data.start_pos + clamp(first_half_time, 0.0, 1.0) * (data.mid_pos - data.start_pos) + clamp(second_half_time, 0.0, 1.0) * (data.end_pos - data.mid_pos);
        let bot_position_in_camera_space = bot_position - camera_pos;

        let pos = (vec2(bot_position_in_camera_space.x, -bot_position_in_camera_space.y) * size + vec2(0.5, 0.5)) * vec2<f32>(textureDimensions(output_texture));
        let output_color = vec4(1.0, 1.0, 1.0, 1.0);

        textureStore(output_texture, vec2<i32>(i32(pos.x), i32(pos.y)), output_color);
    } else {
        // Do not render the bot
    }

}

// Tells wgpu that this function is a valid compute pipeline entry_point
@compute
// Specifies the "dimension" of this work group
// This is the maximum guaranteed by wgpu
@workgroup_size(256, 1, 1)
fn clear_texture(
    // global_invocation_id specifies our position in the invocation grid
    @builtin(global_invocation_id) global_invocation_id: vec3<u32>
) {
    let index = global_invocation_id.x;
    let dimensions = textureDimensions(output_texture);
    let total = dimensions.x * dimensions.y;

    // workgroup_size may not be a multiple of the array size so
    // we need to exit out a thread that would index out of bounds.
    if (index >= total) {
        return;
    }

    let pos = vec2(index % dimensions.x, index / dimensions.x);

    textureStore(output_texture, pos, vec4(0.0, 0.0, 0.0, 0.0));
}