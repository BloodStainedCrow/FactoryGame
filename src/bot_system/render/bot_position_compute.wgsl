@group(0) @binding(0) var<storage, read> input: array<BotFlightData>;
@group(0) @binding(1) var<storage, read_write> output: array<vec2<f32>>;
@group(0) @binding(2) var<uniform> current_time: f32;
@group(0) @binding(3) var<uniform> camera_pos: vec2<f32>;

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
fn get_position(
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

    var pos: vec2<f32>;
    if should_be_visible {
        let is_waiting = current_time < data.start_time;

        if is_waiting {
           pos = data.start_pos; 
        } else {
            let is_in_first_half = current_time < data.mid_time;

            if is_in_first_half {
                pos = data.start_pos + (data.mid_pos - data.start_pos) * ((current_time - data.start_time) / (data.mid_time - data.start_time));
            } else {
                pos = data.mid_pos + (data.end_pos - data.mid_pos) * ((current_time - data.mid_time) / (data.end_time - data.mid_time));
            }
        }
    } else {
        // FIXME: Just put the bot super far away for now so the user cannot see it.
        // Ideally I would want a proper culling algorithm, but I really do not want to use atomics.
        // TODO Benchmark with and without atomics.
        // Another option is a prefix sum style algorithm
        pos = vec2(1000000000000000.0, 1000000000000000.0);
    }
    output[index] = pos - camera_pos;
}
