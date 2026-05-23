@group(0) @binding(0) var<uniform> size: vec2<f32>;
@group(0) @binding(1) var<storage, read> position_buffer: array<vec2<f32>>;

const vertex_pos = array(vec2(0.0, 0.0), vec2(0.0, 1.0), vec2(1.0, 0.0), vec2(1.0, 0.0), vec2(0.0, 1.0), vec2(1.0, 1.0));

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
};

@vertex
fn vs_main(
    @builtin(vertex_index) in_vertex_index: u32,
) -> VertexOutput {
    var out: VertexOutput;
    let corner_position = vertex_pos[in_vertex_index % 6];
    let bot_position = position_buffer[in_vertex_index / 6];
    out.clip_position = vec4<f32>((corner_position.x * 2 + (bot_position.x * 2.0 - 1.0)) * size.x, (-(corner_position.y * 2 + (bot_position.y * 2.0 - 1.0))* size.y), 0.0, 1.0);
    
    // out.tex_coords = model.corner_position;
    return out;
}

// @group(0) @binding(1) var<uniform> pure_color: vec4<f32>;
// @fragment
// fn fs_texture(in: VertexOutput) -> @location(0) vec4<f32> {
//     return textureSample(t_diffuse, s_diffuse, in.tex_coords);
// }


@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    return vec4(1.0, 1.0, 1.0, 1.0);
}