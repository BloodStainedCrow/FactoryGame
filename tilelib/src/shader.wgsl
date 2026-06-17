// Vertex shader

struct VertexInput {
    @location(0) corner_position: vec2<f32>,

    @location(1) sprite_position: vec2<f32>,
    @location(2) sprite_dimensions: vec2<f32>,
    
    @location(3) texture_width_mul: f32,
    @location(4) texture_width_offs: f32,
};

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) tex_coords: vec2<f32>,
};

@vertex
fn vs_main(
    @builtin(vertex_index) in_vertex_index: u32,
    model: VertexInput,
) -> VertexOutput {
    var out: VertexOutput;
    out.clip_position = vec4<f32>(model.corner_position.x * model.sprite_dimensions.x * 2 + (model.sprite_position.x * 2.0 - 1.0), -(model.corner_position.y * model.sprite_dimensions.y * 2 + (model.sprite_position.y * 2.0 - 1.0)), 0.0, 1.0);
    out.tex_coords = vec2<f32>(model.corner_position.x * model.texture_width_mul + model.texture_width_offs, model.corner_position.y);
    return out;
}

// Fragment shader

@group(0) @binding(0)
var t_diffuse: texture_2d<f32>;
@group(0) @binding(1)
var s_diffuse: sampler;

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    return textureSample(t_diffuse, s_diffuse, in.tex_coords);
    // return vec4<f32>(0.0, 0.0, in.clip_position.z, 1.0);
}
