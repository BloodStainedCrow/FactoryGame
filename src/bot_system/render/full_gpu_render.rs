use std::{cmp::min, num::NonZeroI32};

use rayon::{
    iter::{IndexedParallelIterator, ParallelIterator},
    slice::ParallelSlice,
};
use tilelib::types::{DrawInstance, InprogressRawRenderer, RawRenderer, RendererTrait};
use wgpu::{
    BindGroupDescriptor, BindGroupLayout, BlendState, ComputePassDescriptor,
    ComputePipelineDescriptor, Extent3d, RenderPipelineDescriptor, ShaderModuleDescriptor,
    TextureAspect, TextureUsages,
    util::{BufferInitDescriptor, DeviceExt},
    wgt::{
        BufferDescriptor, CommandEncoderDescriptor, SamplerDescriptor, TextureDescriptor,
        TextureViewDescriptor,
    },
};

use crate::{bot_system::BotRenderInfo, rendering::BOT_SPRITE};

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub(crate) struct FullGPURender {
    next_insertion: usize,
    slots: Box<[BotFlightData]>,

    #[serde(skip)]
    compute_pipeline: Option<wgpu::ComputePipeline>,
    #[serde(skip)]
    single_pixel_compute_clear_pipeline: Option<wgpu::ComputePipeline>,
    #[serde(skip)]
    single_pixel_compute_render_pipeline: Option<wgpu::ComputePipeline>,
    #[serde(skip)]
    single_pixel_splat_pipeline: Option<wgpu::RenderPipeline>,
    #[serde(skip)]
    render_pipeline: Option<wgpu::RenderPipeline>,
    #[serde(skip)]
    next_upload: Option<usize>,
    #[serde(skip)]
    data_buffer: Option<Vec<wgpu::Buffer>>,
    #[serde(skip)]
    position_buffer: Option<Vec<wgpu::Buffer>>,
    #[serde(skip)]
    current_time_buffer: Option<wgpu::Buffer>,
    #[serde(skip)]
    camera_pos_buffer: Option<wgpu::Buffer>,
    #[serde(skip)]
    vertex_buffer: Option<wgpu::Buffer>,
    #[serde(skip)]
    index_buffer: Option<wgpu::Buffer>,
    #[serde(skip)]
    size_buffer: Option<wgpu::Buffer>,
    #[serde(skip)]
    render_bind_group_layout: Option<wgpu::BindGroupLayout>,

    #[serde(skip)]
    intermediate_texture: Option<wgpu::Texture>,
    #[serde(skip)]
    intermediate_texture_sampler: Option<wgpu::Sampler>,
}

impl Clone for FullGPURender {
    fn clone(&self) -> Self {
        Self {
            next_insertion: self.next_insertion.clone(),
            slots: self.slots.clone(),
            compute_pipeline: self.compute_pipeline.clone(),
            single_pixel_compute_render_pipeline: self.single_pixel_compute_render_pipeline.clone(),
            single_pixel_compute_clear_pipeline: self.single_pixel_compute_clear_pipeline.clone(),
            single_pixel_splat_pipeline: self.single_pixel_splat_pipeline.clone(),
            next_upload: self.next_upload.clone(),
            data_buffer: self.data_buffer.clone(),
            position_buffer: self.position_buffer.clone(),
            current_time_buffer: self.current_time_buffer.clone(),
            camera_pos_buffer: self.camera_pos_buffer.clone(),
            render_pipeline: self.render_pipeline.clone(),
            vertex_buffer: self.vertex_buffer.clone(),
            index_buffer: self.index_buffer.clone(),
            size_buffer: self.size_buffer.clone(),
            render_bind_group_layout: self.render_bind_group_layout.clone(),
            intermediate_texture: self.intermediate_texture.clone(),
            intermediate_texture_sampler: self.intermediate_texture_sampler.clone(),
        }
    }
}

#[cfg(feature = "show-info")]
impl<E: egui_show_info::InfoExtractor<Self, Info>, Info: egui_show_info::EguiDisplayable>
    egui_show_info::ShowInfo<E, Info> for FullGPURender
{
    fn show_fields<C: egui_show_info::Cache<String, Info>>(
        &self,
        _extractor: &mut E,
        _ui: &mut egui::Ui,
        _path: String,
        _cache: &mut C,
    ) {
    }
}

#[cfg(feature = "show-info")]
impl get_size2::GetSize for FullGPURender {}

impl<R> super::BotRender<R> for FullGPURender
where
    R: RendererTrait,
{
    fn new(num_slots: usize) -> Self {
        Self {
            next_insertion: 0,
            slots: vec![BotFlightData::default(); num_slots].into_boxed_slice(),
            compute_pipeline: None,
            single_pixel_compute_render_pipeline: None,
            single_pixel_compute_clear_pipeline: None,
            single_pixel_splat_pipeline: None,
            next_upload: None,
            data_buffer: None,
            position_buffer: None,
            current_time_buffer: None,
            camera_pos_buffer: None,
            render_pipeline: None,
            vertex_buffer: None,
            index_buffer: None,
            size_buffer: None,
            render_bind_group_layout: None,
            intermediate_texture: None,
            intermediate_texture_sampler: None,
        }
    }

    fn add_flying_bots(
        &mut self,
        bot_kind: usize,
        new_bots: impl IntoIterator<Item = BotRenderInfo>,
        current_time: f32,
    ) -> Result<(), super::AddBotError> {
        assert!(self.next_insertion <= self.slots.len());

        // TODO: Is there a better function for doing this
        for render_info in new_bots {
            // assert!(self.slots[self.next_insertion].end_time() < current_time);
            self.slots[self.next_insertion] = render_info.into();
            self.next_insertion += 1;
            self.next_insertion %= self.slots.len();
        }

        Ok(())
    }

    #[profiling::function]
    fn render<const N: usize>(
        &mut self,
        renderer: &mut [&mut R; N],
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
    fn render_map_view<const N: usize>(
        &mut self,
        renderer: &mut [&mut R; N],
        camera_pos: (f32, f32),
        num_tiles_across_screen_horizontal: f32,
        num_tiles_across_screen_vertical: f32,
        current_time: f32,
    ) {
        // FIXME: This is a runtime panic for a compile time error
        let Some(renderer) = renderer.into_iter().next() else {
            unreachable!("Only a Single RawRenderer should be supplied");
        };

        renderer.do_custom_draw(|device, render_pass, queue, target_format, canvas_size| {
            let buffer_size = device.limits().max_storage_buffer_binding_size as usize
                / size_of::<BotFlightData>();

            // Do the upload of the newly set stuff
            let data_buffers = if let Some(buffers) = &self.data_buffer {
                let next_upload = self
                    .next_upload
                    .expect("When we have a buffer, we should know what next to upload");
                // NOTE(Tim): This could lead to some jank *if* all slots (or more) are filled in a single tick. This is unreasonable and would also break the CPU storage side, so I do not worry about it.
                if next_upload != self.next_insertion {
                    if self.next_insertion > next_upload {
                        let affected_buffers =
                            (next_upload / buffer_size)..=(self.next_insertion / buffer_size);
                        for affected_buffer in affected_buffers {
                            let buffer_start = affected_buffer * buffer_size;
                            let next_buffer_start = (affected_buffer + 1) * buffer_size;

                            let start_write_index = next_upload.saturating_sub(buffer_start);

                            queue.write_buffer(
                                &buffers[affected_buffer],
                                (start_write_index * size_of::<BotFlightData>())
                                    .try_into()
                                    .unwrap(),
                                bytemuck::cast_slice(
                                    &self.slots
                                        [next_upload..min(self.next_insertion, next_buffer_start)],
                                ),
                            );
                        }
                    } else {
                        // self.next_insertion wrapped. So we need to upload the tail and the first bit
                        // FIXME: For now I just assume that the entire upload range belongs into the last/first buffer

                        dbg!("Wrapping");
                        queue.write_buffer(
                            buffers.last().unwrap(),
                            (((self.slots.len() - next_upload) % buffer_size)
                                * size_of::<BotFlightData>())
                            .try_into()
                            .unwrap(),
                            bytemuck::cast_slice(&self.slots[next_upload..]),
                        );
                        queue.write_buffer(
                            buffers.first().unwrap(),
                            0,
                            bytemuck::cast_slice(&self.slots[..self.next_insertion]),
                        );
                    }
                    self.next_upload = Some(self.next_insertion);
                }

                buffers
            } else {
                // We do not yet have the GPU buffer, do the initial creation and upload

                let buffers = self
                    .slots
                    .chunks(buffer_size)
                    .enumerate()
                    .map(|(i, slots)| {
                        let buffer = device.create_buffer_init(&BufferInitDescriptor {
                            label: Some(&format!("bot_data_buffer_{}", i)),
                            contents: bytemuck::cast_slice(&slots),
                            usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST,
                        });
                        buffer
                    })
                    .collect();

                self.data_buffer = Some(buffers);
                self.next_upload = Some(self.next_insertion);
                self.data_buffer.as_ref().unwrap()
            };

            // Setup the position buffer if needed
            let position_buffers = if let Some(buffers) = &self.position_buffer {
                buffers
            } else {
                // We do not yet have the GPU buffer, do the initial creation and upload
                let buffers = self
                    .slots
                    .chunks(buffer_size)
                    .enumerate()
                    .map(|(i, slots)| {
                        let buffer = device.create_buffer(&BufferDescriptor {
                            label: Some(&format!("bot_position_buffer_{}", i)),
                            size: (slots.len() * size_of::<BotFlightData>())
                                .try_into()
                                .unwrap(),
                            usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::VERTEX,
                            mapped_at_creation: false,
                        });
                        buffer
                    })
                    .collect();
                self.position_buffer = Some(buffers);
                self.position_buffer.as_ref().unwrap()
            };

            // Setup the current_time buffer if needed
            let current_time_buffer = if let Some(buffer) = &self.current_time_buffer {
                queue.write_buffer(buffer, 0, bytemuck::bytes_of(&current_time));
                buffer
            } else {
                // We do not yet have the GPU buffer, do the initial creation and upload
                let buffer = device.create_buffer_init(&BufferInitDescriptor {
                    label: Some("bot_current_time_buffer"),
                    contents: bytemuck::bytes_of(&current_time),
                    usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
                });
                self.current_time_buffer = Some(buffer);
                self.current_time_buffer.as_ref().unwrap()
            };

            // Setup the camera_pos buffer if needed
            let camera_pos_buffer = if let Some(buffer) = &self.camera_pos_buffer {
                queue.write_buffer(buffer, 0, bytemuck::bytes_of(&[camera_pos.0, camera_pos.1]));
                buffer
            } else {
                // We do not yet have the GPU buffer, do the initial creation and upload
                let buffer = device.create_buffer_init(&BufferInitDescriptor {
                    label: Some("bot_camera_pos_buffer"),
                    contents: bytemuck::bytes_of(&[camera_pos.0, camera_pos.1]),
                    usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
                });
                self.camera_pos_buffer = Some(buffer);
                self.camera_pos_buffer.as_ref().unwrap()
            };

            // Do the rendering
            let size = [
                1.0 / num_tiles_across_screen_horizontal,
                1.0 / num_tiles_across_screen_vertical,
            ];

            let size_buffer = if let Some(buffer) = &self.size_buffer {
                queue.write_buffer(buffer, 0, bytemuck::bytes_of(&size));
                buffer
            } else {
                let buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                    label: Some("Bot Size Buffer"),
                    contents: bytemuck::bytes_of(&size),
                    usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
                });
                self.size_buffer = Some(buffer);
                self.size_buffer.as_ref().unwrap()
            };

            let mut encoder = device.create_command_encoder(&CommandEncoderDescriptor {
                label: Some("Bot Command Encoder"),
            });

            if size[0] <= 1.5 / canvas_size[0] as f32 && size[1] <= 1.5 / canvas_size[1] as f32 {
                // The bot is a single pixel

                // When the bot is a single pixel, we use a compute shader to render it, since the normal render pipeline is very inefficient for tris this small.
                let mut compute_pass = encoder.begin_compute_pass(&ComputePassDescriptor {
                    label: Some("Bot render single pixel pass"),
                    timestamp_writes: None,
                });

                let pipeline = if let Some(pipeline) = &self.single_pixel_compute_render_pipeline {
                    pipeline
                } else {
                    let shader = device.create_shader_module(wgpu::include_wgsl!(
                        "./bot_render_single_pixel.wgsl"
                    ));
                    let pipeline = device.create_compute_pipeline(&ComputePipelineDescriptor {
                        label: Some("Bot Single Pixel Compute Pipeline"),
                        layout: None,
                        module: &shader,
                        entry_point: Some("put_pixel"),
                        compilation_options: Default::default(),
                        cache: None,
                    });

                    self.single_pixel_compute_render_pipeline = Some(pipeline);

                    self.single_pixel_compute_render_pipeline.as_ref().unwrap()
                };

                compute_pass.set_pipeline(pipeline);

                let group_layout = pipeline.get_bind_group_layout(0);

                let texture = if let Some(texture) = &self.intermediate_texture
                    && texture.width() == canvas_size[0]
                    && texture.height() == canvas_size[1]
                {
                    texture
                } else {
                    let texture = device.create_texture(&TextureDescriptor {
                        label: Some("single_pixel_intermediate_texture"),
                        size: Extent3d {
                            width: canvas_size[0],
                            height: canvas_size[1],
                            depth_or_array_layers: 1,
                        },
                        mip_level_count: 1,
                        sample_count: 1,
                        dimension: wgpu::TextureDimension::D2,
                        format: wgpu::TextureFormat::Rgba8Unorm,
                        usage: TextureUsages::TEXTURE_BINDING | TextureUsages::STORAGE_BINDING,
                        view_formats: &[wgpu::TextureFormat::Rgba8Unorm],
                    });
                    self.intermediate_texture = Some(texture);
                    self.intermediate_texture.as_ref().unwrap()
                };

                let texture_view = texture.create_view(&TextureViewDescriptor {
                    label: Some("single_pixel_intermediate_texture_view"),
                    format: None,
                    dimension: None,
                    usage: None,
                    aspect: TextureAspect::All,
                    base_mip_level: 0,
                    mip_level_count: None,
                    base_array_layer: 0,
                    array_layer_count: None,
                });

                let clear_pipeline =
                    if let Some(pipeline) = &self.single_pixel_compute_clear_pipeline {
                        pipeline
                    } else {
                        let shader = device.create_shader_module(wgpu::include_wgsl!(
                            "./bot_render_single_pixel.wgsl"
                        ));
                        let pipeline = device.create_compute_pipeline(&ComputePipelineDescriptor {
                            label: Some("Bot Single Pixel Compute Pipeline"),
                            layout: None,
                            module: &shader,
                            entry_point: Some("clear_texture"),
                            compilation_options: Default::default(),
                            cache: None,
                        });

                        self.single_pixel_compute_clear_pipeline = Some(pipeline);

                        self.single_pixel_compute_clear_pipeline.as_ref().unwrap()
                    };

                compute_pass.set_pipeline(clear_pipeline);
                let clear_group_layout = clear_pipeline.get_bind_group_layout(0);

                let bind_group = device.create_bind_group(&BindGroupDescriptor {
                    label: Some("clear_compute_shader_bind_group"),
                    layout: &clear_group_layout,
                    entries: &[wgpu::BindGroupEntry {
                        binding: 3,
                        resource: wgpu::BindingResource::TextureView(&texture_view),
                    }],
                });

                compute_pass.set_bind_group(0, &bind_group, &[]);
                compute_pass.dispatch_workgroups(
                    ((canvas_size[0] * canvas_size[1]).div_ceil(256))
                        .try_into()
                        .unwrap(),
                    1,
                    1,
                );

                compute_pass.set_pipeline(pipeline);

                for (data_buffer, num_slots) in data_buffers
                    .iter()
                    .zip(self.slots.chunks(buffer_size).map(|slots| slots.len()))
                {
                    let bind_group = device.create_bind_group(&BindGroupDescriptor {
                        label: Some("single_pixel_compute_shader_bind_group"),
                        layout: &group_layout,
                        entries: &[
                            wgpu::BindGroupEntry {
                                binding: 0,
                                resource: wgpu::BindingResource::Buffer(
                                    data_buffer.as_entire_buffer_binding(),
                                ),
                            },
                            wgpu::BindGroupEntry {
                                binding: 1,
                                resource: wgpu::BindingResource::Buffer(
                                    current_time_buffer.as_entire_buffer_binding(),
                                ),
                            },
                            wgpu::BindGroupEntry {
                                binding: 2,
                                resource: wgpu::BindingResource::Buffer(
                                    camera_pos_buffer.as_entire_buffer_binding(),
                                ),
                            },
                            wgpu::BindGroupEntry {
                                binding: 3,
                                resource: wgpu::BindingResource::TextureView(&texture_view),
                            },
                            wgpu::BindGroupEntry {
                                binding: 4,
                                resource: wgpu::BindingResource::Buffer(
                                    size_buffer.as_entire_buffer_binding(),
                                ),
                            },
                        ],
                    });

                    compute_pass.set_bind_group(0, &bind_group, &[]);
                    compute_pass.dispatch_workgroups(
                        (num_slots.div_ceil(256)).try_into().unwrap(),
                        1,
                        1,
                    );
                }

                std::mem::drop(compute_pass);

                let buffer = encoder.finish();
                queue.submit(std::iter::once(buffer));

                let pipeline = if let Some(pipeline) = &self.single_pixel_splat_pipeline {
                    pipeline
                } else {
                    let render_shader = device.create_shader_module(wgpu::include_wgsl!(
                        "./bot_render_intermediate_splat.wgsl"
                    ));
                    let pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
                        label: Some("Bot Single Pixel Splat Pipeline"),
                        layout: None,
                        cache: None,
                        vertex: wgpu::VertexState {
                            module: &render_shader,
                            entry_point: Some("vs_main"),
                            buffers: &[],
                            compilation_options: wgpu::PipelineCompilationOptions::default(),
                        },
                        fragment: Some(wgpu::FragmentState {
                            module: &render_shader,
                            entry_point: Some("fs_texture"),
                            targets: &[Some(wgpu::ColorTargetState {
                                format: *target_format,
                                blend: Some(BlendState::PREMULTIPLIED_ALPHA_BLENDING),
                                write_mask: wgpu::ColorWrites::ALL,
                            })],
                            compilation_options: wgpu::PipelineCompilationOptions::default(),
                        }),
                        primitive: wgpu::PrimitiveState {
                            topology: wgpu::PrimitiveTopology::TriangleList,
                            strip_index_format: None,
                            front_face: wgpu::FrontFace::Ccw,
                            cull_mode: None,
                            // Setting this to anything other than Fill requires Features::NON_FILL_POLYGON_MODE
                            polygon_mode: wgpu::PolygonMode::Fill,
                            // Requires Features::DEPTH_CLIP_CONTROL
                            unclipped_depth: false,
                            // Requires Features::CONSERVATIVE_RASTERIZATION
                            conservative: false,
                        },
                        depth_stencil: None,
                        multisample: wgpu::MultisampleState {
                            count: 1,
                            mask: !0,
                            alpha_to_coverage_enabled: false,
                        },
                        multiview: None,
                    });

                    self.single_pixel_splat_pipeline = Some(pipeline);

                    self.single_pixel_splat_pipeline.as_ref().unwrap()
                };

                render_pass.set_pipeline(pipeline);

                let sampler = if let Some(sampler) = &self.intermediate_texture_sampler {
                    sampler
                } else {
                    let sampler = device.create_sampler(&SamplerDescriptor {
                        address_mode_u: wgpu::AddressMode::ClampToEdge,
                        address_mode_v: wgpu::AddressMode::ClampToEdge,
                        address_mode_w: wgpu::AddressMode::ClampToEdge,
                        mag_filter: wgpu::FilterMode::Nearest,
                        min_filter: wgpu::FilterMode::Nearest,
                        mipmap_filter: wgpu::FilterMode::Nearest,
                        ..Default::default()
                    });
                    self.intermediate_texture_sampler = Some(sampler);

                    self.intermediate_texture_sampler.as_ref().unwrap()
                };

                let bind_group = device.create_bind_group(&BindGroupDescriptor {
                    label: Some("single_pixel_splat_shader_bind_group"),
                    layout: &pipeline.get_bind_group_layout(0),
                    entries: &[
                        wgpu::BindGroupEntry {
                            binding: 0,
                            resource: wgpu::BindingResource::TextureView(&texture_view),
                        },
                        wgpu::BindGroupEntry {
                            binding: 1,
                            resource: wgpu::BindingResource::Sampler(sampler),
                        },
                    ],
                });

                render_pass.set_bind_group(0, &bind_group, &[]);

                render_pass.draw(0..6, 0..1);
            } else {
                // The bot is big enough to be rendered using the rendering pipeline

                let pipeline = if let Some(pipeline) = &self.compute_pipeline {
                    pipeline
                } else {
                    let shader = device
                        .create_shader_module(wgpu::include_wgsl!("./bot_position_compute.wgsl"));
                    let pipeline = device.create_compute_pipeline(&ComputePipelineDescriptor {
                        label: Some("Bot Compute Pipeline"),
                        layout: None,
                        module: &shader,
                        entry_point: None,
                        compilation_options: Default::default(),
                        cache: None,
                    });

                    self.compute_pipeline = Some(pipeline);

                    self.compute_pipeline.as_ref().unwrap()
                };

                let mut encoder = device.create_command_encoder(&CommandEncoderDescriptor {
                    label: Some("Bot Command Encoder"),
                });

                let mut compute_pass = encoder.begin_compute_pass(&ComputePassDescriptor {
                    label: Some("Bot render position pass"),
                    timestamp_writes: None,
                });

                // Start the position calculating compute shader
                compute_pass.set_pipeline(pipeline);
                let group_layout = pipeline.get_bind_group_layout(0);

                for ((data_buffer, position_buffer), num_slots) in data_buffers
                    .iter()
                    .zip(position_buffers.iter())
                    .zip(self.slots.chunks(buffer_size).map(|slots| slots.len()))
                {
                    let bind_group = device.create_bind_group(&BindGroupDescriptor {
                        label: Some("position_compute_shader_bind_group"),
                        layout: &group_layout,
                        entries: &[
                            wgpu::BindGroupEntry {
                                binding: 0,
                                resource: wgpu::BindingResource::Buffer(
                                    data_buffer.as_entire_buffer_binding(),
                                ),
                            },
                            wgpu::BindGroupEntry {
                                binding: 1,
                                resource: wgpu::BindingResource::Buffer(
                                    position_buffer.as_entire_buffer_binding(),
                                ),
                            },
                            wgpu::BindGroupEntry {
                                binding: 2,
                                resource: wgpu::BindingResource::Buffer(
                                    current_time_buffer.as_entire_buffer_binding(),
                                ),
                            },
                            wgpu::BindGroupEntry {
                                binding: 3,
                                resource: wgpu::BindingResource::Buffer(
                                    camera_pos_buffer.as_entire_buffer_binding(),
                                ),
                            },
                        ],
                    });

                    compute_pass.set_bind_group(0, &bind_group, &[]);
                    compute_pass.dispatch_workgroups(
                        (num_slots.div_ceil(256)).try_into().unwrap(),
                        1,
                        1,
                    );
                }

                // Drop the compute pass since we are done with it.
                std::mem::drop(compute_pass);

                let buffer = encoder.finish();
                queue.submit(std::iter::once(buffer));

                let render_bind_group_layout = if let Some(layout) = &self.render_bind_group_layout
                {
                    layout
                } else {
                    let render_shader =
                        device.create_shader_module(wgpu::include_wgsl!("./bot_render.wgsl"));
                    let layout =
                        device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                            entries: &[
                                wgpu::BindGroupLayoutEntry {
                                    binding: 0,
                                    visibility: wgpu::ShaderStages::VERTEX,
                                    ty: wgpu::BindingType::Buffer {
                                        ty: wgpu::BufferBindingType::Uniform,
                                        has_dynamic_offset: false,
                                        min_binding_size: None,
                                    },
                                    count: None,
                                },
                                wgpu::BindGroupLayoutEntry {
                                    binding: 1,
                                    visibility: wgpu::ShaderStages::VERTEX,
                                    ty: wgpu::BindingType::Buffer {
                                        ty: wgpu::BufferBindingType::Storage { read_only: true },
                                        has_dynamic_offset: false,
                                        min_binding_size: None,
                                    },
                                    count: None,
                                },
                            ],
                            label: Some("render_bind_group_layout"),
                        });
                    self.render_bind_group_layout = Some(layout);
                    self.render_bind_group_layout.as_ref().unwrap()
                };

                let render_pipeline = if let Some(pipeline) = &self.render_pipeline {
                    pipeline
                } else {
                    // FIXME: We are double creating the shader!!!!
                    let render_shader =
                        device.create_shader_module(wgpu::include_wgsl!("./bot_render.wgsl"));
                    let render_pipeline_layout =
                        device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                            label: Some("Bot Render Pipeline Layout"),
                            bind_group_layouts: &[&render_bind_group_layout],
                            push_constant_ranges: &[],
                        });

                    let pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
                        label: Some("Bot render pipeline"),
                        layout: Some(&render_pipeline_layout),
                        vertex: wgpu::VertexState {
                            module: &render_shader,
                            entry_point: Some("vs_main"), // 1.
                            // buffers: &[vertex_desc(), index_desc()], // 2.
                            buffers: &[],
                            compilation_options: wgpu::PipelineCompilationOptions::default(),
                        },
                        fragment: Some(wgpu::FragmentState {
                            // 3.
                            module: &render_shader,
                            entry_point: Some("fs_main"),
                            targets: &[Some(wgpu::ColorTargetState {
                                // 4.
                                format: *target_format,
                                blend: None,
                                write_mask: wgpu::ColorWrites::ALL,
                            })],
                            compilation_options: wgpu::PipelineCompilationOptions::default(),
                        }),
                        primitive: wgpu::PrimitiveState {
                            topology: wgpu::PrimitiveTopology::TriangleList, // 1.
                            strip_index_format: None,
                            front_face: wgpu::FrontFace::Ccw, // 2.
                            cull_mode: None,
                            // Setting this to anything other than Fill requires Features::NON_FILL_POLYGON_MODE
                            polygon_mode: wgpu::PolygonMode::Fill,
                            // Requires Features::DEPTH_CLIP_CONTROL
                            unclipped_depth: false,
                            // Requires Features::CONSERVATIVE_RASTERIZATION
                            conservative: false,
                        },
                        depth_stencil: None, // 1.
                        multisample: wgpu::MultisampleState {
                            count: 1,                         // 2.
                            mask: !0,                         // 3.
                            alpha_to_coverage_enabled: false, // 4.
                        },
                        multiview: None, // 5.
                        cache: None,     // 6.
                    });

                    self.render_pipeline = Some(pipeline);

                    self.render_pipeline.as_ref().unwrap()
                };

                let vertex_buffer = if let Some(buffer) = &self.vertex_buffer {
                    buffer
                } else {
                    let buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                        label: Some("Vertex Buffer"),
                        contents: bytemuck::bytes_of(&[
                            0.0f32, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0,
                        ]),
                        usage: wgpu::BufferUsages::VERTEX,
                    });
                    self.vertex_buffer = Some(buffer);
                    self.vertex_buffer.as_ref().unwrap()
                };

                let index_buffer = if let Some(buffer) = &self.index_buffer {
                    buffer
                } else {
                    let buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
                        label: Some("Index Buffer"),
                        contents: bytemuck::cast_slice(&[0u16, 1, 2, 3, 4, 5]),
                        usage: wgpu::BufferUsages::INDEX,
                    });
                    self.index_buffer = Some(buffer);
                    self.index_buffer.as_ref().unwrap()
                };

                render_pass.set_pipeline(render_pipeline);

                // Set the vertices
                // render_pass.set_vertex_buffer(0, vertex_buffer.slice(..));
                // Set the instances
                // render_pass.set_index_buffer(index_buffer.slice(..), wgpu::IndexFormat::Uint16);

                for (position_buffer, instance_count) in position_buffers
                    .iter()
                    .zip(self.slots.chunks(buffer_size).map(|slots| slots.len()))
                {
                    let render_bind_group = device.create_bind_group(&BindGroupDescriptor {
                        label: Some("bot_render_bind_group"),
                        layout: &render_bind_group_layout,
                        entries: &[
                            wgpu::BindGroupEntry {
                                binding: 0,
                                resource: wgpu::BindingResource::Buffer(
                                    size_buffer.as_entire_buffer_binding(),
                                ),
                            },
                            wgpu::BindGroupEntry {
                                binding: 1,
                                resource: wgpu::BindingResource::Buffer(
                                    position_buffer.as_entire_buffer_binding(),
                                ),
                            },
                        ],
                    });
                    render_pass.set_bind_group(0, &render_bind_group, &[]);
                    // render_pass.set_vertex_buffer(1, position_buffer.slice(..));

                    render_pass.draw(0..(6 * instance_count as u32), 0..1);
                    // render_pass.draw(0..400, 0..1);
                }
            }
        });
    }
}

fn vertex_desc() -> wgpu::VertexBufferLayout<'static> {
    wgpu::VertexBufferLayout {
        array_stride: (2 * std::mem::size_of::<f32>()).try_into().unwrap(),
        step_mode: wgpu::VertexStepMode::Vertex,
        attributes: &[wgpu::VertexAttribute {
            offset: 0,
            shader_location: 0,
            format: wgpu::VertexFormat::Float32x2,
        }],
    }
}

fn index_desc() -> wgpu::VertexBufferLayout<'static> {
    wgpu::VertexBufferLayout {
        array_stride: (2 * std::mem::size_of::<f32>()).try_into().unwrap(),
        step_mode: wgpu::VertexStepMode::Instance,
        attributes: &[wgpu::VertexAttribute {
            offset: 0,
            shader_location: 1,
            format: wgpu::VertexFormat::Float32x2,
        }],
    }
}

#[derive(
    Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable, serde::Serialize, serde::Deserialize,
)]
#[repr(C)]
struct BotFlightData {
    start_pos: [f32; 2],
    mid_pos: [f32; 2],
    end_pos: [f32; 2],
    wait_start_time: f32,
    start_time: f32,
    mid_time: f32,
    end_time: f32,
}

impl Default for BotFlightData {
    fn default() -> Self {
        Self {
            start_pos: Default::default(),
            mid_pos: Default::default(),
            end_pos: Default::default(),
            wait_start_time: 3.0,
            start_time: 2.0,
            mid_time: 1.0,
            end_time: 0.0,
        }
    }
}

impl From<BotRenderInfo> for BotFlightData {
    fn from(value: BotRenderInfo) -> Self {
        match value {
            BotRenderInfo::StraightLine {
                sprite,
                start_time,
                end_time,
                start_pos,
                end_pos,
            } => Self {
                start_pos: start_pos.into(),
                mid_pos: start_pos.into(),
                end_pos: end_pos.into(),
                wait_start_time: start_time,
                start_time,
                mid_time: start_time,
                end_time,
            },
            BotRenderInfo::VShape {
                sprite,
                start_time,
                mid_time,
                end_time,
                start_pos,
                mid_pos,
                end_pos,
            } => Self {
                start_pos: start_pos.into(),
                mid_pos: mid_pos.into(),
                end_pos: end_pos.into(),
                wait_start_time: start_time,
                start_time,
                mid_time,
                end_time,
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
            } => Self {
                start_pos: start_pos.into(),
                mid_pos: mid_pos.into(),
                end_pos: end_pos.into(),
                wait_start_time,
                start_time,
                mid_time,
                end_time,
            },
        }
    }
}
