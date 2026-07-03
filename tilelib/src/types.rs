// FIXME:
#![allow(clippy::unwrap_used)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::cast_precision_loss)]
#![allow(clippy::cast_possible_truncation)]

use std::{
    borrow::Borrow,
    collections::HashMap,
    mem,
    sync::{atomic::AtomicU32, Arc, Mutex},
};

use image::GenericImageView;
use itertools::Itertools;
use log::{info, trace, warn};
use wgpu::{
    util::DeviceExt,
    wgt::{CommandEncoderDescriptor, TextureDescriptor},
    BackendOptions, Backends, BufferUsages, ExperimentalFeatures, Extent3d, InstanceDescriptor,
    InstanceFlags, MemoryBudgetThresholds, Operations, RenderPass,
    RenderPassDepthStencilAttachment, Sampler, TexelCopyBufferInfo, TexelCopyBufferLayout,
    TexelCopyTextureInfo, TextureFormat, TextureUsages, TextureView,
};

// Main Thread only
pub struct Display {
    sprite_shader: wgpu::ShaderModule,

    config: wgpu::SurfaceConfiguration,

    pub renderer: Option<Renderer>,

    // This needs to be after renderer, so it is dropped after it (since the Renderer includes a view into the Surface Texture)
    surface: wgpu::Surface<'static>,
}

impl Display {
    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub fn from_winit(window: Arc<winit::window::Window>) -> Self {
        let size = window.inner_size();

        assert!(size.width > 0, "Width of window must be greater than 0");
        assert!(size.height > 0, "Height of window must be greater than 0");

        // The instance is a handle to our GPU
        // Backends::all => Vulkan + Metal + DX12 + Browser WebGPU
        let instance = wgpu::Instance::new(InstanceDescriptor {
            backends: Backends::BROWSER_WEBGPU,
            flags: InstanceFlags::default(),
            memory_budget_thresholds: MemoryBudgetThresholds::default(),
            backend_options: BackendOptions::default(),
            display: None,
        });

        let surface = instance.create_surface(window).unwrap();

        // TODO: For now we just block on this future, maybe more efficient in the future to have actual async?
        let adapter = pollster::block_on(instance.request_adapter(&wgpu::RequestAdapterOptions {
            power_preference: wgpu::PowerPreference::default(),
            compatible_surface: Some(&surface),
            force_fallback_adapter: false,
        }))
        .unwrap();

        info!("Using adapter {}", adapter.get_info().name);

        let (device, queue) = pollster::block_on(adapter.request_device(&wgpu::DeviceDescriptor {
            required_features: wgpu::Features::empty(),
            // WebGL doesn't support all of wgpu's features, so if
            // we're building for the web, we'll have to disable some.
            required_limits: if cfg!(target_arch = "wasm32") {
                wgpu::Limits::downlevel_webgl2_defaults()
            } else {
                wgpu::Limits::default()
            },
            label: None,
            memory_hints: wgpu::MemoryHints::default(),
            trace: wgpu::Trace::Off,

            experimental_features: ExperimentalFeatures::disabled(),
        }))
        .unwrap();

        let surface_caps = surface.get_capabilities(&adapter);
        // Shader code in this tutorial assumes an sRGB surface texture. Using a different
        // one will result in all the colors coming out darker. If you want to support non
        // sRGB surfaces, you'll need to account for that when drawing to the frame.
        let surface_format = surface_caps
            .formats
            .iter()
            .find(|f| f.is_srgb())
            .copied()
            .unwrap_or(surface_caps.formats[0]);

        let config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: surface_format,
            width: size.width,
            height: size.height,
            present_mode: surface_caps.present_modes[0],
            alpha_mode: surface_caps.alpha_modes[0],
            view_formats: vec![],
            desired_maximum_frame_latency: 2,
        };

        let texture_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            multisampled: false,
                            view_dimension: wgpu::TextureViewDimension::D2,
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        // This should match the filterable field of the
                        // corresponding Texture entry above.
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                ],
                label: Some("texture_bind_group_layout"),
            });

        let shader = device.create_shader_module(wgpu::include_wgsl!("shader.wgsl"));

        let render_pipeline_layout =
            device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: Some("Render Pipeline Layout"),
                bind_group_layouts: &[Some(&texture_bind_group_layout)],
                immediate_size: 0,
            });

        let render_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Render Pipeline"),
            layout: Some(&render_pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: Some("vs_main"),            // 1.
                buffers: &[vertex_desc(), index_desc()], // 2.
                compilation_options: wgpu::PipelineCompilationOptions::default(),
            },
            fragment: Some(wgpu::FragmentState {
                // 3.
                module: &shader,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu::ColorTargetState {
                    // 4.
                    format: config.format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
                compilation_options: wgpu::PipelineCompilationOptions::default(),
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList, // 1.
                strip_index_format: None,
                front_face: wgpu::FrontFace::Ccw, // 2.
                cull_mode: Some(wgpu::Face::Back),
                // Setting this to anything other than Fill requires Features::NON_FILL_POLYGON_MODE
                polygon_mode: wgpu::PolygonMode::Fill,
                // Requires Features::DEPTH_CLIP_CONTROL
                unclipped_depth: false,
                // Requires Features::CONSERVATIVE_RASTERIZATION
                conservative: false,
            },
            depth_stencil: None, // 1.
            multisample: wgpu::MultisampleState {
                count: 1,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
            multiview_mask: None,
            cache: None,
        });

        surface.configure(&device, &config);

        let output = surface.get_current_texture();

        let output = match output {
            wgpu::CurrentSurfaceTexture::Success(surface_texture)
            | wgpu::CurrentSurfaceTexture::Suboptimal(surface_texture) => surface_texture,
            wgpu::CurrentSurfaceTexture::Timeout => todo!(),
            wgpu::CurrentSurfaceTexture::Occluded => todo!(),
            wgpu::CurrentSurfaceTexture::Outdated => todo!(),
            wgpu::CurrentSurfaceTexture::Lost => todo!(),
            wgpu::CurrentSurfaceTexture::Validation => todo!(),
        };

        let depth_texture = create_depth_texture(&device, &config, "depth_texture");

        let encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("Render Encoder"),
        });

        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Vertex Buffer"),
            contents: bytemuck::bytes_of(&[
                0.0f32, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0,
            ]),
            usage: wgpu::BufferUsages::VERTEX,
        });

        let index_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Index Buffer"),
            contents: bytemuck::cast_slice(&[0u16, 1, 2, 3, 4, 5]),
            usage: wgpu::BufferUsages::INDEX,
        });

        Self {
            surface,
            config,
            renderer: Some(Renderer {
                queue,
                device,
                encoder,
                output: Some(output),
                depth: Some(depth_texture.0),
                pipeline: render_pipeline,
                vertex_buffer,
                index_buffer,
                texture_bind_group_layout,
                textures: HashMap::new(),

                runtime_textures: HashMap::new(),
            }),

            sprite_shader: shader,
        }
    }

    pub fn resize(&mut self, new_size: winit::dpi::PhysicalSize<u32>) {
        if new_size.width > 0 && new_size.height > 0 {
            self.config.width = new_size.width;
            self.config.height = new_size.height;

            let Renderer {
                queue,
                device,
                output,
                depth,
                encoder,
                pipeline,
                vertex_buffer,
                index_buffer,
                texture_bind_group_layout,
                textures,

                runtime_textures,
            } = self.renderer.take().unwrap();

            mem::drop(output);
            mem::drop(depth);

            self.surface.configure(&device, &self.config);
            let output = self.surface.get_current_texture();

            let output = match output {
                wgpu::CurrentSurfaceTexture::Success(surface_texture)
                | wgpu::CurrentSurfaceTexture::Suboptimal(surface_texture) => surface_texture,
                wgpu::CurrentSurfaceTexture::Timeout => todo!(),
                wgpu::CurrentSurfaceTexture::Occluded => todo!(),
                wgpu::CurrentSurfaceTexture::Outdated => todo!(),
                wgpu::CurrentSurfaceTexture::Lost => todo!(),
                wgpu::CurrentSurfaceTexture::Validation => todo!(),
            };

            let depth = create_depth_texture(&device, &self.config, "depth_texture");

            self.renderer = Some(Renderer {
                device,
                queue,
                output: Some(output),
                depth: Some(depth.0),
                encoder,
                pipeline,
                vertex_buffer,
                index_buffer,
                texture_bind_group_layout,
                textures,

                runtime_textures,
            });
        }
    }

    pub fn get_renderer(&mut self) -> &mut Renderer {
        let Some(renderer) = self.renderer.as_mut() else {
            unreachable!()
        };

        renderer
    }

    pub fn finish_frame(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let Some(renderer) = self.renderer.take() else {
            unreachable!();
        };

        let Renderer {
            queue,
            device,
            output,
            depth,
            encoder,
            pipeline,
            vertex_buffer,
            index_buffer,
            texture_bind_group_layout,
            textures,

            runtime_textures,
        } = renderer;

        if let Some(output) = output {
            // submit will accept anything that implements IntoIter
            queue.submit(std::iter::once(encoder.finish()));
            output.present();
        } else {
            warn!("Could not finish frame, due to missing output!");
        }

        let encoder = device.create_command_encoder(&wgpu::CommandEncoderDescriptor {
            label: Some("Render Encoder"),
        });

        let output = self.surface.get_current_texture();

        let output = match output {
            wgpu::CurrentSurfaceTexture::Success(surface_texture)
            | wgpu::CurrentSurfaceTexture::Suboptimal(surface_texture) => Some(surface_texture),
            wgpu::CurrentSurfaceTexture::Timeout
            | wgpu::CurrentSurfaceTexture::Occluded
            | wgpu::CurrentSurfaceTexture::Outdated
            | wgpu::CurrentSurfaceTexture::Lost
            | wgpu::CurrentSurfaceTexture::Validation => None,
        };

        self.renderer = Some(Renderer {
            output,
            device,
            queue,
            depth,
            encoder,
            pipeline,
            vertex_buffer,
            index_buffer,
            texture_bind_group_layout,
            textures,

            runtime_textures,
        });

        Ok(())
    }

    // fn create_texture() -> Texture {}
}

pub struct Renderer {
    output: Option<wgpu::SurfaceTexture>,
    device: wgpu::Device,
    queue: wgpu::Queue,
    depth: Option<wgpu::Texture>,
    encoder: wgpu::CommandEncoder,
    pipeline: wgpu::RenderPipeline,
    vertex_buffer: wgpu::Buffer,
    index_buffer: wgpu::Buffer,

    texture_bind_group_layout: wgpu::BindGroupLayout,

    textures: HashMap<u32, wgpu::BindGroup>,

    runtime_textures: HashMap<usize, wgpu::Texture>,
}

impl RendererTrait for Renderer {
    #[allow(clippy::too_many_lines)]
    fn draw(&mut self, layer: &Layer) {
        if layer
            .sprite_commands
            .commands
            .values()
            .flat_map(|(_a, b)| b)
            .next()
            .is_none()
            && layer.runtime_commands.values().flatten().next().is_none()
        {
            // When we do not have anything to draw we must do an early return.
            // Buffer::slice will panic with a slice of length 0.
            return;
        }

        let diffuse_sampler = self.device.create_sampler(&wgpu::SamplerDescriptor {
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            address_mode_w: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Nearest,
            min_filter: wgpu::FilterMode::Nearest,
            mipmap_filter: wgpu::MipmapFilterMode::Nearest,
            ..Default::default()
        });

        let Some(output) = &self.output else {
            return;
        };

        let view = output
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());

        let Some(depth) = &self.depth else {
            return;
        };

        let depth_view = depth.create_view(&wgpu::TextureViewDescriptor::default());

        let mut render_pass = self.encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("layer renderer"),
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: &view,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Load,
                    store: wgpu::StoreOp::Store,
                },
                depth_slice: None,
            })],
            depth_stencil_attachment: Some(RenderPassDepthStencilAttachment {
                view: &depth_view,
                depth_ops: Some(Operations {
                    load: wgpu::LoadOp::Clear(1.0),
                    store: wgpu::StoreOp::Store,
                }),
                stencil_ops: None,
            }),
            timestamp_writes: None,
            occlusion_query_set: None,
            multiview_mask: None,
        });

        render_pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));

        // For each texture:
        for (texture_id, (sprite, instances)) in &layer.sprite_commands.commands {
            let bind_group = if let Some(texture) = self.textures.get(texture_id) {
                texture
            } else {
                trace!("Uploading texture {texture_id} to gpu");
                // Upload that texture:
                let texture_size = wgpu::Extent3d {
                    width: sprite.texture.dims.0,
                    height: sprite.texture.dims.1,
                    // All textures are stored as 3D, we represent our 2D texture
                    // by setting depth to 1.
                    depth_or_array_layers: 1,
                };

                let diffuse_texture = self.device.create_texture(&wgpu::TextureDescriptor {
                    size: texture_size,
                    mip_level_count: 1, // We'll talk about this a little later
                    sample_count: 1,
                    dimension: wgpu::TextureDimension::D2,
                    // Most images are stored using sRGB, so we need to reflect that here.
                    format: wgpu::TextureFormat::Rgba8UnormSrgb,
                    // TEXTURE_BINDING tells wgpu that we want to use this texture in shaders
                    // COPY_DST means that we want to copy data to this texture
                    usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
                    label: Some("diffuse_texture"),
                    // This is the same as with the SurfaceConfig. It
                    // specifies what texture formats can be used to
                    // create TextureViews for this texture. The base
                    // texture format (Rgba8UnormSrgb in this case) is
                    // always supported. Note that using a different
                    // texture format is not supported on the WebGL2
                    // backend.
                    view_formats: &[],
                });

                self.queue.write_texture(
                    // Tells wgpu where to copy the pixel data
                    wgpu::TexelCopyTextureInfo {
                        texture: &diffuse_texture,
                        mip_level: 0,
                        origin: wgpu::Origin3d::ZERO,
                        aspect: wgpu::TextureAspect::All,
                    },
                    // The actual pixel data
                    &sprite.texture.data,
                    // The layout of the texture
                    wgpu::TexelCopyBufferLayout {
                        offset: 0,
                        bytes_per_row: Some(4 * sprite.texture.dims.0),
                        rows_per_image: Some(sprite.texture.dims.1),
                    },
                    texture_size,
                );

                // We don't need to configure the texture view much, so let's
                // let wgpu define it.
                let diffuse_texture_view =
                    diffuse_texture.create_view(&wgpu::TextureViewDescriptor::default());

                let diffuse_bind_group =
                    self.device.create_bind_group(&wgpu::BindGroupDescriptor {
                        layout: &self.texture_bind_group_layout,
                        entries: &[
                            wgpu::BindGroupEntry {
                                binding: 0,
                                resource: wgpu::BindingResource::TextureView(&diffuse_texture_view),
                            },
                            wgpu::BindGroupEntry {
                                binding: 1,
                                resource: wgpu::BindingResource::Sampler(&diffuse_sampler),
                            },
                        ],
                        label: Some("diffuse_bind_group"),
                    });

                self.textures.insert(*texture_id, diffuse_bind_group);
                let Some(ret) = self.textures.get(texture_id) else {
                    unreachable!();
                };

                ret
            };

            render_pass.set_pipeline(&self.pipeline);

            let instance_data = &instances;
            // TODO: I doubt creating this buffer every frame is a great idea
            let instance_buffer =
                self.device
                    .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                        label: Some("Instance Buffer for layer"),
                        contents: bytemuck::cast_slice(instance_data),
                        usage: wgpu::BufferUsages::VERTEX,
                    });

            render_pass.set_vertex_buffer(1, instance_buffer.slice(..));
            render_pass.set_index_buffer(self.index_buffer.slice(..), wgpu::IndexFormat::Uint16);

            render_pass.set_bind_group(0, bind_group, &[]);

            render_pass.draw_indexed(0..6, 0, 0..instances.len().try_into().unwrap());
        }
    }

    fn get_aspect_ratio(&self) -> f32 {
        self.output.as_ref().map_or(1.0, |output_texture| {
            output_texture.texture.width() as f32 / output_texture.texture.height() as f32
        })
    }

    fn do_texture_updates<I: IntoIterator<Item = (usize, usize, [u8; 4])>>(
        &mut self,
        runtime_texture_id: usize,
        iter: I,
    ) {
        if !self.runtime_textures.contains_key(&runtime_texture_id) {
            return;
        }

        let data_layout = TexelCopyBufferLayout {
            offset: 0,
            bytes_per_row: None,
            rows_per_image: None,
        };
        let mut encoder = self
            .device
            .create_command_encoder(&CommandEncoderDescriptor { label: None });

        for (x, y, rbg) in iter {
            let buffer = self
                .device
                .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                    label: None,
                    contents: &rbg,
                    usage: BufferUsages::COPY_SRC,
                });

            encoder.copy_buffer_to_texture(
                TexelCopyBufferInfo {
                    buffer: &buffer,
                    layout: data_layout,
                },
                TexelCopyTextureInfo {
                    texture: &self.runtime_textures[&runtime_texture_id],
                    mip_level: 0,
                    origin: wgpu::Origin3d {
                        x: x.try_into().unwrap(),
                        y: y.try_into().unwrap(),
                        z: 0,
                    },
                    aspect: wgpu::TextureAspect::All,
                },
                wgpu::Extent3d {
                    width: 1,
                    height: 1,
                    depth_or_array_layers: 1,
                },
            );
        }
        self.queue.submit([encoder.finish()]);
    }

    fn has_runtime_texture(&self, runtime_texture_id: usize) -> bool {
        self.runtime_textures.contains_key(&runtime_texture_id)
    }

    fn create_runtime_texture_if_missing<B: Borrow<[u8]>>(
        &mut self,
        runtime_texture_id: usize,
        size: [usize; 2],
        initial_value: impl FnOnce() -> B,
    ) -> bool {
        if self.runtime_textures.contains_key(&runtime_texture_id) {
            return false;
        }

        let val = initial_value();

        assert_eq!(val.borrow().len(), 4 * size[0] * size[1]);

        let texture = self.device.create_texture(&TextureDescriptor {
            label: None,
            size: Extent3d {
                width: size[0] as u32,
                height: size[1] as u32,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: TextureFormat::Rgba8Uint,
            usage: TextureUsages::COPY_DST | TextureUsages::TEXTURE_BINDING,
            view_formats: &[],
        });

        self.queue.write_texture(
            // Tells wgpu where to copy the pixel data
            wgpu::TexelCopyTextureInfo {
                texture: &texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            // The actual pixel data
            val.borrow(),
            // The layout of the texture
            wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(4 * size[0] as u32),
                rows_per_image: Some(size[1] as u32),
            },
            Extent3d {
                width: size[0] as u32,
                height: size[1] as u32,
                depth_or_array_layers: 1,
            },
        );

        self.runtime_textures.insert(runtime_texture_id, texture);

        true
    }

    fn do_custom_draw(
        &mut self,
        fun: impl FnOnce(&wgpu::Device, &mut RenderPass, &wgpu::Queue, &wgpu::TextureFormat, &[u32; 2]),
    ) {
        let Some(output) = &self.output else {
            return;
        };

        let view = output
            .texture
            .create_view(&wgpu::TextureViewDescriptor::default());

        let Some(depth) = &self.depth else {
            return;
        };

        let depth_view = depth.create_view(&wgpu::TextureViewDescriptor::default());

        let mut render_pass = self.encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: Some("custom renderer"),
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: &view,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Load,
                    store: wgpu::StoreOp::Store,
                },
                depth_slice: None,
            })],
            depth_stencil_attachment: Some(RenderPassDepthStencilAttachment {
                view: &depth_view,
                depth_ops: Some(Operations {
                    load: wgpu::LoadOp::Clear(1.0),
                    store: wgpu::StoreOp::Store,
                }),
                stencil_ops: None,
            }),
            timestamp_writes: None,
            occlusion_query_set: None,
            multiview_mask: None,
        });
        (fun)(
            &self.device,
            &mut render_pass,
            &mut self.queue,
            &output.texture.format(),
            &[output.texture.size().width, output.texture.size().height],
        );
    }
}

#[derive(Debug, Clone)]
pub struct RawRenderer {
    queue: wgpu::Queue,
    device: wgpu::Device,
    target_format: wgpu::TextureFormat,

    render_pipeline: wgpu::RenderPipeline,

    vertex_buffer: wgpu::Buffer,
    index_buffer: wgpu::Buffer,

    texture_bind_group_layout: wgpu::BindGroupLayout,

    runtime_textures: Arc<Mutex<HashMap<usize, wgpu::Texture>>>,
    color_buffer_cache: Arc<Mutex<HashMap<[u8; 4], wgpu::Buffer>>>,
}

impl RawRenderer {
    #[must_use]
    pub fn new(
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        target_format: wgpu::TextureFormat,
    ) -> Self {
        // Shader code in this tutorial assumes an sRGB surface texture. Using a different
        // one will result in all the colors coming out darker. If you want to support non
        // sRGB surfaces, you'll need to account for that when drawing to the frame.
        let texture_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            multisampled: false,
                            view_dimension: wgpu::TextureViewDimension::D2,
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        // This should match the filterable field of the
                        // corresponding Texture entry above.
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                ],
                label: Some("texture_bind_group_layout"),
            });

        let shader = device.create_shader_module(wgpu::include_wgsl!("shader.wgsl"));

        let render_pipeline_layout =
            device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: Some("Render Pipeline Layout"),
                bind_group_layouts: &[Some(&texture_bind_group_layout)],
                immediate_size: 0,
            });

        let render_pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: Some("Render Pipeline"),
            layout: Some(&render_pipeline_layout),
            vertex: wgpu::VertexState {
                module: &shader,
                entry_point: Some("vs_main"),            // 1.
                buffers: &[vertex_desc(), index_desc()], // 2.
                compilation_options: wgpu::PipelineCompilationOptions::default(),
            },
            fragment: Some(wgpu::FragmentState {
                // 3.
                module: &shader,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu::ColorTargetState {
                    // 4.
                    format: target_format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
                compilation_options: wgpu::PipelineCompilationOptions::default(),
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList, // 1.
                strip_index_format: None,
                front_face: wgpu::FrontFace::Ccw, // 2.
                cull_mode: Some(wgpu::Face::Back),
                // Setting this to anything other than Fill requires Features::NON_FILL_POLYGON_MODE
                polygon_mode: wgpu::PolygonMode::Fill,
                // Requires Features::DEPTH_CLIP_CONTROL
                unclipped_depth: false,
                // Requires Features::CONSERVATIVE_RASTERIZATION
                conservative: false,
            },
            // depth_stencil: Some(DepthStencilState {
            //     format: DEPTH_FORMAT,
            //     depth_write_enabled: true,
            //     depth_compare: wgpu::CompareFunction::Less,
            //     stencil: StencilState::default(),
            //     bias: DepthBiasState::default(),
            // }), // 1.
            depth_stencil: None,
            multisample: wgpu::MultisampleState {
                count: 1,                         // 2.
                mask: !0,                         // 3.
                alpha_to_coverage_enabled: false, // 4.
            },
            multiview_mask: None, // 5.
            cache: None,          // 6.
        });

        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Vertex Buffer"),
            contents: bytemuck::bytes_of(&[
                0.0f32, 0.0, 0.0, 1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 1.0,
            ]),
            usage: wgpu::BufferUsages::VERTEX,
        });

        let index_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("Index Buffer"),
            contents: bytemuck::cast_slice(&[0u16, 1, 2, 3, 4, 5]),
            usage: wgpu::BufferUsages::INDEX,
        });

        Self {
            queue: queue.clone(),
            device: device.clone(),
            target_format,

            texture_bind_group_layout,

            render_pipeline,

            vertex_buffer,
            index_buffer,

            runtime_textures: Arc::new(Mutex::new(HashMap::new())),
            color_buffer_cache: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub const fn start_draw<'a, 'b, 'c>(
        &'a self,
        render_pass: &'b mut RenderPass<'c>,
        canvas_size: [u32; 2],
    ) -> InprogressRawRenderer<'a, 'b, 'c> {
        InprogressRawRenderer {
            raw_renderer: self,
            render_pass,
            canvas_size,
        }
    }

    pub fn reset_runtime_textures(&mut self) {
        self.runtime_textures = Arc::default();
    }
}

pub struct InprogressRawRenderer<'a, 'b, 'c> {
    raw_renderer: &'a RawRenderer,
    render_pass: &'b mut RenderPass<'c>,
    canvas_size: [u32; 2],
}

impl RendererTrait for InprogressRawRenderer<'_, '_, '_> {
    #[expect(clippy::too_many_lines)]
    // Clippy bug
    #[expect(clippy::significant_drop_tightening)]
    fn draw(&mut self, layer: &Layer) {
        if layer
            .sprite_commands
            .commands
            .values()
            .flat_map(|(_a, b)| b)
            .next()
            .is_none()
            && layer.runtime_commands.values().flatten().next().is_none()
        {
            // When we do not have anything to draw we must do an early return.
            // Buffer::slice will panic with a slice of length 0.
            return;
        }

        let diffuse_sampler = self
            .raw_renderer
            .device
            .create_sampler(&wgpu::SamplerDescriptor {
                address_mode_u: wgpu::AddressMode::ClampToEdge,
                address_mode_v: wgpu::AddressMode::ClampToEdge,
                address_mode_w: wgpu::AddressMode::ClampToEdge,
                mag_filter: wgpu::FilterMode::Nearest,
                min_filter: wgpu::FilterMode::Nearest,
                mipmap_filter: wgpu::MipmapFilterMode::Nearest,
                ..Default::default()
            });

        self.render_pass
            .set_vertex_buffer(0, self.raw_renderer.vertex_buffer.slice(..));

        // For each texture:
        for (texture_id, (sprite, instances)) in layer
            .sprite_commands
            .commands
            .iter()
            .sorted_by_key(|v| *v.0)
        {
            let bind_group = {
                trace!("Uploading texture {texture_id} to gpu");
                // Upload that texture:
                let texture_size = wgpu::Extent3d {
                    width: sprite.texture.dims.0,
                    height: sprite.texture.dims.1,
                    // All textures are stored as 3D, we represent our 2D texture
                    // by setting depth to 1.
                    depth_or_array_layers: 1,
                };

                let diffuse_texture =
                    self.raw_renderer
                        .device
                        .create_texture(&wgpu::TextureDescriptor {
                            size: texture_size,
                            mip_level_count: 1, // We'll talk about this a little later
                            sample_count: 1,
                            dimension: wgpu::TextureDimension::D2,
                            // Most images are stored using sRGB, so we need to reflect that here.
                            format: wgpu::TextureFormat::Rgba8Unorm,
                            // TEXTURE_BINDING tells wgpu that we want to use this texture in shaders
                            // COPY_DST means that we want to copy data to this texture
                            usage: wgpu::TextureUsages::TEXTURE_BINDING
                                | wgpu::TextureUsages::COPY_DST,
                            label: Some("diffuse_texture"),
                            // This is the same as with the SurfaceConfig. It
                            // specifies what texture formats can be used to
                            // create TextureViews for this texture. The base
                            // texture format (Rgba8UnormSrgb in this case) is
                            // always supported. Note that using a different
                            // texture format is not supported on the WebGL2
                            // backend.
                            view_formats: &[],
                        });

                self.raw_renderer.queue.write_texture(
                    // Tells wgpu where to copy the pixel data
                    wgpu::TexelCopyTextureInfo {
                        texture: &diffuse_texture,
                        mip_level: 0,
                        origin: wgpu::Origin3d::ZERO,
                        aspect: wgpu::TextureAspect::All,
                    },
                    // The actual pixel data
                    &sprite.texture.data,
                    // The layout of the texture
                    wgpu::TexelCopyBufferLayout {
                        offset: 0,
                        bytes_per_row: Some(4 * sprite.texture.dims.0),
                        rows_per_image: Some(sprite.texture.dims.1),
                    },
                    texture_size,
                );

                // We don't need to configure the texture view much, so let's
                // let wgpu define it.
                let diffuse_texture_view =
                    diffuse_texture.create_view(&wgpu::TextureViewDescriptor::default());

                let diffuse_bind_group =
                    self.raw_renderer
                        .device
                        .create_bind_group(&wgpu::BindGroupDescriptor {
                            layout: &self.raw_renderer.texture_bind_group_layout,
                            entries: &[
                                wgpu::BindGroupEntry {
                                    binding: 0,
                                    resource: wgpu::BindingResource::TextureView(
                                        &diffuse_texture_view,
                                    ),
                                },
                                wgpu::BindGroupEntry {
                                    binding: 1,
                                    resource: wgpu::BindingResource::Sampler(&diffuse_sampler),
                                },
                            ],
                            label: Some("diffuse_bind_group"),
                        });

                diffuse_bind_group
            };

            self.render_pass
                .set_pipeline(&self.raw_renderer.render_pipeline);

            let instance_data = &instances;
            // TODO: I doubt creating this buffer every frame is a great idea
            let instance_buffer =
                self.raw_renderer
                    .device
                    .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                        label: Some("Instance Buffer for layer"),
                        contents: bytemuck::cast_slice(instance_data),
                        usage: wgpu::BufferUsages::VERTEX,
                    });

            self.render_pass
                .set_vertex_buffer(1, instance_buffer.slice(..));
            self.render_pass.set_index_buffer(
                self.raw_renderer.index_buffer.slice(..),
                wgpu::IndexFormat::Uint16,
            );

            self.render_pass.set_bind_group(0, &bind_group, &[]);

            self.render_pass
                .draw_indexed(0..6, 0, 0..instances.len().try_into().unwrap());
        }

        let runtime_textures = self.raw_renderer.runtime_textures.lock().unwrap();
        for (texture_id, instances) in layer.runtime_commands.iter().sorted_by_key(|v| *v.0) {
            let texture = &runtime_textures[texture_id];
            let bind_group = {
                // We don't need to configure the texture view much, so let's
                // let wgpu define it.
                let diffuse_texture_view =
                    texture.create_view(&wgpu::TextureViewDescriptor::default());

                let diffuse_bind_group =
                    self.raw_renderer
                        .device
                        .create_bind_group(&wgpu::BindGroupDescriptor {
                            layout: &self.raw_renderer.texture_bind_group_layout,
                            entries: &[
                                wgpu::BindGroupEntry {
                                    binding: 0,
                                    resource: wgpu::BindingResource::TextureView(
                                        &diffuse_texture_view,
                                    ),
                                },
                                wgpu::BindGroupEntry {
                                    binding: 1,
                                    resource: wgpu::BindingResource::Sampler(&diffuse_sampler),
                                },
                            ],
                            label: Some("diffuse_bind_group"),
                        });

                diffuse_bind_group
            };

            self.render_pass
                .set_pipeline(&self.raw_renderer.render_pipeline);

            let instance_data = &instances;
            let instance_buffer =
                self.raw_renderer
                    .device
                    .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                        label: Some("Instance Buffer for layer"),
                        contents: bytemuck::cast_slice(instance_data),
                        usage: wgpu::BufferUsages::VERTEX,
                    });

            self.render_pass
                .set_vertex_buffer(1, instance_buffer.slice(..));
            self.render_pass.set_index_buffer(
                self.raw_renderer.index_buffer.slice(..),
                wgpu::IndexFormat::Uint16,
            );

            self.render_pass.set_bind_group(0, &bind_group, &[]);

            self.render_pass
                .draw_indexed(0..6, 0, 0..instances.len().try_into().unwrap());
        }
    }

    fn get_aspect_ratio(&self) -> f32 {
        self.canvas_size[0] as f32 / self.canvas_size[1] as f32
    }

    // Clippy bug
    #[expect(clippy::significant_drop_tightening)]
    fn do_texture_updates<I: IntoIterator<Item = (usize, usize, [u8; 4])>>(
        &mut self,
        runtime_texture_id: usize,
        iter: I,
    ) {
        if !self
            .raw_renderer
            .runtime_textures
            .lock()
            .unwrap()
            .contains_key(&runtime_texture_id)
        {
            return;
        }

        let data_layout = TexelCopyBufferLayout {
            offset: 0,
            bytes_per_row: None,
            rows_per_image: None,
        };

        let mut encoder = self
            .raw_renderer
            .device
            .create_command_encoder(&CommandEncoderDescriptor { label: None });

        let texture = &self.raw_renderer.runtime_textures.lock().unwrap()[&runtime_texture_id];

        let mut cache = self.raw_renderer.color_buffer_cache.lock().unwrap();

        for (x, y, rgb) in iter {
            let buffer = match cache.entry(rgb) {
                std::collections::hash_map::Entry::Occupied(occupied_entry) => {
                    occupied_entry.into_mut()
                },
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(self.raw_renderer.device.create_buffer_init(
                        &wgpu::util::BufferInitDescriptor {
                            label: None,
                            contents: &rgb,
                            usage: BufferUsages::COPY_SRC,
                        },
                    ))
                },
            };

            encoder.copy_buffer_to_texture(
                TexelCopyBufferInfo {
                    buffer,
                    layout: data_layout,
                },
                TexelCopyTextureInfo {
                    texture,
                    mip_level: 0,
                    origin: wgpu::Origin3d {
                        x: x.try_into().unwrap(),
                        y: y.try_into().unwrap(),
                        z: 0,
                    },
                    aspect: wgpu::TextureAspect::All,
                },
                wgpu::Extent3d {
                    width: 1,
                    height: 1,
                    depth_or_array_layers: 1,
                },
            );
        }

        let command_buffer = encoder.finish();
        self.raw_renderer.queue.submit([command_buffer]);
    }

    fn has_runtime_texture(&self, runtime_texture_id: usize) -> bool {
        self.raw_renderer
            .runtime_textures
            .lock()
            .unwrap()
            .contains_key(&runtime_texture_id)
    }

    fn create_runtime_texture_if_missing<B: Borrow<[u8]>>(
        &mut self,
        runtime_texture_id: usize,
        size: [usize; 2],
        initial_value: impl FnOnce() -> B,
    ) -> bool {
        if self
            .raw_renderer
            .runtime_textures
            .lock()
            .unwrap()
            .contains_key(&runtime_texture_id)
        {
            return false;
        }

        let val = initial_value();

        let texture = self.raw_renderer.device.create_texture(&TextureDescriptor {
            label: None,
            size: Extent3d {
                width: size[0] as u32,
                height: size[1] as u32,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: TextureFormat::Rgba8Unorm,
            usage: TextureUsages::COPY_DST
                | TextureUsages::TEXTURE_BINDING
                | wgpu::TextureUsages::RENDER_ATTACHMENT,
            view_formats: &[],
        });

        self.raw_renderer.queue.write_texture(
            // Tells wgpu where to copy the pixel data
            wgpu::TexelCopyTextureInfo {
                texture: &texture,
                mip_level: 0,
                origin: wgpu::Origin3d::ZERO,
                aspect: wgpu::TextureAspect::All,
            },
            // The actual pixel data
            val.borrow(),
            // The layout of the texture
            wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(4 * size[0] as u32),
                rows_per_image: Some(size[1] as u32),
            },
            Extent3d {
                width: size[0] as u32,
                height: size[1] as u32,
                depth_or_array_layers: 1,
            },
        );

        self.raw_renderer
            .runtime_textures
            .lock()
            .unwrap()
            .insert(runtime_texture_id, texture);

        true
    }

    fn do_custom_draw(
        &mut self,
        fun: impl FnOnce(&wgpu::Device, &mut RenderPass, &wgpu::Queue, &wgpu::TextureFormat, &[u32; 2]),
    ) {
        (fun)(
            &self.raw_renderer.device,
            self.render_pass,
            &self.raw_renderer.queue,
            &self.raw_renderer.target_format,
            &self.canvas_size,
        );
    }
}

pub trait RendererTrait {
    fn draw(&mut self, layer: &Layer);
    fn get_aspect_ratio(&self) -> f32;
    fn create_runtime_texture_if_missing<B: Borrow<[u8]>>(
        &mut self,
        runtime_texture_id: usize,
        size: [usize; 2],
        initial_value: impl FnOnce() -> B,
    ) -> bool;
    fn has_runtime_texture(&self, runtime_texture_id: usize) -> bool;
    fn do_texture_updates<I: IntoIterator<Item = (usize, usize, [u8; 4])>>(
        &mut self,
        runtime_texture_id: usize,
        iter: I,
    );

    fn do_custom_draw(
        &mut self,
        fun: impl FnOnce(&wgpu::Device, &mut RenderPass, &wgpu::Queue, &wgpu::TextureFormat, &[u32; 2]),
    );
}

#[derive(Debug)]
pub struct Layer {
    x_mult: f32,
    y_mult: f32,
    sprite_commands: DrawSpriteCommands,
    runtime_commands: HashMap<usize, Vec<Instance>>,
}

enum LayerType {
    Transparent,
    Oquaque(wgpu::Color),
}

#[derive(Debug, Default)]
struct DrawSpriteCommands {
    commands: HashMap<u32, (Sprite, Vec<Instance>)>,
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
struct Instance {
    pub position: [f32; 2],
    pub size: [f32; 2],
    pub width_mul: f32,
    pub width_offs: f32,
}

#[derive(Debug, Clone, Copy)]
pub struct DrawInstance {
    pub position: [f32; 2],
    pub size: [f32; 2],
    pub animation_frame: u32,
}

impl Default for Layer {
    fn default() -> Self {
        Self::new()
    }
}

impl Layer {
    #[must_use]
    pub fn new() -> Self {
        Self {
            x_mult: 1.0,
            y_mult: 1.0,
            sprite_commands: DrawSpriteCommands::default(),
            runtime_commands: HashMap::default(),
        }
    }

    // TODO:
    #[expect(clippy::float_cmp)]
    pub fn extend(&mut self, other: Self) {
        assert_eq!(self.x_mult, other.x_mult);
        assert_eq!(self.y_mult, other.y_mult);
        for (k, v) in other.runtime_commands {
            match self.runtime_commands.entry(k) {
                std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                    occupied_entry.get_mut().extend(v);
                },
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(v);
                },
            }
        }
        for (k, v) in other.sprite_commands.commands {
            match self.sprite_commands.commands.entry(k) {
                std::collections::hash_map::Entry::Occupied(mut occupied_entry) => {
                    let (sprite, instances) = occupied_entry.get_mut();
                    instances.extend(v.1);
                },
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(v);
                },
            }
        }
    }

    #[must_use]
    pub fn square_tile_grid(tile_size: f32, aspect_ratio: f32) -> Self {
        // TODO: The tiles are not square if the draw surface is not square
        Self {
            x_mult: tile_size,
            y_mult: tile_size * aspect_ratio,
            sprite_commands: DrawSpriteCommands::default(),
            runtime_commands: HashMap::default(),
        }
    }

    /// # Panics
    /// If the instances animation frame is not in range for the sprites' texture
    pub fn draw_sprite(&mut self, sprite: &Sprite, instance: DrawInstance) {
        assert!(instance.animation_frame < sprite.texture.number_anim_frames);

        if let Some((a, b)) = self.sprite_commands.commands.get_mut(&sprite.texture.id) {
            b.push(Instance {
                position: [
                    instance.position[0] * self.x_mult,
                    instance.position[1] * self.y_mult,
                ],
                size: [
                    instance.size[0] * self.x_mult,
                    instance.size[1] * self.y_mult,
                ],
                #[allow(clippy::cast_precision_loss)]
                width_mul: 1.0 / sprite.texture.number_anim_frames as f32,
                #[allow(clippy::cast_precision_loss)]
                width_offs: instance.animation_frame as f32
                    / sprite.texture.number_anim_frames as f32,
            });
        } else {
            self.sprite_commands.commands.insert(
                sprite.texture.id,
                (
                    sprite.clone(),
                    vec![Instance {
                        position: [
                            instance.position[0] * self.x_mult,
                            instance.position[1] * self.y_mult,
                        ],
                        size: [
                            instance.size[0] * self.x_mult,
                            instance.size[1] * self.y_mult,
                        ],
                        #[allow(clippy::cast_precision_loss)]
                        width_mul: 1.0 / sprite.texture.number_anim_frames as f32,
                        #[allow(clippy::cast_precision_loss)]
                        width_offs: instance.animation_frame as f32
                            / sprite.texture.number_anim_frames as f32,
                    }],
                ),
            );
        }
    }

    /// # Panics
    /// If the instances animation frame is not in range for the sprites' texture
    pub fn draw_many_sprites(
        &mut self,
        sprite: &Sprite,
        instance: impl IntoIterator<Item = DrawInstance>,
    ) {
        if let Some((a, b)) = self.sprite_commands.commands.get_mut(&sprite.texture.id) {
            b.extend(instance.into_iter().map(|instance| {
                debug_assert!(instance.animation_frame < sprite.texture.number_anim_frames);
                Instance {
                    position: [
                        instance.position[0] * self.x_mult,
                        instance.position[1] * self.y_mult,
                    ],
                    size: [
                        instance.size[0] * self.x_mult,
                        instance.size[1] * self.y_mult,
                    ],
                    #[allow(clippy::cast_precision_loss)]
                    width_mul: 1.0 / sprite.texture.number_anim_frames as f32,
                    #[allow(clippy::cast_precision_loss)]
                    width_offs: instance.animation_frame as f32
                        / sprite.texture.number_anim_frames as f32,
                }
            }));
        } else {
            self.sprite_commands.commands.insert(
                sprite.texture.id,
                (
                    sprite.clone(),
                    instance
                        .into_iter()
                        .map(|instance| {
                            debug_assert!(
                                instance.animation_frame < sprite.texture.number_anim_frames
                            );
                            Instance {
                                position: [
                                    instance.position[0] * self.x_mult,
                                    instance.position[1] * self.y_mult,
                                ],
                                size: [
                                    instance.size[0] * self.x_mult,
                                    instance.size[1] * self.y_mult,
                                ],
                                #[allow(clippy::cast_precision_loss)]
                                width_mul: 1.0 / sprite.texture.number_anim_frames as f32,
                                #[allow(clippy::cast_precision_loss)]
                                width_offs: instance.animation_frame as f32
                                    / sprite.texture.number_anim_frames as f32,
                            }
                        })
                        .collect(),
                ),
            );
        }
    }

    pub fn draw_runtime_texture(&mut self, texture_id: usize, instance: DrawInstance) {
        if let Some(b) = self.runtime_commands.get_mut(&texture_id) {
            b.push(Instance {
                position: [
                    instance.position[0] * self.x_mult,
                    instance.position[1] * self.y_mult,
                ],
                size: [
                    instance.size[0] * self.x_mult,
                    instance.size[1] * self.y_mult,
                ],
                width_mul: 1.0,
                width_offs: 0.0,
            });
        } else {
            self.runtime_commands.insert(
                texture_id,
                vec![Instance {
                    position: [
                        instance.position[0] * self.x_mult,
                        instance.position[1] * self.y_mult,
                    ],
                    size: [
                        instance.size[0] * self.x_mult,
                        instance.size[1] * self.y_mult,
                    ],
                    width_mul: 1.0,
                    width_offs: 0.0,
                }],
            );
        }
    }
}

#[derive(Debug, Clone)]
pub struct Sprite {
    pub texture: Texture,
    custom_shader: Option<Arc<Shader>>,
}

#[derive(Debug)]
pub struct Shader {}

struct TextureAtlas {}

impl Sprite {
    #[must_use]
    pub const fn new(texture: Texture) -> Self {
        Self {
            texture,
            custom_shader: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Texture {
    id: u32,
    pub number_anim_frames: u32,
    data: Arc<[u8]>,
    dims: (u32, u32),
}

impl Default for Texture {
    fn default() -> Self {
        let diffuse_bytes = include_bytes!("image.png");
        let diffuse_image = image::load_from_memory(diffuse_bytes).unwrap();
        let diffuse_rgba = diffuse_image.to_rgba8().into_vec();

        let dimensions = diffuse_image.dimensions();

        Self::new(1, diffuse_rgba, dimensions)
    }
}

impl Texture {
    pub fn new(number_frames: u32, data: impl Into<Arc<[u8]>>, dims: (u32, u32)) -> Self {
        static TEXTURE_ID_COUNTER: AtomicU32 = AtomicU32::new(0);
        Self {
            id: TEXTURE_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
            number_anim_frames: number_frames,
            data: data.into(),
            dims,
        }
    }
}

fn vertex_desc() -> wgpu::VertexBufferLayout<'static> {
    wgpu::VertexBufferLayout {
        array_stride: (2 * std::mem::size_of::<f32>()).try_into().unwrap(), // 1.
        step_mode: wgpu::VertexStepMode::Vertex,                            // 2.
        attributes: &[
            // 3.
            wgpu::VertexAttribute {
                offset: 0,                             // 4.
                shader_location: 0,                    // 5.
                format: wgpu::VertexFormat::Float32x2, // 6.
            },
        ],
    }
}

fn index_desc() -> wgpu::VertexBufferLayout<'static> {
    wgpu::VertexBufferLayout {
        array_stride: std::mem::size_of::<Instance>().try_into().unwrap(), // 1.
        step_mode: wgpu::VertexStepMode::Instance,                         // 2.
        attributes: &[
            // 3.
            wgpu::VertexAttribute {
                offset: 0,                             // 4.
                shader_location: 1,                    // 5.
                format: wgpu::VertexFormat::Float32x2, // 6.
            },
            wgpu::VertexAttribute {
                offset: 2u64 * std::mem::size_of::<f32>() as u64, // 4.
                shader_location: 2,                               // 5.
                format: wgpu::VertexFormat::Float32x2,            // 6.
            },
            wgpu::VertexAttribute {
                offset: 4 * std::mem::size_of::<f32>() as u64,
                shader_location: 3,
                format: wgpu::VertexFormat::Float32,
            },
            wgpu::VertexAttribute {
                offset: 5 * std::mem::size_of::<f32>() as u64,
                shader_location: 4,
                format: wgpu::VertexFormat::Float32,
            },
        ],
    }
}

pub const DEPTH_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Depth32Float; // 1.

#[must_use]
pub fn create_depth_texture(
    device: &wgpu::Device,
    config: &wgpu::SurfaceConfiguration,
    label: &str,
) -> (wgpu::Texture, TextureView, Sampler) {
    let size = wgpu::Extent3d {
        // 2.
        width: config.width.max(1),
        height: config.height.max(1),
        depth_or_array_layers: 1,
    };
    let desc = wgpu::TextureDescriptor {
        label: Some(label),
        size,
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format: DEPTH_FORMAT,
        usage: wgpu::TextureUsages::RENDER_ATTACHMENT // 3.
                | wgpu::TextureUsages::TEXTURE_BINDING,
        view_formats: &[],
    };
    let texture = device.create_texture(&desc);

    let view = texture.create_view(&wgpu::TextureViewDescriptor::default());
    let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
        // 4.
        address_mode_u: wgpu::AddressMode::ClampToEdge,
        address_mode_v: wgpu::AddressMode::ClampToEdge,
        address_mode_w: wgpu::AddressMode::ClampToEdge,
        mag_filter: wgpu::FilterMode::Linear,
        min_filter: wgpu::FilterMode::Linear,
        mipmap_filter: wgpu::MipmapFilterMode::Nearest,
        compare: Some(wgpu::CompareFunction::LessEqual), // 5.
        lod_min_clamp: 0.0,
        lod_max_clamp: 100.0,
        ..Default::default()
    });

    (texture, view, sampler)
}
