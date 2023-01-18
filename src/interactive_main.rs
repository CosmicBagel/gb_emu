/*

- loading rom
- egui ui with pixels rendering
- display output in egui?
- make cpu
    - registers
    - timers (potentially use other thread to count)
    - instructions

pixels / egui setup based on
https://github.com/parasyte/pixels/tree/main/examples/minimal-egui
(its MIT licensed)

*/

use crate::gui::Framework;
use log::error;
use pixels::{Error, Pixels, SurfaceTexture};
use winit::dpi::{LogicalSize, Pixel};
use winit::event::{self, Event, VirtualKeyCode};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::WindowBuilder;
use winit_input_helper::WinitInputHelper;

mod gui;

const WIDTH: u32 = 640;
const HEIGHT: u32 = 480;
const BOX_SIZE: i16 = 64;

// representation of the application state
struct World {
    box_x: i16,
    box_y: i16,
    velocity_x: i16,
    velocity_y: i16,
}

impl World {
    // create a new 'world' instance that can draw a moving box
    fn new() -> Self {
        Self {
            box_x: 24,
            box_y: 16,
            velocity_x: 1,
            velocity_y: 1,
        }
    }

    // update the 'world' internal state; bounce the box around the screen
    fn update(&mut self) {
        if self.box_x <= 0 || self.box_x + BOX_SIZE > WIDTH as i16 {
            self.velocity_x *= -1;
        }
        if self.box_y <= 0 || self.box_y + BOX_SIZE > HEIGHT as i16 {
            self.velocity_y *= -1;
        }

        self.box_x += self.velocity_x;
        self.box_y += self.velocity_y;
    }

    // draw the 'world' state to the frame buffer
    // assumes the default texture format 'wgpu::TextureFormat::Rgba8UnormSrgb'
    fn draw(&self, frame: &mut [u8]) {
        for (i, pixel) in frame.chunks_exact_mut(4).enumerate() {
            let x = (i % WIDTH as usize) as i16;
            let y = (i / WIDTH as usize) as i16;

            let inside_the_box = x >= self.box_x
                && x < self.box_x + BOX_SIZE
                && y >= self.box_y
                && y < self.box_y + BOX_SIZE;

            let rgba = if inside_the_box {
                [0x5e, 0x48, 0xe8, 0xff]
            } else {
                [0x48, 0xb2, 0xe8, 0xff]
            };

            pixel.copy_from_slice(&rgba);
        }
    }
}

fn main() -> Result<(), Error> {
    println!("Hello, world!");
    env_logger::init();
    let event_loop = EventLoop::new();
    let mut input = WinitInputHelper::new();
    let window = {
        let size = LogicalSize::new(WIDTH as f64, HEIGHT as f64);
        WindowBuilder::new()
            .with_title("Hello Pixels + egui")
            .with_inner_size(size)
            .with_min_inner_size(size)
            .build(&event_loop)
            .unwrap()
    };

    let (mut pixels, mut framework) = {
        let window_size = window.inner_size();
        let scale_factor = window.scale_factor() as f32;
        let surface_texture = SurfaceTexture::new(window_size.width, window_size.height, &window);
        let pixels = Pixels::new(WIDTH, HEIGHT, surface_texture)?;
        let framework = Framework::new(
            &event_loop,
            window_size.width,
            window_size.height,
            scale_factor,
            &pixels,
        );

        (pixels, framework)
    };
    let mut world = World::new();

    event_loop.run(move |event, _, control_flow| {
        // handle input events
        if input.update(&event) {
            // close event
            if input.key_pressed(VirtualKeyCode::Escape) || input.quit() {
                *control_flow = ControlFlow::Exit;
                return;
            }

            //update scale factor
            if let Some(scale_factor) = input.scale_factor() {
                framework.scale_factor(scale_factor);
            }

            // resize window event
            if let Some(size) = input.window_resized() {
                if let Err(err) = pixels.resize_surface(size.width, size.height) {
                    error!("pixels.resize_surface() failed: {err}");
                    *control_flow = ControlFlow::Exit;
                    return;
                }
                framework.resize(size.width, size.height);
            }

            // update internal state and request a redraw
            world.update();
            window.request_redraw();
        }

        match event {
            Event::WindowEvent { event, .. } => {
                // update egui inputs
                framework.handle_event(&event);
            }

            //draw the current frame
            Event::RedrawRequested(_) => {
                // draw the world
                world.draw(pixels.get_frame_mut());

                // prepare egui
                framework.prepare(&window);

                // render everything together
                let render_result = pixels.render_with(|encoder, render_target, context| {
                    //render the world texture
                    context.scaling_renderer.render(encoder, render_target);

                    //render egui
                    framework.render(encoder, render_target, context);

                    Ok(())
                });

                // basic error handling
                if let Err(err) = render_result {
                    error!("pixels.render() failed: {err}");
                    *control_flow = ControlFlow::Exit;
                }
            }
            _ => (),
        }
    });
}
