use core::time;
use cpu::{Cpu, CpuStepResult};
use pixels::{Error, Pixels, SurfaceTexture};
use ppu::Ppu;
use std::{env, process::exit, thread};
use winit::{
    dpi::{LogicalSize, Pixel},
    event::{self, Event, VirtualKeyCode},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
};
use winit_input_helper::WinitInputHelper;

mod addresses;
mod cpu;
mod ppu;
mod gui;

const GB_WIDTH: u32 = 160;
const GB_HEIGHT: u32 = 144;

const WINDOW_WIDTH: u32 = 640;
const WINDOW_HEIGHT: u32 = 480;
const WINDOW_TITLE: &str = "gb_emu";

fn init_emulator() -> (Cpu, Ppu) {
    let args: Vec<_> = env::args().collect();
    if args.len() < 2 {
        println!("Must specify rom file");
        exit(1);
    }

    let filename = args[1].as_str();
    let mut cpu = Cpu::new();
    cpu.load_rom(filename);
    (cpu, Ppu::new())
}

fn main() {
    let (mut cpu, mut ppu) = init_emulator();

    let event_loop = EventLoop::new();
    let mut input = WinitInputHelper::new();
    let window = {
        let size = LogicalSize::new(WINDOW_WIDTH as f64, WINDOW_HEIGHT as f64);
        let min_size = LogicalSize::new(GB_WIDTH as f64, GB_HEIGHT as f64);
        WindowBuilder::new()
            .with_title(WINDOW_TITLE)
            .with_inner_size(size)
            .with_min_inner_size(min_size)
            .build(&event_loop)
            .unwrap()
    };

    let mut pixels = {
        let window_size = window.inner_size();
        let scale_factor = window.scale_factor() as f32;
        let surface_texture = SurfaceTexture::new(window_size.width, window_size.height, &window);
        let pixels = Pixels::new(GB_WIDTH, GB_HEIGHT, surface_texture).unwrap();

        pixels
    };

    //**timing stuff**
    //4.194304 MHz
    //238.4185791015625 nanoseconds per cycle (ns)
    //about 4194.304 cycles in 1ms
    //20972 is about 5ms -> we'll use this as cycles per sleep
    //1 nop takes 4 cycles
    let cycle_duration = time::Duration::from_nanos(238);
    let cycles_per_yield = 20_000u32;
    let mut cycle_count_since_last_yield = 0u32;

    let mut total_cycles = 0;
    let mut is_gui_active = true;

    event_loop.run(move |event, _, control_flow| {
        //poll mode allows us to choose when to render out an image, as well as how long to sleep
        //in between loop cycles
        control_flow.set_poll();

        if input.update(&event) {
            //all _input_ events have been collected and can be queried at any time
            if input.key_pressed(VirtualKeyCode::Escape) {
                is_gui_active = !is_gui_active;
            }

            if input.quit() {
                control_flow.set_exit();
            }

            if let Some(scale_factor) = input.scale_factor() {
                //todo update scaling factor in GUI
            }

            if let Some(size) = input.window_resized() {
                //resize pixels and gui here
            }

            //todo: fetch joypad input here (keyboard or controller possibly) => update interrupts

            //process emulator cycles until a frame is ready
            let mut cycle_cost = 0;
            loop {
                match cpu.do_step() {
                    CpuStepResult::Stopped => {
                        control_flow.set_exit();
                        break;
                    }
                    CpuStepResult::CyclesExecuted(cycles) => cycle_cost = cycles,
                }
                total_cycles += cycle_cost;

                let ppu_step_result = ppu.do_step(&mut cpu, cycle_cost);

                match ppu_step_result {
                    ppu::PpuStepResult::NoAction => {}
                    ppu::PpuStepResult::Draw => {
                        //will be only triggered on Draw PPU response
                        window.request_redraw();
                        break;
                    }
                }
            }

            //todo:
            // should be using a timer, subtracting time used to actually process instruction
            // then only spin waiting for the time remaining
            // always use multiples of 4 cycles, this will make timing a bit easier
            // all instructions take multiples of 4 cycles

            // spin_sleep::sleep(cycle_duration * cycle_cost);
            cycle_count_since_last_yield += cycle_cost;

            // this is so that the emulator doesn't hog the cpu and get punished
            // by the scheduler
            if cycle_count_since_last_yield >= cycles_per_yield {
                cycle_count_since_last_yield = 0;
                // thread::yield_now();
            }
        }

        match event {
            Event::WindowEvent { event, .. } => {
                if is_gui_active {
                    // todo pass to gui
                }
            }
            Event::RedrawRequested(_) => {
                // todo pixels and gui rendering
                // respect is_gui_active

                // draw image to 'pixels' buffer and flip buffer
                //might turn this into a 'get image' func and have pixels lib
                //interfaced with outside of ppu
                let gb_pixels = ppu.get_pixels();
                let frame = pixels.get_frame_mut();
                for (gb_ind, gb_pixel) in gb_pixels.iter().enumerate() {
                    let pixel_ind = gb_ind * 4;
                    let pixel = &mut frame[pixel_ind..pixel_ind + 4];

                    match gb_pixel {
                        ppu::PixelShade::White => pixel.copy_from_slice(&[0xff, 0xff, 0xff, 0xff]),
                        ppu::PixelShade::Light => pixel.copy_from_slice(&[0xaa, 0xaa, 0xaa, 0xff]),
                        ppu::PixelShade::Medium => pixel.copy_from_slice(&[0x66, 0x66, 0x66, 0xff]),
                        ppu::PixelShade::Dark => pixel.copy_from_slice(&[0x22, 0x22, 0x22, 0xff]),
                    }
                }

                let render_result = pixels.render_with(|encoder, render_target, context| {
                    context.scaling_renderer.render(encoder, render_target);
                    Ok(())
                });

                if let Err(err) = render_result {
                    println!("pixels.render() failed: {err}");
                    control_flow.set_exit();
                }

                println!("draw");
            }
            Event::LoopDestroyed => {
                println!("Total cycles emulated: {:}", total_cycles);
            }
            _ => (),
        }
    });
}
