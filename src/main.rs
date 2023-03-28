use addresses::INTERRUPT_FLAG_ADDRESS;
use constants::*;
// use core::time;
use cpu::{Cpu, CpuStepResult, InputState};
use pixels::{PixelsBuilder, SurfaceTexture};
use ppu::Ppu;
use simple_logger::SimpleLogger;
use std::{env, thread, time};
use winit::{
    dpi::LogicalSize,
    event::{Event, VirtualKeyCode},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
};
use winit_input_helper::WinitInputHelper;

mod addresses;
mod constants;
mod cpu;
// mod gui;
mod ppu;

fn init_emulator() -> (Cpu, Ppu) {
    let args: Vec<_> = env::args().collect();
    let filename = if args.len() < 2 {
        //this is to make debugging easier to deal with
        log::info!("Using default rom file");
        DEFAULT_ROM
    } else {
        args[1].as_str()
    };

    let mut cpu = Cpu::new();
    cpu.load_rom(filename);
    (cpu, Ppu::new())
}

fn main() {
    SimpleLogger::new()
        .with_colors(true)
        .with_level(log::LevelFilter::Error)
        .with_module_level("gb_emu", log::LevelFilter::Debug)
        .init()
        .unwrap();
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
        // let scale_factor = window.scale_factor() as f32;
        let surface_texture = SurfaceTexture::new(window_size.width, window_size.height, &window);
        let pixels = PixelsBuilder::new(GB_WIDTH as u32, GB_HEIGHT as u32, surface_texture)
            .enable_vsync(false)
            .build()
            .unwrap();

        pixels
    };

    //**timing stuff**
    //4.194304 MHz
    //238.4185791015625 nanoseconds per cycle (ns)
    //about 4194.304 cycles in 1ms
    //20972 is about 5ms -> we'll use this as cycles per sleep
    //1 nop takes 4 cycles
    // let cycle_duration = time::Duration::from_nanos(238);
    //target is 16.742706298828125ms
    let target_loop_duration = time::Duration::from_nanos(16742706);
    let mut turbo = false;
    let cycles_per_yield = 20_000u32;
    let mut cycle_count_since_last_yield = 0u32;

    let mut is_gui_active = IS_GUI_ACTIVE_DEFAULT;

    event_loop.run(move |event, _, control_flow| {
        let start_time = time::Instant::now();
        //poll mode allows us to choose when to render out an image, as well as how long to sleep
        //in between loop cycles
        control_flow.set_poll();

        if input.update(&event) {
            //all _input_ events have been collected and can be queried at any time
            if input.key_pressed(VirtualKeyCode::Escape) {
                is_gui_active = !is_gui_active;
            }

            turbo = input.held_shift();

            if input.close_requested() {
                control_flow.set_exit();
            }

            // if let Some(scale_factor) = input.scale_factor() {
            //todo update scaling factor in GUI
            // }

            if let Some(size) = input.window_resized() {
                //resize pixels and gui here
                if let Err(err) = pixels.resize_surface(size.width, size.height) {
                    log::error!("pixels.resize_surface() failed: {err}");
                    *control_flow = ControlFlow::Exit;
                    return;
                }
            }

            //reset before input is checked again
            cpu.input_state.a_button_was_pressed = false;
            handle_keyboard_input(&input, &mut cpu.input_state);
            //todo gamepad input here
            update_joypad_interrupt(&mut cpu);

            //initial joypad update (will also be updated when JOYPAD register is written to)
            cpu.update_joypad();

            //process emulator cycles until a frame is ready
            let mut frame_cycles = 0;
            loop {
                let cycle_cost;
                match cpu.do_step() {
                    CpuStepResult::Stopped => {
                        control_flow.set_exit();
                        break;
                    }
                    CpuStepResult::CyclesExecuted(cycles) => cycle_cost = cycles,
                }
                frame_cycles += cycle_cost;

                let ppu_step_result = ppu.do_step(&mut cpu, cycle_cost);

                match ppu_step_result {
                    ppu::PpuStepResult::NoAction => {}
                    ppu::PpuStepResult::Draw => {
                        //will be only triggered on Draw PPU response
                        window.request_redraw();
                        break;
                    }
                }

                // don't let the emulator hang in case of bug or something weird
                if frame_cycles >= CLOCKS_PER_FRAME {
                    log::warn!("Forcing frame draw!!! ppu should be triggering a frame");
                    window.request_redraw();
                    break;
                }
            }

            //todo:
            // should be using a timer, subtracting time used to actually process instruction
            // then only spin waiting for the time remaining
            // always use multiples of 4 cycles, this will make timing a bit easier
            // all instructions take multiples of 4 cycles

            // spin_sleep::sleep(cycle_duration * cycle_cost);
            cycle_count_since_last_yield += frame_cycles;

            // this is so that the emulator doesn't hog the cpu and get punished
            // by the scheduler
            if cycle_count_since_last_yield >= cycles_per_yield {
                cycle_count_since_last_yield = 0;
                // thread::yield_now();
            }
        }

        match event {
            Event::WindowEvent { .. } => {
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
                let frame = pixels.frame_mut();
                for (gb_ind, gb_pixel) in gb_pixels.iter().enumerate() {
                    const PIXEL_BYTES: usize = 4;
                    let pixel_ind = gb_ind * PIXEL_BYTES;
                    let pixel = &mut frame[pixel_ind..pixel_ind + PIXEL_BYTES];

                    match gb_pixel {
                        ppu::PixelShade::White => pixel.copy_from_slice(&WHITE_SHADE),
                        ppu::PixelShade::Light => pixel.copy_from_slice(&LIGHT_SHADE),
                        ppu::PixelShade::Medium => pixel.copy_from_slice(&MEDIUM_SHADE),
                        ppu::PixelShade::Dark => pixel.copy_from_slice(&DARK_SHADE),
                        ppu::PixelShade::Disabled => pixel.copy_from_slice(&DISABLED_SHADE),
                    }
                }

                let render_result = pixels.render_with(|encoder, render_target, context| {
                    context.scaling_renderer.render(encoder, render_target);
                    Ok(())
                });

                if let Err(err) = render_result {
                    log::error!("pixels.render() failed: {err}");
                    control_flow.set_exit();
                }

                if !turbo {
                    let elapsed = time::Instant::now() - start_time;
                    if elapsed < target_loop_duration {
                        thread::sleep(target_loop_duration - elapsed);
                    } else {
                        log::warn!("Slow update: {:?}", elapsed.as_millis());
                    }
                }
            }
            Event::LoopDestroyed => {}
            _ => (),
        }
    });
}

fn update_joypad_interrupt(cpu: &mut Cpu) {
    if cpu.input_state.a_button_was_pressed {
        let int_flags = cpu.read_hw_reg(INTERRUPT_FLAG_ADDRESS);
        cpu.write_hw_reg(INTERRUPT_FLAG_ADDRESS, int_flags | 0b0001_0000);
    }
}

fn handle_keyboard_input(input: &WinitInputHelper, input_state: &mut InputState) {
    if input.key_pressed(VirtualKeyCode::A) {
        input_state.left = true;
        input_state.a_button_was_pressed = true;
    }
    if input.key_released(VirtualKeyCode::A) {
        input_state.left = false;
    }

    if input.key_pressed(VirtualKeyCode::S) {
        input_state.down = true;
        input_state.a_button_was_pressed = true;
    }
    if input.key_released(VirtualKeyCode::S) {
        input_state.down = false;
    }

    if input.key_pressed(VirtualKeyCode::D) {
        input_state.right = true;
        input_state.a_button_was_pressed = true;
    }
    if input.key_released(VirtualKeyCode::D) {
        input_state.right = false;
    }

    if input.key_pressed(VirtualKeyCode::W) {
        input_state.up = true;
        input_state.a_button_was_pressed = true;
    }
    if input.key_released(VirtualKeyCode::W) {
        input_state.up = false;
    }

    if input.key_pressed(VirtualKeyCode::J) {
        input_state.b = true;
        input_state.a_button_was_pressed = true;
    }
    if input.key_released(VirtualKeyCode::J) {
        input_state.b = false;
    }

    if input.key_pressed(VirtualKeyCode::K) {
        input_state.a = true;
        input_state.a_button_was_pressed = true;
    }
    if input.key_released(VirtualKeyCode::K) {
        input_state.a = false;
    }

    if input.key_pressed(VirtualKeyCode::U) {
        input_state.select = true;
        input_state.a_button_was_pressed = true;
    }
    if input.key_released(VirtualKeyCode::U) {
        input_state.select = false;
    }

    if input.key_pressed(VirtualKeyCode::I) {
        input_state.start = true;
        input_state.a_button_was_pressed = true;
    }
    if input.key_released(VirtualKeyCode::I) {
        input_state.start = false;
    }
}
