use core::time;
use cpu::{Cpu, CpuStepResult};
use ppu::Ppu;
use std::{env, process::exit, thread};

mod cpu;
mod ppu;



fn init() -> (Cpu, Ppu) {
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
    let (mut cpu, mut ppu) = init();

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

    cpu.load_rom(filename);
    loop {
        //todo: fetch joypad input here (keyboard or controller possibly) => update interrupts

        let cycle_cost;
        match cpu.do_step() {
            CpuStepResult::Stopped => break,
            CpuStepResult::CyclesExecuted(cycles) => cycle_cost = cycles,
        }
        total_cycles += cycle_cost;

        let ppu_step_result = ppu.do_step(&mut cpu, cycle_cost);

        match ppu_step_result {
            ppu::PpuStepResult::NoAction => {}
            ppu::PpuStepResult::Draw => {
                // draw image to 'pixels' buffer and flip buffer
                //might turn this into a 'get image' func and have pixels lib
                //interfaced with outside of ppu
                let gb_pixels = ppu.get_pixels();
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

    println!("Total cycles emulated: {:}", total_cycles);
}
