//ppu = picture processing unit
use crate::cpu::Cpu;

pub struct Ppu {
    v_ram: Vec<u8>,
}

impl Ppu {
    pub fn new() -> Ppu{
        Ppu {
            v_ram: vec![0; 0x10_000],
        }
    }

    pub fn do_step(&mut self, cpu: &mut Cpu) {

    }
}
