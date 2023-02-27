use crate::cpu::Cpu;

pub struct Video {
    v_ram: Vec<u8>,
}

impl Video {
    pub fn new() -> Video{
        Video {
            v_ram: vec![0; 0x10_000],
        }
    }

    pub fn do_step(&mut self, cpu: &mut Cpu) {

    }
}
