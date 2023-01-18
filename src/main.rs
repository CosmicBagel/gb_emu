use std::{fs::read, process::exit};

struct Cpu {
    //registers
    // 16-bit	Hi	Lo	Name/Function
    // 	   AF	A	-	Accumulator & Flags
    // 	   BC	B	C	BC
    // 	   DE	D	E	DE
    // 	   HL	H	L	HL
    // 	   SP	-	-	Stack Pointer
    // 	   PC	-	-	Program Counter/Pointer
    // 3 bit indicator for registers (0-7)
    // 0 => B, 1 => C, 2 => D, 3 => E, 4 => H, 5 => L, 6 => HL, 7 => A
    // note when HL is used, it means the byte the address HL is pointing to (indirect)
    a: u8,
    //flags (lower 8 bits of AF)
    // 	Bit	Name	Explanation
    // 	7	z		Zero flag
    // 	6	n		Subtraction flag (BCD)
    // 	5	h		Half Carry flag (BCD)
    // 	4	c		Carry flag
    f: u8, // the flags register

    b: u8,
    c: u8,

    d: u8,
    e: u8,

    h: u8,
    l: u8,

    sp: usize,
    pc: usize,

    // memory
    mem: Vec<u8>,
}

impl Cpu {
    fn new() -> Cpu {
        Cpu {
            a: 0x00,
            f: 0x00,
            b: 0x00,
            c: 0x00,
            d: 0x00,
            e: 0x00,
            h: 0x00,
            l: 0x00,
            sp: 0x0000,
            pc: 0x0100,

            mem: vec![0; 0xffff],
        }
    }

    fn load_rom(&mut self, filename: &str) {
        let rom = read(filename).unwrap();

        if rom.len() < 0x0100 {
            // first 256 bytes is header, any rom with less than this is invalid
            // really anything shorter than 512 bytes is probably invalid
            panic!("Rom is of invalid size ({} bytes)", rom.len());
        }

        // copy_from_slice doesn't work since both vectors need to be the same
        // size, using a loop instead
        for i in 0..rom.len() {
            self.mem[i] = rom[i];
        }
    }

    fn run(&mut self) {
        loop {
            // 0x7fff is the highest rom address, we'll halt on this
            // unless there's a reason to allow it
            if self.pc > 0x7fff {
                println!("attempted to execute outside of rom space");
                exit(0);
            }

            println!("{:#02x}: {:02x}", self.pc, self.mem[self.pc]);

            match self.mem[self.pc] {
                //nop
                0x00 => {
                    self.pc += 1;
                    continue;
                }
                // jump
                0xc3 => {
                    let target =
                        ((self.mem[self.pc + 2] as u16) << 8) | self.mem[self.pc + 1] as u16;
                    self.pc = target as usize;
                }
                //xor
                _ => panic!(
                    "unhandled instruction '0x{:02x}'\n{}",
                    self.mem[self.pc],
                    self.dump_registers()
                ),
            }
        }
    }

    fn dump_registers(&self) -> String {
        format!(
            "A: {:#02x}\tF: {:#02x}
B: {:#02x}\tC: {:#02x}
D: {:#02x}\tE: {:#02x}
H: {:#02x}\tL: {:#02x}
SP: {:#x}
PC: {:#x}",
            self.a, self.f, self.b, self.c, self.d, self.e, self.h, self.l, self.sp, self.pc
        )
    }
}

fn main() {
    /*
    - load rom
        - actual rom starts at 0x0100 usually a nop followed by a jmp to 0x0150
        - check if cgb rom (only compatible with gb roms)
            - 0x0143 => 0x80 game supports gbc, but is backwards compatible
                        0xC0 game only supports gbc (hardware ignores bit 6 so
                            this is the same as 0x80)
                        values with bit 7 set either 2 or 3 will switch the gameboy
                            to  PGB mode, which is not understood or documented currently
        - header checksum 0x014D
        - checksum 0x014E-0x14F 16 bit big endian => literal sum of all bytes on the rom, except the
            checksum bytes
    - opcodes use little endian, little endian in memory as well
    - registers are always big endian? (program counter, stack pointer?)
        endianness does not apply to registers (apparently)

    - registers can be accessed by single byte opcodes
        0 => B, 1 => C, 2 => D, 3 => E, 4 => H, 5 => L, 6 => HL, 7 => A
        seems to work this way for all 8 bit arithmetic opcodes where you can specify a register

    - process instructions byte by byte? front to back?
        - rom memory addressing?
    - registers
        - general registers
        - timer and divider registers
    - interrupts (can be disabled!)

    - some things might not work if expecting state left over from boot process

     */

    let filename = "tetris.gb";
    let mut cpu = Cpu::new();
    cpu.load_rom(filename);
    cpu.run();
}
