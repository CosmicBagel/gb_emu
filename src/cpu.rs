use spin_sleep;
use std::{fs::read, process::exit, thread, time};

pub type CycleCount = u32;
type BytecodeTable = [fn(&mut Cpu, u8) -> CycleCount; 255];

pub struct Cpu {
    //registers
    // 16-bit	Hi	Lo	Name/Function
    // 	   AF	A	-	Accumulator & Flags
    // 	   BC	B	C	BC
    // 	   DE	D	E	DE
    // 	   HL	H	L	HL
    // 	   SP	-	-	Stack Pointer
    // 	   PC	-	-	Program Counter/Pointer
    // 3 bit indicator for registers (0-7)
    // 0 => B, 1 => C, 2 => D, 3 => E, 4 => H, 5 => L, 6 => (HL), 7 => A
    // note when HL is used, it means the byte the address HL is pointing to (indirect)
    // that's what the ( ) mean
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

    //opcode tables
    primary_bytecode_table: BytecodeTable,
    cb_bytecode_table: BytecodeTable,
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            a: 0x00,
            f: 0x00,
            b: 0x00,
            c: 0x00,
            d: 0x00,
            e: 0x00,
            h: 0x00,
            l: 0x00,
            sp: 0xfffe, //the starting point for stack ram (0xff80 to 0xfffe; 127 bytes)
            pc: 0x0000,

            mem: vec![0; 0x10000], //65535 valid memory bytes the full virtual space of 0xffff
            primary_bytecode_table: Cpu::build_bytecode_table(),
            cb_bytecode_table: Cpu::build_cb_bytecode_table(),
        }
    }

    pub fn load_rom(&mut self, filename: &str) {
        let rom = read(filename).unwrap();

        if rom.len() < 0x0100 {
            // first 256 bytes is header, any rom with less than this is invalid
            // really anything shorter than 512 bytes is probably invalid
            panic!("Rom is of invalid size ({} bytes)", rom.len());
        }

        let mut global_sum = 0u16;
        let mut header_sum = 0u8;

        // copy rom and calc checksums
        for i in 0..rom.len() {
            //https://gbdev.io/pandocs/The_Cartridge_Header.html#014d--header-checksum
            if i >= 0x0134 && i <= 0x014c {
                header_sum = header_sum.wrapping_sub(rom[i]).wrapping_sub(1);
            }
            if i != 0x14e && i != 0x14f {
                global_sum = global_sum.wrapping_add(rom[i] as u16);
            }
            self.mem[i] = rom[i];
        }

        let global_checksum = (self.mem[0x014e] as u16) << 8 | self.mem[0x014f] as u16;
        let header_checksum = self.mem[0x014d];

        if header_sum != header_checksum {
            println!(
                "Warning: header checksum failed (found: {:#02x} expected {:#02x})",
                header_sum, header_checksum
            );
        } else {
            println!("Header checksum passed ({:#x})", header_sum);
        }

        if global_sum != global_checksum {
            println!(
                "Warning: global checksum failed (found: {:#04x} expected {:#04x})",
                global_sum, global_checksum
            );
        } else {
            println!("Global checksum passed ({:#04x})", global_sum);
        }

        self.apply_post_boot_state(header_sum);
    }

    pub fn run(&mut self) {
        //**timing stuff**
        //4.194304 MHz
        //238.4185791015625 nanoseconds per cycle (ns)
        //about 4194.304 cycles in 1ms
        //20972 is about 5ms -> we'll use this as cycles per sleep
        //1 nop takes 4 cycles
        let cycle_duration = time::Duration::from_nanos(238);
        let cycles_per_sleep = 20_000u32;
        let mut cycle_count_since_last_sleep = 0u32;

        // there are about 245 unique opcodes
        // 30 implemented so far
        // 0 have tests
        loop {
            // 0x7fff is the highest rom address, we'll halt on this
            // unless there's a reason to allow it
            if self.pc > 0x7fff {
                println!("attempted to execute outside of rom space");
                exit(0);
            }

            let opcode = self.mem[self.pc];
            println!("{:#02x}: {:02x}", self.pc, opcode);

            //execute instruction
            let cycle_cost = self.primary_bytecode_table[opcode as usize](self, opcode);

            //todo:
            // should be using a timer, subtracting time used to actually process instruction
            // then only spin waiting for the time remaining
            // always use multiples of 4 cycles, this will make timing a bit easier
            // all instructions take multiples of 4 cycles
            spin_sleep::sleep(cycle_duration * cycle_cost);
            cycle_count_since_last_sleep += cycle_cost;

            // this is so that the emulator doesn't hog the cpu and get punished
            // by the scheduler
            if cycle_count_since_last_sleep >= cycles_per_sleep {
                cycle_count_since_last_sleep = 0;
                thread::yield_now();
            }

            // check for interrupts and adjust PC accordingly
            // EI (0xfb) is delayed by one instruction (calling DI right after
            // EI would mean no interrupts can trigger)

            //IME, IE, IF
            // IME = master interrupt enable flag (write only), can only be modified by
            //      EI instruction (enable interrupts), DI instruction (disable interrupts),
            //      RETI instruction (return and enable interrupts),
            //      ISR (not an instruction, the interrupt service routine, disables interrupts and
            //          calls the interrupt handler)
            //      IME is write only, meaning it is not a part of the addressable memory space
            //      it is a flag somewhere in the cpu's silicon.
            // ISR interrupt service handler: magic code on the cpu that runs before any
            //      interrupt handler is called (does it effect the call stack?)
            //          1. the IF bit corresponding to the handler is reset (to 0)
            //          2. disables interrupt handling (IME)
            //          3. corresponding handler address is called (eg $40), a regular call,
            //              just like the `call $40` so current PC is pushed onto the stack
            //          Note: this routine should last 20 cycles (or 5 `nop`s)
            // IE, located at $ffff, first 5 bits (0-4) correspond to an interrupt
            //      handler to be enabled or disabled (1/0)
            // IF, located at $ff0f, first 5 bits (0-4) are flags for each handler, when a bit
            //      is flipped to 1, the corresponding interrupt has been requested. Usually not
            //      necessary to write to this, unless the user wants to manually request
            //      an interrupt, or discard interrupts
            //
            //
            //interrupts (in same bit order as for IE & IF)
            // VBlank (bit 0)   - $40 -> handles
            // LCD STAT (bit 1) - $48
            // Timer (bit 2)    - $50
            // Serial (bit 3)   - $58
            // Joypad (bit 4)   - $60
            // Bit 0 (VBlank) has the highest priority,
            // and Bit 4 (Joypad) has the lowest priority
            // If multiple bits are 1 in the IF, then we call the interrupts in this order
            //
            // *Interrupts are always disabled when calling an interrupt, so by
            // default interrupts do not "nest". However user can call IE which will
            // enabled interrupts while the handler is running, allowing for a nested
            // interrupt to occur
        }
    }

    fn get_af(&self) -> u16 {
        (self.a as u16) << 8 | self.f as u16
    }

    fn set_af(&mut self, value: u16) {
        self.a = (value >> 8) as u8;
        self.f = value as u8;
    }

    fn get_bc(&self) -> u16 {
        (self.b as u16) << 8 | self.c as u16
    }

    fn set_bc(&mut self, value: u16) {
        self.b = (value >> 8) as u8;
        self.c = value as u8;
    }

    fn get_de(&self) -> u16 {
        (self.d as u16) << 8 | self.e as u16
    }

    fn set_de(&mut self, value: u16) {
        self.d = (value >> 8) as u8;
        self.e = value as u8;
    }

    fn get_hl(&self) -> u16 {
        (self.h as u16) << 8 | self.l as u16
    }

    fn set_hl(&mut self, value: u16) {
        self.h = (value >> 8) as u8;
        self.l = value as u8;
    }

    fn dump_registers(&self) -> String {
        format!(
            "A: 0x{:02x}\tF: 0x{:02x}
B: 0x{:02x}\tC: 0x{:02x}
D: 0x{:02x}\tE: 0x{:02x}
H: 0x{:02x}\tL: 0x{:02x}
SP: 0x{:04x}
PC: 0x{:04x}",
            self.a, self.f, self.b, self.c, self.d, self.e, self.h, self.l, self.sp, self.pc
        )
    }

    // this is a shortcut around actually running the boot rom
    // with the exception of video memory state, the rest of the system should be
    // the same as if the boot rom actually ran
    fn apply_post_boot_state(&mut self, header_sum: u8) {
        // update cpu registers and hardware registers
        // using register state indicated by
        // https://gbdev.io/pandocs/Power_Up_Sequence.html#cpu-registers
        // this mimics the state of the machine after the boot rom finishes

        // cpu registers
        self.a = 0x01;
        if header_sum != 0 {
            self.f = 0u8 | 1 << 7 | 1 << 5 | 1 << 4;
        } else {
            self.f = 0u8 | 1 << 7;
        }
        self.b = 0x00;
        self.c = 0x13;
        self.d = 0x00;
        self.e = 0xd8;
        self.h = 0x01;
        self.l = 0x4d;
        self.pc = 0x100;
        self.sp = 0xfffe;

        // hardware registers
        self.mem[0xff00] = 0xcf; //p1
        self.mem[0xff01] = 0x00; //sb
        self.mem[0xff02] = 0x7e; //sc
        self.mem[0xff04] = 0xab; //div
        self.mem[0xff05] = 0x00; //tima
        self.mem[0xff06] = 0x00; //tma
        self.mem[0xff07] = 0xf8; //tac
        self.mem[0xff0f] = 0xe1; //if

        self.mem[0xff10] = 0x80; //nr10
        self.mem[0xff11] = 0xbf; //nr11
        self.mem[0xff12] = 0xf3; //nr12
        self.mem[0xff13] = 0xff; //nr13
        self.mem[0xff14] = 0xbf; //nr14
        self.mem[0xff16] = 0x3f; //nr21
        self.mem[0xff17] = 0x00; //nr22
        self.mem[0xff18] = 0xff; //nr23
        self.mem[0xff19] = 0xbf; //nr24
        self.mem[0xff1a] = 0x7f; //nr30
        self.mem[0xff1b] = 0xff; //nr31
        self.mem[0xff1c] = 0x9f; //nr32
        self.mem[0xff1d] = 0xff; //nr33
        self.mem[0xff1e] = 0xbf; //nr34
        self.mem[0xff20] = 0xff; //nr41
        self.mem[0xff21] = 0x00; //nr42
        self.mem[0xff22] = 0x00; //nr43
        self.mem[0xff23] = 0xbf; //nr44
        self.mem[0xff24] = 0x77; //nr50
        self.mem[0xff25] = 0xf3; //nr51
        self.mem[0xff26] = 0xf1; //nr52

        self.mem[0xff40] = 0x91; //lcdc
        self.mem[0xff41] = 0x85; //stat
        self.mem[0xff42] = 0x00; //scy
        self.mem[0xff43] = 0x00; //scx
        self.mem[0xff44] = 0x00; //ly
        self.mem[0xff45] = 0x00; //lyc
        self.mem[0xff46] = 0xff; //dma
        self.mem[0xff47] = 0xfc; //bgp
        self.mem[0xff48] = 0x00; //obp0 (technically uninitialized)
        self.mem[0xff49] = 0x00; //obp1 (technically uninitialized)
        self.mem[0xff4a] = 0x00; //wy
        self.mem[0xff4b] = 0x00; //wx

        self.mem[0xff4d] = 0xff; //key1
        self.mem[0xff4f] = 0xff; //vbk
        self.mem[0xff51] = 0xff; //hdma1
        self.mem[0xff52] = 0xff; //hdma2
        self.mem[0xff53] = 0xff; //hdma3
        self.mem[0xff54] = 0xff; //hdma4
        self.mem[0xff55] = 0xff; //hdma5

        self.mem[0xff56] = 0xff; //rp
        self.mem[0xff68] = 0xff; //bcps
        self.mem[0xff69] = 0xff; //bcpd
        self.mem[0xff6a] = 0xff; //ocps
        self.mem[0xff6b] = 0xff; //ocpd
        self.mem[0xff70] = 0xff; //svbk
        self.mem[0xffff] = 0x00; //ie
    }

    fn build_bytecode_table() -> BytecodeTable {
        // initialize table with all opcodes as not_implemented
        let mut table: BytecodeTable = [Cpu::not_implemented; 255];

        // invalid opcodes (there are 11 invalid opcodes)
        table[0xd3] = Cpu::invalid_opcode;
        table[0xdb] = Cpu::invalid_opcode;
        table[0xdd] = Cpu::invalid_opcode;

        table[0xe3] = Cpu::invalid_opcode;
        table[0xe4] = Cpu::invalid_opcode;
        table[0xeb] = Cpu::invalid_opcode;
        table[0xec] = Cpu::invalid_opcode;
        table[0xed] = Cpu::invalid_opcode;

        table[0xf4] = Cpu::invalid_opcode;
        table[0xfc] = Cpu::invalid_opcode;
        table[0xfd] = Cpu::invalid_opcode;

        // actual opcodes
        table[0x00] = Cpu::nop;
        table[0xc3] = Cpu::jump;

        table[0xa8] = Cpu::xor_8bit_a_b;
        table[0xa9] = Cpu::xor_8bit_a_c;
        table[0xaa] = Cpu::xor_8bit_a_d;
        table[0xab] = Cpu::xor_8bit_a_e;
        table[0xac] = Cpu::xor_8bit_a_h;
        table[0xad] = Cpu::xor_8bit_a_l;
        table[0xae] = Cpu::xor_8bit_a_hl_indirect;
        table[0xaf] = Cpu::xor_8bit_a_a;

        table[0x01] = Cpu::load_16bit_bc_immediate_value;
        table[0x11] = Cpu::load_16bit_de_immediate_value;
        table[0x21] = Cpu::load_16bit_hl_immediate_value;
        table[0x31] = Cpu::load_16bit_sp_immediate_value;

        table[0x06] = Cpu::load_8bit_b_immediate_value;
        table[0x0e] = Cpu::load_8bit_c_immediate_value;
        table[0x16] = Cpu::load_8bit_d_immediate_value;
        table[0x1e] = Cpu::load_8bit_e_immediate_value;
        table[0x26] = Cpu::load_8bit_h_immediate_value;
        table[0x2e] = Cpu::load_8bit_l_immediate_value;
        table[0x36] = Cpu::load_8bit_hl_indirect_from_immediate_value;
        table[0x3e] = Cpu::load_8bit_a_immediate_value;

        table[0x02] = Cpu::load_8bit_bc_indirect_from_a;
        table[0x12] = Cpu::load_8bit_de_indirect_from_a;
        table[0x22] = Cpu::load_8bit_hl_inc_indirect_from_a;
        table[0x32] = Cpu::load_8bit_hl_dec_indirect_from_a;

        table[0x0a] = Cpu::load_8bit_a_from_bc_indirect;
        table[0x1a] = Cpu::load_8bit_a_from_de_indirect;
        table[0x2a] = Cpu::load_8bit_a_from_hl_inc_indirect;
        table[0x3a] = Cpu::load_8bit_a_from_hl_dec_indirect;

        table
    }

    fn build_cb_bytecode_table() -> BytecodeTable {
        // initialize table with all opcodes as not_implemented
        let mut table: BytecodeTable = [Cpu::not_implemented; 255];

        table[0x00] = Cpu::not_implemented;

        table
    }

    fn not_implemented(&mut self, opcode: u8) -> CycleCount {
        panic!(
            "cpu function not implemented\nopcode 0x{:02x}\n{}",
            opcode,
            self.dump_registers()
        );
    }

    fn invalid_opcode(&mut self, opcode: u8) -> CycleCount {
        panic!("opcode 0x{:02x} is an invalid instruction", opcode);
    }

    //0x00
    fn nop(&mut self, _: u8) -> CycleCount {
        self.pc += 1;
        4
    }

    //0xc3
    fn jump(&mut self, _: u8) -> CycleCount {
        let target = ((self.mem[self.pc + 2] as u16) << 8) | self.mem[self.pc + 1] as u16;
        self.pc = target as usize;
        16
    }

    //0xa8
    fn xor_8bit_a_b(&mut self, _: u8) -> CycleCount {
        self.a = self.a ^ self.b;
        self.pc += 1;
        4
    }

    //0xa9
    fn xor_8bit_a_c(&mut self, _: u8) -> CycleCount {
        self.a = self.a ^ self.c;
        self.pc += 1;
        4
    }

    //0xaa
    fn xor_8bit_a_d(&mut self, _: u8) -> CycleCount {
        self.a = self.a ^ self.d;
        self.pc += 1;
        4
    }

    //0xab
    fn xor_8bit_a_e(&mut self, _: u8) -> CycleCount {
        self.a = self.a ^ self.e;
        self.pc += 1;
        4
    }

    //0xac
    fn xor_8bit_a_h(&mut self, _: u8) -> CycleCount {
        self.a = self.a ^ self.h;
        self.pc += 1;
        4
    }

    //0xad
    fn xor_8bit_a_l(&mut self, _: u8) -> CycleCount {
        self.a = self.a ^ self.l;
        self.pc += 1;
        4
    }

    //0xae
    fn xor_8bit_a_hl_indirect(&mut self, _: u8) -> CycleCount {
        self.a = self.a ^ self.mem[self.get_hl() as usize];
        self.pc += 1;
        4
    }

    //0xaf
    fn xor_8bit_a_a(&mut self, _: u8) -> CycleCount {
        self.a = self.a ^ self.a;
        self.pc += 1;
        4
    }

    // note: this 16bit loads are little endian
    //0x01
    fn load_16bit_bc_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = (self.mem[self.pc + 2] as u16) << 8 | self.mem[self.pc + 1] as u16;
        self.set_bc(value);
        self.pc += 3;
        12
    }

    //0x11
    fn load_16bit_de_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = (self.mem[self.pc + 2] as u16) << 8 | self.mem[self.pc + 1] as u16;
        self.set_de(value);
        self.pc += 3;
        12
    }

    //0x21
    fn load_16bit_hl_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = (self.mem[self.pc + 2] as u16) << 8 | self.mem[self.pc + 1] as u16;
        self.set_hl(value);
        self.pc += 3;
        12
    }

    //0x31
    fn load_16bit_sp_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = (self.mem[self.pc + 2] as u16) << 8 | self.mem[self.pc + 1] as u16;
        self.sp = value as usize;
        self.pc += 3;
        12
    }

    //0x06
    fn load_8bit_b_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = self.mem[self.pc + 1];
        self.b = value;
        self.pc += 2;
        8
    }

    //0x0e
    fn load_8bit_c_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = self.mem[self.pc + 1];
        self.c = value;
        self.pc += 2;
        8
    }

    // 0x16
    fn load_8bit_d_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = self.mem[self.pc + 1];
        self.d = value;
        self.pc += 2;
        8
    }

    // 0x1e
    fn load_8bit_e_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = self.mem[self.pc + 1];
        self.e = value;
        self.pc += 2;
        8
    }

    //0x26
    fn load_8bit_h_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = self.mem[self.pc + 1];
        self.h = value;
        self.pc += 2;
        8
    }

    //0x2e
    fn load_8bit_l_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = self.mem[self.pc + 1];
        self.l = value;
        self.pc += 2;
        8
    }

    //0x36
    fn load_8bit_hl_indirect_from_immediate_value(&mut self, _: u8) -> CycleCount {
        // load value to hl address
        let value = self.mem[self.pc + 1];
        let hl = self.get_hl() as usize;
        self.mem[hl] = value;
        self.pc += 2;
        12
    }

    //0x3e
    fn load_8bit_a_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = self.mem[self.pc + 1];
        self.a = value;
        self.pc += 2;
        8
    }

    // load from accumulator (indirect BC, DE, HL+, HL-)
    // 02 12 22 32 A register stored to (BC), (DE), (HL+), or (HL-)
    // hl+ and hl- means increment or decrement hl AFTER memory read
    //0x02
    fn load_8bit_bc_indirect_from_a(&mut self, _: u8) -> CycleCount {
        let address = self.get_bc() as usize;
        self.mem[address] = self.a;
        self.pc += 1;
        8
    }

    //0x12
    fn load_8bit_de_indirect_from_a(&mut self, _: u8) -> CycleCount {
        let address = self.get_de() as usize;
        self.mem[address] = self.a;
        self.pc += 1;
        8
    }

    //0x22
    fn load_8bit_hl_inc_indirect_from_a(&mut self, _: u8) -> CycleCount {
        //Load to the absolute address specified by the 16-bit register HL,
        //data from the 8-bit A register. The value of HL is incremented
        //after the memory write.
        let address = self.get_hl() as usize;
        self.mem[address] = self.a;
        self.set_hl(address as u16 + 1);
        self.pc += 1;
        8
    }

    //0x32
    fn load_8bit_hl_dec_indirect_from_a(&mut self, _: u8) -> CycleCount {
        //Load to the absolute address specified by the 16-bit register HL,
        //data from the 8-bit A register. The value of HL is decremented
        //after the memory write.
        let address = self.get_hl() as usize;
        self.mem[address] = self.a;
        self.set_hl(address as u16 - 1);
        self.pc += 1;
        8
    }

    //load to accumulator (indirect BC, DE, HL+, HL-)
    // 0a 1a 2a 3a (BC), (DE), (HL+), or (HL-) stored to A register
    //0x0a
    fn load_8bit_a_from_bc_indirect(&mut self, _: u8) -> CycleCount {
        let address = self.get_bc() as usize;
        self.a = self.mem[address];
        self.pc += 1;
        8
    }

    //0x1a
    fn load_8bit_a_from_de_indirect(&mut self, _: u8) -> CycleCount {
        let address = self.get_de() as usize;
        self.a = self.mem[address];
        self.pc += 1;
        8
    }

    //0x2a
    fn load_8bit_a_from_hl_inc_indirect(&mut self, _: u8) -> CycleCount {
        //Load to the 8-bit A register, data from the absolute address
        //specified by the 16-bit register HL. The value of HL is
        //incremented after the memory read.

        let address = self.get_hl() as usize;
        self.a = self.mem[address];
        self.set_hl(address as u16 + 1);
        self.pc += 1;
        8
    }

    //0x3a
    fn load_8bit_a_from_hl_dec_indirect(&mut self, _: u8) -> CycleCount {
        let address = self.get_hl() as usize;
        self.a = self.mem[address];
        self.set_hl(address as u16 - 1);
        self.pc += 1;
        8
    }
}
