use std::fs::{read, File};
use std::io::{BufWriter, Write};

use crate::addresses::*;
use crate::constants::*;

pub type CycleCount = u32;
type BytecodeTable = [fn(&mut Cpu, u8) -> CycleCount; 256];

const Z_FLAG_MASK: u8 = 0b1000_0000;
const N_FLAG_MASK: u8 = 0b0100_0000;
const H_FLAG_MASK: u8 = 0b0010_0000;
const C_FLAG_MASK: u8 = 0b0001_0000;

#[derive(Debug)]
pub enum InterruptFlags {
    VBlank = 0b0000_0001,
    LcdStat = 0b0000_0010,
    Timer = 0b0000_0100,
    Serial = 0b0000_1000,
    Joypad = 0b0001_0000,
}

#[derive(Debug)]
enum InterruptAddresses {
    VBlank = 0x0040,
    LcdStat = 0x0048,
    Timer = 0x0050,
    Serial = 0x0058,
    Joypad = 0x0060,
}

enum JoypadBits {
    DownOrStart = 0b0000_1000,
    UpOrSelect = 0b0000_0100,
    LeftOrB = 0b0000_0010,
    RightOrA = 0b0000_0001,
}

pub enum CpuStepResult {
    CyclesExecuted(u32),
    Stopped,
}

pub struct InputState {
    pub a_button_was_pressed: bool,
    pub up: bool,
    pub down: bool,
    pub left: bool,
    pub right: bool,
    pub start: bool,
    pub select: bool,
    pub b: bool,
    pub a: bool,
}

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

    last_opcode: u8,

    // memory
    mem: Vec<u8>,
    rom: Vec<u8>,

    pub input_state: InputState,

    //opcode tables
    primary_bytecode_table: BytecodeTable,
    cb_bytecode_table: BytecodeTable,

    //IME
    interrupt_master_enable: bool,

    // quirk emulation
    //ei_delay is to emulate the delay of enabling interrupts (it happens one instruction late)
    //so if I call ei, then increment A, interrupt won't be enabled till after the increment
    //this can effect the HALT bug
    // ei_delay is decremented when its greater than zero
    // if zero after decrement, set ime true
    // ignored if it is currently zero
    // enable_interrupt will set this to 2, (enabling ime is checked after each instruction)
    // disable_interrupt will set this to 0, to stop ime from being enabled
    ei_delay: u8,

    //cycle counters for timers
    div_cycle_counter: u32,
    timer_cycle_counter: u32,

    dr_log_buf_writer: Option<BufWriter<File>>,
    instruction_count: u32,
    last_pc: usize,

    // special case states
    ///halt = pause cpu & timer until any interrupt occurs (ppu keeps running)
    is_halted: bool,
    ///stop = everything stops (ppu, cpu, timers) until system is reset (memory state is preserved)
    is_stopped: bool,
    /// OAM DMA is a fast way to copy data to OAM table, (faster than memcpy). While running, this
    /// blocks all ram except for 0xff80-0xfffe (including ROM), not even instructions can be read
    /// outside this space [needs to be public for ppu]
    pub is_oam_dma_active: bool,
    /// used to track the 160 cycles oam dma runs for
    dma_active_cycle_counter: u32,

    pub rom_loaded: bool,
}

impl Cpu {
    pub fn new() -> Cpu {
        let mut buf_writer = None;
        if DR_GB_LOGGING_ENABLED {
            let dr_log_path = "dr_log.txt";
            let log_output = File::create(dr_log_path).unwrap();
            buf_writer = Some(BufWriter::new(log_output));
        }

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

            last_opcode: 0x00,

            mem: vec![0; 0x10000], //65535 valid memory bytes the full virtual space of 0xffff
            rom: vec![0; 0],

            input_state: InputState {
                a_button_was_pressed: false,
                up: false,
                down: false,
                left: false,
                right: false,
                start: false,
                select: false,
                b: false,
                a: false,
            },

            primary_bytecode_table: Cpu::build_bytecode_table(),
            cb_bytecode_table: Cpu::build_cb_bytecode_table(),

            ei_delay: 0,

            interrupt_master_enable: true,

            div_cycle_counter: 0,
            timer_cycle_counter: 0,

            dr_log_buf_writer: buf_writer,
            instruction_count: 0,
            last_pc: 0x0000,

            is_halted: false,
            is_stopped: false,
            is_oam_dma_active: false,
            dma_active_cycle_counter: 0,

            rom_loaded: false,
        }
    }

    pub fn load_rom(&mut self, filename: &str) {
        //reset CPU
        *self = Cpu::new();

        self.rom = read(filename).unwrap_or_else(|e| {
            panic!("Bad rom file {:}\n{:}", filename, e);
        });

        if self.rom.len() < 0x0100 {
            // first 256 bytes is header, any rom with less than this is invalid
            // really anything shorter than 512 bytes is probably invalid
            panic!("Rom is of invalid size ({} bytes)", self.rom.len());
        }

        let cart_type = self.rom[CARTRIDGE_TYPE_ROM_ADDRESS];

        log::info!("Cartridge Type: 0x{:x}", cart_type);
        if !SUPPORTED_MBC_TYPES.contains(&cart_type) {
            log::warn!("Cartridge Type 0x{:x} not supported", cart_type);
        }

        let mut global_sum = 0u16;
        let mut header_sum = 0u8;

        // copy rom and calc checksums
        for i in 0..self.rom.len() {
            //https://gbdev.io/pandocs/The_Cartridge_Header.html#014d--header-checksum
            if i >= 0x0134 && i <= 0x014c {
                header_sum = header_sum.wrapping_sub(self.rom[i]).wrapping_sub(1);
            }
            if i != 0x14e && i != 0x14f {
                global_sum = global_sum.wrapping_add(self.rom[i] as u16);
            }
            self.rom[i] = self.rom[i];
        }

        let global_checksum = (self.rom[0x014e] as u16) << 8 | self.rom[0x014f] as u16;
        let header_checksum = self.rom[0x014d];

        if header_sum != header_checksum {
            log::warn!(
                "header checksum failed (found: {:#02x} expected {:#02x})",
                header_sum,
                header_checksum
            );
        } else {
            log::info!("Header checksum passed ({:#x})", header_sum);
        }

        if global_sum != global_checksum {
            log::warn!(
                "global checksum failed (found: {:#04x} expected {:#04x})",
                global_sum,
                global_checksum
            );
        } else {
            log::info!("Global checksum passed ({:#04x})", global_sum);
        }

        //copy first memory block into position
        let end = std::cmp::min(self.rom.len(), 0x7fff);
        for i in 0..end {
            self.mem[i] = self.rom[i];
        }

        self.apply_post_boot_state(header_sum);
        self.dr_log_line_initial();
        self.rom_loaded = true;
    }

    pub fn do_step(&mut self) -> CpuStepResult {
        //abort the loop if we keep jumping to the same instruction
        if DR_GB_LOGGING_ENABLED {
            if self.last_pc == self.pc && self.mem[self.pc] == 0x18 && self.mem[self.pc + 1] == 0xfe
            {
                return CpuStepResult::Stopped;
            }
        }

        let opcode = self.mem[self.pc];

        let mut cycle_cost = 0;
        // check interrupts BEFORE executing next instruction (so that PPU triggered
        // interrupts happen as soon as possible)
        if let Some((address, flag_mask)) = self.check_interrupts() {
            // any interrupt un-halts the cpu, even if IME is not on
            self.is_halted = false;
            if self.interrupt_master_enable {
                self.interrupt_service_handler(address, flag_mask);
                // when true, the ISR (interrupt service handler) consumes 20 cycles
                cycle_cost += 20;
                self.update_timer(20);

                // when an interrupt is started, we send the cycle_cost back for PPU
                // to process these 20 dots, that way when the first instruction of
                // the interrupt runs, any state affected by ppu will be up to date
                // IME will be disabled, so this the next instruction will always
                // be run (ie another interrupt will not be started)
                return CpuStepResult::CyclesExecuted(cycle_cost);
            }
        }

        //execute instruction
        if self.is_halted {
            self.update_timer(CPU_CYCLES_PER_HALTED_STEP);
            return CpuStepResult::CyclesExecuted(CPU_CYCLES_PER_HALTED_STEP);
        } else {
            cycle_cost += self.primary_bytecode_table[opcode as usize](self, opcode);
            self.update_timer(cycle_cost);

            if DR_GB_LOGGING_ENABLED {
                //hack for dr gb logging
                self.mem[LY_ADDRESS] = 0x90;
            }

            //temp hack: clear lower 4 bits of F (these are invalid bits)
            self.f = self.f & 0xf0;

            //IME enable quirk, always enables 1 instruction AFTER it is toggled on
            if self.ei_delay > 0 {
                self.ei_delay -= 1;
                if self.ei_delay == 0 {
                    self.interrupt_master_enable = true;
                }
            }
            self.dr_log_line();
            self.instruction_count += 1;
            self.last_pc = self.pc;
            self.last_opcode = opcode;

            if self.is_oam_dma_active {
                self.dma_active_cycle_counter += cycle_cost;
                if self.dma_active_cycle_counter > OAM_DMA_CYCLES {
                    // we're using > here because we want to toggle off once we've exceeded the cycles
                    // if we stop once we've reached _exactly_ the cycle count, the PPU will process
                    // dots incorrectly (as if OAM DMA wasn't running during those dots).
                    // instead we want the following cycles to count
                    // this does mean that sometimes the PPU might process a few dots incorrectly as if
                    // OAM DMA is running, but it is more likely that we will reach 160 exactly due to
                    // the cost of most instructions, additionally the spin wait loops used while waiting
                    // for OAM DMA to finish are usually tuned to be exactly 160 cycles before returning
                    self.is_oam_dma_active = false;
                    log::trace!("OAM DMA END");
                }
            }

            if self.is_stopped {
                return CpuStepResult::Stopped;
            } else {
                return CpuStepResult::CyclesExecuted(cycle_cost);
            }
        }
    }

    fn dr_log_line_initial(&mut self) {
        if DR_GB_LOGGING_ENABLED {
            self.dr_log_buf_writer.as_mut().unwrap().
            write_fmt(format_args!(
                "A:{:02X} F:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} H:{:02X} L:{:02X} SP:{:04X} PC:{:04X} PCMEM:{:02X},{:02X},{:02X},{:02X}\n", 
                self.a, self.f, self.b, self.c, self.d, self.e, self.h, self.l, self.sp, self.pc,
                self.mem[self.pc], self.mem[self.pc + 1], self.mem[self.pc + 2], self.mem[self.pc + 3]
            ))
            .unwrap();
        }
    }

    fn dr_log_line(&mut self) {
        if DR_GB_LOGGING_ENABLED {
            self.dr_log_buf_writer
                .as_mut()
                .unwrap()
                .write_fmt(format_args!(
                    "A:{:02X} F:{:02X} B:{:02X} C:{:02X} D:{:02X} E:{:02X} H:{:02X} L:{:02X} ",
                    self.a, self.f, self.b, self.c, self.d, self.e, self.h, self.l
                ))
                .unwrap();

            self.dr_log_buf_writer
                .as_mut()
                .unwrap()
                .write_fmt(format_args!("SP:{:04X} PC:{:04X} ", self.sp, self.pc))
                .unwrap();
            if self.pc > 0xffff {
                panic!(
                    "Program counter exceeding address space!!!\n{}",
                    self.dump_registers()
                );
            }
            self.dr_log_buf_writer
                .as_mut()
                .unwrap()
                .write_fmt(format_args!(
                    "PCMEM:{:02X},{:02X},{:02X},{:02X}\n",
                    self.mem[self.pc],
                    self.mem[self.pc + 1],
                    self.mem[self.pc + 2],
                    self.mem[self.pc + 3]
                ))
                .unwrap();
        }
    }

    fn update_timer(&mut self, cycle_delta: u32) {
        ///////0xff04 - DIV - divider register
        //incremented at 16_384Hz (ie ticks per second) 256 ratio of main clock
        //CGB mode can run at 32_768 (won't be implementing)
        //Does not increment in STOP mode
        //**writing any value to this register resets it to zero** */
        //this is handled in the write_mem function

        ///////0xff05 - TIMA - timer counter
        //incremented at rate specified by TAC
        //when it overflows (8 bit val) it resets to value specified by TMA
        //and the timer interrupt is requested

        ////////0xff06 - TMA - timer modulo
        //reset point used by TIMA when TIMA overflows
        //when TIMA overflows interrupt is requested

        self.div_cycle_counter += cycle_delta;
        let div_increment = self.div_cycle_counter / 256;
        self.mem[DIV_ADDRESS] = self.mem[DIV_ADDRESS].wrapping_add(div_increment as u8);
        self.div_cycle_counter -= div_increment * 256;

        ////////0xff07 - TAC - timer control
        // Bit  2   - Timer Enable
        // Bits 1-0 - Input Clock Select
        //    00: CPU Clock / 1024    4096 Hz
        //    01: CPU Clock / 16    262144 Hz
        //    10: CPU Clock / 64     65536 Hz
        //    11: CPU Clock / 256    16384 Hz
        let tac = self.mem[TAC_ADDRESS];

        let timer_enable = (tac & 0b0000_0100) != 0;
        if timer_enable {
            self.timer_cycle_counter += cycle_delta;
            let timer_clock_select = tac & 0b0000_0011;
            let timer_divisor = match timer_clock_select {
                0b00 => 1024,
                0b01 => 16,
                0b10 => 64,
                0b11 => 256,
                _ => panic!("invalid clock select"),
            };
            let timer_increment = self.timer_cycle_counter / timer_divisor;
            let tima = self.mem[TIMA_ADDRESS];
            let (result, overflow) = tima.overflowing_add(timer_increment as u8);
            self.timer_cycle_counter -= timer_increment * timer_divisor;

            self.mem[TIMA_ADDRESS] = if overflow {
                let modulo = self.mem[TMA_ADDRESS];
                //request timer interrupt
                self.mem[INTERRUPT_FLAG_ADDRESS] |= InterruptFlags::Timer as u8;

                modulo + result
            } else {
                result
            }
        }
    }

    pub fn update_joypad(&mut self) {
        // Bit 7 - Not used
        // Bit 6 - Not used
        // Bit 5 - P15 Select Action buttons    (0=Select)
        // Bit 4 - P14 Select Direction buttons (0=Select)
        // Bit 3 - P13 Input: Down  or Start    (0=Pressed) (Read Only)
        // Bit 2 - P12 Input: Up    or Select   (0=Pressed) (Read Only)
        // Bit 1 - P11 Input: Left  or B        (0=Pressed) (Read Only)
        // Bit 0 - P10 Input: Right or A        (0=Pressed) (Read Only)

        //get joypad state, reset out current 'pressed' states, we'll refresh these
        //1 = __not__ pressed, 0 = pressed
        let mut joypad_state = self.read_hw_reg(JOYPAD_ADDRESS) & 0b0011_0000;
        joypad_state |= 0b0000_1111;
        let joypad_matrix_select = joypad_state >> 4;
        match joypad_matrix_select {
            0b01 => {
                //action buttons
                if self.input_state.a {
                    joypad_state ^= JoypadBits::RightOrA as u8;
                }
                if self.input_state.b {
                    joypad_state ^= JoypadBits::LeftOrB as u8;
                }
                if self.input_state.select {
                    joypad_state ^= JoypadBits::UpOrSelect as u8;
                }
                if self.input_state.start {
                    joypad_state ^= JoypadBits::DownOrStart as u8;
                }
            }
            0b10 => {
                //direction buttons
                if self.input_state.right {
                    joypad_state ^= JoypadBits::RightOrA as u8;
                }
                if self.input_state.left {
                    joypad_state ^= JoypadBits::LeftOrB as u8;
                }
                if self.input_state.up {
                    joypad_state ^= JoypadBits::UpOrSelect as u8;
                }
                if self.input_state.down {
                    joypad_state ^= JoypadBits::DownOrStart as u8;
                }
            }
            0b11 => {
                // I guess if you're just looking for any input?
                if self.input_state.right | self.input_state.a {
                    joypad_state ^= JoypadBits::RightOrA as u8;
                }
                if self.input_state.left | self.input_state.b {
                    joypad_state ^= JoypadBits::LeftOrB as u8;
                }
                if self.input_state.up | self.input_state.select {
                    joypad_state ^= JoypadBits::UpOrSelect as u8;
                }
                if self.input_state.down | self.input_state.start {
                    joypad_state ^= JoypadBits::DownOrStart as u8;
                }
            }
            _ => {}
        }
        self.write_hw_reg(JOYPAD_ADDRESS, joypad_state);
    }
    fn check_interrupts(&mut self) -> Option<(InterruptAddresses, InterruptFlags)> {
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
        let interrupt_enable = self.mem[INTERRUPT_ENABLE_ADDRESS];
        let interrupt_flag = self.mem[INTERRUPT_FLAG_ADDRESS];

        let interrupt_pending = interrupt_enable & interrupt_flag;

        // early return after each interrupt, can only run one interrupt at
        // at time
        // this enforces a priority order as well
        if InterruptFlags::VBlank as u8 & interrupt_pending != 0 {
            return Some((InterruptAddresses::VBlank, InterruptFlags::VBlank));
        }

        if InterruptFlags::LcdStat as u8 & interrupt_pending != 0 {
            return Some((InterruptAddresses::LcdStat, InterruptFlags::LcdStat));
        }

        if InterruptFlags::Timer as u8 & interrupt_pending != 0 {
            return Some((InterruptAddresses::Timer, InterruptFlags::Timer));
        }

        if InterruptFlags::Serial as u8 & interrupt_pending != 0 {
            return Some((InterruptAddresses::Serial, InterruptFlags::Serial));
        }

        if InterruptFlags::Joypad as u8 & interrupt_pending != 0 {
            return Some((InterruptAddresses::Joypad, InterruptFlags::Joypad));
        }
        None
    }

    fn interrupt_service_handler(
        &mut self,
        address: InterruptAddresses,
        flag_mask: InterruptFlags,
    ) {
        log::trace!("Firing interrupt {:?}", address);
        //0 out the interrupt flag bit
        self.mem[INTERRUPT_FLAG_ADDRESS] ^= flag_mask as u8;
        self.interrupt_master_enable = false;
        self.helper_call(address as u16, self.pc);
        //needs to wait 20 cycles
    }

    // for serial, video, sound, joypad, ect
    // that will be handled outside of the cpu
    pub fn read_hw_reg(&self, address: usize) -> u8 {
        //todo add constraints maybe idk
        self.mem[address]
    }

    // for serial, video, sound, joypad, ect
    // that will be handled outside of the cpu
    pub fn write_hw_reg(&mut self, address: usize, value: u8) {
        self.mem[address] = value;
    }

    //used by cpu instructions, other emulator code just accesses directly
    fn read_mem(&self, address: usize) -> u8 {
        let ppu_mode = self.mem[STAT_ADDRESS] & 0b11;
        if ppu_mode == 3 {
            // if mode 3 block all VRAM (return 0xff garbage value)
            match address {
                VRAM_ADDRESS..=VRAM_END_ADDRESS => return 0xff,
                _ => {}
            }
        }
        if ppu_mode == 2 || ppu_mode == 3 {
            // if mode 2 or 3 block OAM table (return 0xff garbage value)
            match address {
                OAM_TABLE_ADDRESS..=OAM_TABLE_END_ADDRESS => return 0xff,
                _ => {}
            }
        }

        if self.is_oam_dma_active {
            return match address {
                HIGH_RAM_START_ADDRESS..=HIGH_RAM_END_ADDRESS => self.mem[address],
                OAM_DMA_ADDRESS => self.mem[address],
                _ => {
                    log::trace!(
                        "({:x}) warning: program attempted to read invalid memory during OAM DMA",
                        self.pc
                    );
                    0x00
                }
            };
        }

        // normal ram access
        match address {
            FORBIDDEN_ADDRESS_START..=FORBIDDEN_ADDRESS_END => {
                //technically reading from the FORBIDDEN ADDRESS range is supposed to corrupt
                //the OAM memory, but i'd rather not emulate that
                0xff
            }
            JOYPAD_ADDRESS => {
                log::trace!("({:#06x}) JOYPAD read {:#010b}", self.pc, self.mem[address]);
                self.mem[address]
            }
            NR11_CHANNEL1_LENGTH_DUTY_ADDRESS | NR21_CHANNEL2_LENGTH_DUTY_ADDRESS => {
                // filtering out write only bits
                self.mem[address] & 0b1100_0000
            }
            NR13_CHANNEL1_WAVELENGTH_LOW_ADDRESS
            | NR23_CHANNEL2_WAVELENGTH_LOW_ADDRESS
            | NR31_CHANNEL3_LENGTH_TIMER_ADDRESS
            | NR33_CHANNEL3_WAVELENGTH_LOW_ADDRESS => 0x00,
            //
            NR14_CHANNEL1_WAVELENGTH_HIGH_AND_CONTROL_ADDRESS
            | NR24_CHANNEL2_WAVELENGTH_HIGH_AND_CONTROL_ADDRESS
            | NR34_CHANNEL3_WAVELENGTH_HIGH_AND_CONTROL_ADDRESS => {
                // filtering out write only bits
                self.mem[address] & 0b0111_1000
            }
            NR44_CHANNEL_4_CONTROL_ADDRESS => {
                // filtering out write only bits
                self.mem[address] & 0b0111_1111
            }
            _ => self.mem[address],
        }
    }

    //used by cpu instructions, other emulator code just accesses directly
    fn write_mem(&mut self, address: usize, value: u8) {
        let ppu_mode = self.mem[STAT_ADDRESS] & 0b11;
        if ppu_mode == 3 {
            // if mode 3 block all VRAM
            match address {
                VRAM_ADDRESS..=VRAM_END_ADDRESS => return,
                _ => {}
            }
        }
        if ppu_mode == 2 || ppu_mode == 3 {
            // if mode 2 or 3 block OAM table
            match address {
                OAM_TABLE_ADDRESS..=OAM_TABLE_END_ADDRESS => return,
                _ => {}
            }
        }

        if self.is_oam_dma_active {
            match address {
                HIGH_RAM_START_ADDRESS..=HIGH_RAM_END_ADDRESS => {
                    self.mem[address] = value;
                }
                OAM_DMA_ADDRESS => {
                    self.mem[address] = value;
                }
                _ => {
                    log::trace!("({:#06x}) warning: program attempted to write to invalid memory during OAM DMA", self.pc);
                }
            }
            return;
        }

        match address {
            //enforcing the read-only part of read-only memory
            ROM_BANK_00_START_ADDRESS..=ROM_BANK_00_END_ADDRESS
            | ROM_BANK_NN_START_ADDRESS..=ROM_BANK_NN_END_ADDRESS => {}
            //writing to div ALWAYS causes it to reset to zero
            DIV_ADDRESS => {
                self.mem[DIV_ADDRESS] = 0x00;
            }
            LY_ADDRESS => {} //block LY_ADDRESS write, only ppu can write to this
            FORBIDDEN_ADDRESS_START..=FORBIDDEN_ADDRESS_END => {} //forbidden memory is forbidden
            STAT_ADDRESS => {
                //bottom three bits (0-2) are read only
                let current_lower_bits = self.mem[address] & 0b0000_0111;
                let filtered_value = (value & 0b1111_1000) | current_lower_bits;
                log::trace!(
                    "({:#06x}) STAT changed from {:#010b} to {:#010b} (unfiltered {:#010b})",
                    self.pc,
                    self.mem[address],
                    filtered_value,
                    value
                );
                self.mem[address] = filtered_value;
            }
            LCDC_ADDRESS => {
                let is_vblank = self.mem[STAT_ADDRESS] & 0b11 == 1;
                log::trace!(
                    "({:#06x}) LCDC changed from {:#010b} to {:#010b}, During VBlank?: {:?}",
                    self.pc,
                    self.mem[address],
                    value,
                    is_vblank
                );
                self.mem[address] = value;
            }
            OAM_DMA_ADDRESS => {
                log::trace!("({:#06x}) Writing to OAM DMA 0x{:x}", self.pc, value);
                if !self.is_oam_dma_active {
                    self.oam_dma(value);
                } else {
                    self.mem[OAM_DMA_ADDRESS] = value;
                }
            }
            TIMA_ADDRESS => {
                self.log_mem_change("TIMA", address, value);
                self.mem[address] = value;
            }
            TAC_ADDRESS => {
                self.log_mem_change("TAC", address, value);
                self.mem[address] = value;
            }
            INTERRUPT_FLAG_ADDRESS => {
                self.log_mem_change("IF", address, value);
                self.mem[address] = value;
            }
            INTERRUPT_ENABLE_ADDRESS => {
                self.log_mem_change("IE", address, value);
                self.mem[address] = value;
            }
            SB_SERIAL_OUT_ADDRESS => {
                self.log_mem_change("SB", address, value);
                self.mem[address] = value;
            }
            SC_SERIAL_CONTROL_ADDRESS => {
                self.log_mem_change("SC", address, value);
                self.mem[address] = value;
            }
            JOYPAD_ADDRESS => {
                let filtered_value = (value & 0b0011_0000) | (self.mem[address] & 0b0000_1111);
                log::trace!(
                    "({:#06x}) JOYPAD changed from {:#010b} to {:#010b} (unfiltered {:#010b})",
                    self.pc,
                    self.mem[address],
                    filtered_value,
                    value
                );
                self.mem[address] = filtered_value;
                self.update_joypad();
            }
            NR52_SOUND_ON_OFF_ADDRESS => {
                if self.is_apu_active() {
                    self.mem[address] = (value & 0b1000_0000) | self.mem[address];
                }
            }
            NR51_SOUND_PANNING_ADDRESS | NR50_VOLUME_VIN_PANNING_ADDRESS => {
                if self.is_apu_active() {
                    self.mem[address] = value;
                }
            }
            NR10_CHANNEL1_SWEEP_ADDRESS
            | NR11_CHANNEL1_LENGTH_DUTY_ADDRESS
            | NR12_CHANNEL1_VOLUME_ENVELOPE_ADDRESS
            | NR13_CHANNEL1_WAVELENGTH_LOW_ADDRESS
            | NR14_CHANNEL1_WAVELENGTH_HIGH_AND_CONTROL_ADDRESS => {
                if self.is_apu_active() {
                    self.mem[address] = value;
                }
            }
            NR21_CHANNEL2_LENGTH_DUTY_ADDRESS
            | NR22_CHANNEL2_VOLUME_ENVELOPE_ADDRESS
            | NR23_CHANNEL2_WAVELENGTH_LOW_ADDRESS
            | NR24_CHANNEL2_WAVELENGTH_HIGH_AND_CONTROL_ADDRESS => {
                if self.is_apu_active() {
                    self.mem[address] = value;
                }
            }
            NR30_CHANNEL3_DAC_ENABLE_ADDRESS
            | NR31_CHANNEL3_LENGTH_TIMER_ADDRESS
            | NR32_CHANNEL3_OUTPUT_LEVEL_ADDRESS
            | NR33_CHANNEL3_WAVELENGTH_LOW_ADDRESS
            | NR34_CHANNEL3_WAVELENGTH_HIGH_AND_CONTROL_ADDRESS => {
                if self.is_apu_active() {
                    self.mem[address] = value;
                }
            }
            WAVE_PATTERN_RAM_START_ADDRESS..=WAVE_PATTERN_RAM_END_ADDRESS => {
                if self.is_apu_active() {
                    self.mem[address] = value;
                }
            }
            NR41_CHANNEL4_LENGTH_TIMER_ADDRESS
            | NR42_CHANNEL_4_VOLUME_ENVELOPE_ADDRESS
            | NR43_CHANNEL_4_FREQUENCY_AND_RANDOMNESS_ADDRESS
            | NR44_CHANNEL_4_CONTROL_ADDRESS => {
                if self.is_apu_active() {
                    self.mem[address] = value;
                }
            }
            _ => self.mem[address] = value,
        }
    }

    fn log_mem_change(&self, name: &str, address: usize, new_value: u8) {
        log::trace!(
            "({:#06x}) {} changed from {:#010b} to {:#010b}",
            self.pc,
            name,
            self.mem[address],
            new_value
        )
    }

    fn is_apu_active(&self) -> bool {
        self.mem[NR52_SOUND_ON_OFF_ADDRESS] & 0b1000_0000 != 0
    }

    fn oam_dma(&mut self, start_address_high_byte: u8) {
        //Source:      $XX00-$XX9F   ;XX = $00 to $DF
        //Destination: $FE00-$FE9F
        log::trace!("({:#06x}) OAM DMA TRIGGERED", self.pc);
        if start_address_high_byte > 0xdf {
            // invalid start address, do not execute oam dma
            log::trace!(
                "({:#06x}) warning: program attempted invalid oam dma (invalid start address)",
                self.pc
            );
            return;
        }

        self.mem[OAM_DMA_ADDRESS] = start_address_high_byte;

        self.is_oam_dma_active = true;
        self.dma_active_cycle_counter = 0;

        let start_address = (start_address_high_byte as usize) << 8;
        // when oma dma is started, a base source address is stored, 0xZZ00, where the
        // start address is the ZZ byte, the copy reads from 0xZZ00 - 0xZZ9f
        for offset in 0x00..=0x9f {
            self.mem[0xfe00 + offset] = self.mem[start_address + offset];
        }
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

    fn read_reg(&self, reg_index: u8) -> u8 {
        // 0 => B, 1 => C, 2 => D, 3 => E, 4 => H, 5 => L, 6 => (HL), 7 => A
        let hl = self.get_hl() as usize;
        match reg_index {
            0 => self.b,
            1 => self.c,
            2 => self.d,
            3 => self.e,
            4 => self.h,
            5 => self.l,
            6 => self.read_mem(hl),
            7 => self.a,
            _ => panic!("invalid from_reg value in load operation"),
        }
    }

    fn write_reg(&mut self, reg_index: u8, value: u8) {
        let hl = self.get_hl() as usize;
        match reg_index {
            0 => self.b = value,
            1 => self.c = value,
            2 => self.d = value,
            3 => self.e = value,
            4 => self.h = value,
            5 => self.l = value,
            6 => self.write_mem(hl, value),
            7 => self.a = value,
            _ => panic!("invalid to_reg value in load operation"),
        };
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
        self.pc = 0x0100;
        self.sp = 0xfffe;

        // hardware registers
        self.mem[0xff00] = 0xcf; //p1
        self.mem[0xff01] = 0x00; //sb
        self.mem[0xff02] = 0x7e; //sc
        self.mem[DIV_ADDRESS] = 0xab; //div
        self.mem[TIMA_ADDRESS] = 0x00; //tima
        self.mem[TMA_ADDRESS] = 0x00; //tma
        self.mem[TAC_ADDRESS] = 0xf8; //tac
        self.mem[INTERRUPT_FLAG_ADDRESS] = 0xe1; //if

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

        self.mem[LCDC_ADDRESS] = 0b1001_0001; //0x91 //lcdc
        self.mem[STAT_ADDRESS] = 0b1000_0101; //0x85 //stat
        self.mem[0xff42] = 0x00; //scy
        self.mem[0xff43] = 0x00; //scx
        self.mem[LY_ADDRESS] = 0x00; //ly
        self.mem[LYC_ADDRESS] = 0x00; //lyc
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
        self.mem[INTERRUPT_ENABLE_ADDRESS] = 0x00; //ie
    }

    fn build_bytecode_table() -> BytecodeTable {
        // initialize table with all opcodes as not_implemented
        let mut table: BytecodeTable = [Cpu::not_implemented; 256];

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
        table[0xe9] = Cpu::jump_hl;
        table[0xc2] = Cpu::jump_conditional;
        table[0xd2] = Cpu::jump_conditional;
        table[0xca] = Cpu::jump_conditional;
        table[0xda] = Cpu::jump_conditional;
        table[0x18] = Cpu::jr;
        table[0x20] = Cpu::jr_conditional;
        table[0x30] = Cpu::jr_conditional;
        table[0x28] = Cpu::jr_conditional;
        table[0x38] = Cpu::jr_conditional;
        table[0xcb] = Cpu::prefix;

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

        table[0x08] = Cpu::load_16bit_sp_indirect_value;
        table[0xf8] = Cpu::load_16bit_hl_sp_offset;
        table[0xf9] = Cpu::load_16bit_sp_from_hl;

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

        table[0xf0] = Cpu::load_8bit_a_from_io_immediate_offset;
        table[0xe0] = Cpu::load_8bit_io_from_a_immediate_offset;
        table[0xf2] = Cpu::load_8bit_a_from_io_c_offset;
        table[0xe2] = Cpu::load_8bit_io_from_a_c_offset;
        table[0xea] = Cpu::load_8bit_memory_from_a;
        table[0xfa] = Cpu::load_8bit_a_from_memory;

        for i in 0..64usize {
            if 0x40 + i == 0x76 {
                // this is the halt op
                continue;
            }
            table[0x40 + i] = Cpu::load_8bit_reg_to_reg;
        }

        table[0xe8] = Cpu::add_16bit_sp_r8_immediate;
        table[0x09] = Cpu::add_16bit_hl_from_reg;
        table[0x19] = Cpu::add_16bit_hl_from_reg;
        table[0x29] = Cpu::add_16bit_hl_from_reg;
        table[0x39] = Cpu::add_16bit_hl_from_reg;
        table[0x03] = Cpu::inc_16bit_reg;
        table[0x13] = Cpu::inc_16bit_reg;
        table[0x23] = Cpu::inc_16bit_reg;
        table[0x33] = Cpu::inc_16bit_reg;
        table[0x0b] = Cpu::dec_16bit_reg;
        table[0x1b] = Cpu::dec_16bit_reg;
        table[0x2b] = Cpu::dec_16bit_reg;
        table[0x3b] = Cpu::dec_16bit_reg;

        for i in 0..8usize {
            table[0x80 + i] = Cpu::add_8bit_a_reg;
            table[0x90 + i] = Cpu::sub_8bit_a_reg;
            table[0xa0 + i] = Cpu::and_8bit_a_reg;
            table[0xb0 + i] = Cpu::or_8bit_a_reg;
            table[0x88 + i] = Cpu::adc_8bit_a_reg;
            table[0x98 + i] = Cpu::sbc_8bit_a_reg;
            table[0xb8 + i] = Cpu::cp_8bit_a_reg;
        }

        table[0xc6] = Cpu::add_8bit_a_immediate;
        table[0xd6] = Cpu::sub_8bit_a_immediate;
        table[0xe6] = Cpu::and_8bit_a_immediate;
        table[0xf6] = Cpu::or_8bit_a_immediate;
        table[0xce] = Cpu::adc_8bit_a_immediate;
        table[0xde] = Cpu::sbc_8bit_a_immediate;
        table[0xee] = Cpu::xor_8bit_a_immediate;
        table[0xfe] = Cpu::cp_8bit_a_immediate;

        table[0x04] = Cpu::inc_8bit_reg;
        table[0x14] = Cpu::inc_8bit_reg;
        table[0x24] = Cpu::inc_8bit_reg;
        table[0x34] = Cpu::inc_8bit_reg;
        table[0x0c] = Cpu::inc_8bit_reg;
        table[0x1c] = Cpu::inc_8bit_reg;
        table[0x2c] = Cpu::inc_8bit_reg;
        table[0x3c] = Cpu::inc_8bit_reg;

        table[0x05] = Cpu::dec_8bit_reg;
        table[0x15] = Cpu::dec_8bit_reg;
        table[0x25] = Cpu::dec_8bit_reg;
        table[0x35] = Cpu::dec_8bit_reg;
        table[0x0d] = Cpu::dec_8bit_reg;
        table[0x1d] = Cpu::dec_8bit_reg;
        table[0x2d] = Cpu::dec_8bit_reg;
        table[0x3d] = Cpu::dec_8bit_reg;

        table[0x27] = Cpu::daa_8bit;
        table[0x37] = Cpu::scf_8bit;
        table[0x3f] = Cpu::ccf_8bit;
        table[0x2f] = Cpu::cpl_8bit;

        table[0x07] = Cpu::rlca_8bit;
        table[0x0f] = Cpu::rrca_8bit;
        table[0x17] = Cpu::rla_8bit;
        table[0x1f] = Cpu::rra_8bit;

        table[0xcd] = Cpu::call;
        table[0xc4] = Cpu::call_conditional_nz;
        table[0xcc] = Cpu::call_conditional_z;
        table[0xd4] = Cpu::call_conditional_nc;
        table[0xdc] = Cpu::call_conditional_c;
        table[0xc9] = Cpu::ret;
        table[0xc0] = Cpu::ret_conditional_nz;
        table[0xc8] = Cpu::ret_conditional_z;
        table[0xd0] = Cpu::ret_conditional_nc;
        table[0xd8] = Cpu::ret_conditional_c;
        table[0xd9] = Cpu::reti;

        table[0xc7] = Cpu::rst;
        table[0xd7] = Cpu::rst;
        table[0xe7] = Cpu::rst;
        table[0xf7] = Cpu::rst;
        table[0xcf] = Cpu::rst;
        table[0xdf] = Cpu::rst;
        table[0xef] = Cpu::rst;
        table[0xff] = Cpu::rst;

        table[0xc1] = Cpu::pop;
        table[0xd1] = Cpu::pop;
        table[0xe1] = Cpu::pop;
        table[0xf1] = Cpu::pop;
        table[0xc5] = Cpu::push;
        table[0xd5] = Cpu::push;
        table[0xe5] = Cpu::push;
        table[0xf5] = Cpu::push;

        table[0xf3] = Cpu::disable_interrupt;
        table[0xfb] = Cpu::enable_interrupt;

        table[0x76] = Cpu::halt;
        table[0x10] = Cpu::stop;

        table
    }

    fn build_cb_bytecode_table() -> BytecodeTable {
        // initialize table with all opcodes as not_implemented
        let mut table: BytecodeTable = [Cpu::cb_not_implemented; 256];

        for i in 0..8usize {
            table[0x00 + i] = Cpu::cb_rlc;
            table[0x08 + i] = Cpu::cb_rrc;
            table[0x10 + i] = Cpu::cb_rl;
            table[0x18 + i] = Cpu::cb_rr;
            table[0x20 + i] = Cpu::cb_sla;
            table[0x28 + i] = Cpu::cb_sra;
            table[0x30 + i] = Cpu::cb_swap;
            table[0x38 + i] = Cpu::cb_srl;
        }

        for i in 0..64usize {
            table[0x40 + i] = Cpu::cb_bit;
            table[0x80 + i] = Cpu::cb_res;
            table[0xc0 + i] = Cpu::cb_set;
        }

        table
    }

    fn not_implemented(&mut self, opcode: u8) -> CycleCount {
        panic!(
            "cpu function not implemented\nopcode 0x{:02x}\n{}",
            opcode,
            self.dump_registers()
        );
    }

    fn cb_not_implemented(&mut self, opcode: u8) -> CycleCount {
        panic!(
            "CB cpu function not implemented\nopcode 0x{:02x}\n{}",
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

    //0xcb
    fn prefix(&mut self, _: u8) -> CycleCount {
        let inner_opcode = self.read_mem(self.pc + 1);
        let inner_cycle_count = self.cb_bytecode_table[inner_opcode as usize](self, inner_opcode);
        4 + inner_cycle_count
    }

    //0xc3
    fn jump(&mut self, _: u8) -> CycleCount {
        let target = ((self.read_mem(self.pc + 2) as u16) << 8) | self.read_mem(self.pc + 1) as u16;
        log::trace!("({:#06x}) jumping to {:#06x}", self.pc, target);
        self.pc = target as usize;
        16
    }

    //0xe9
    fn jump_hl(&mut self, _: u8) -> CycleCount {
        log::trace!("({:#06x}) jumping to {:#06x}", self.pc, self.get_hl());
        self.pc = self.get_hl() as usize;
        4
    }

    //0xc2,d2,ca,da
    fn jump_conditional(&mut self, opcode: u8) -> CycleCount {
        //0b110cc010
        let cc = (opcode & 0b000_11_000) >> 3;

        // Cc Condition Flag
        // 00 NZ        Z = 0
        // 01 Z         Z = 1
        // 10 NC        CY = 0
        // 11 C         CY = 1

        let mut condition = false;
        match cc {
            0 => {
                //nz
                condition = self.f & Z_FLAG_MASK == 0;
            }
            1 => {
                //z
                condition = self.f & Z_FLAG_MASK != 0;
            }
            2 => {
                //nc
                condition = self.f & C_FLAG_MASK == 0;
            }
            3 => {
                //c
                condition = self.f & C_FLAG_MASK != 0;
            }
            _ => {}
        }

        if condition {
            let target =
                ((self.read_mem(self.pc + 2) as u16) << 8) | self.read_mem(self.pc + 1) as u16;
            log::trace!("({:#06x}) jumping to {:#06x}", self.pc, target);
            self.pc = target as usize;
            16
        } else {
            self.pc += 3;
            12
        }
    }

    //0x18
    fn jr(&mut self, _: u8) -> CycleCount {
        let offset = self.read_mem(self.pc + 1) as i8;
        // 2 is added at the end as the relative jump is supposed to
        // be effected by reading the two bytes for the instruction
        let mut target;
        if offset >= 0 {
            target = self.pc + (offset as usize);
        } else {
            let z = (offset * -1) as u8;
            target = self.pc - (z as usize);
        }
        target += 2;
        self.pc = target;

        12
    }

    //0x20,30,28,38
    fn jr_conditional(&mut self, opcode: u8) -> CycleCount {
        //0b001_cc_000
        let cc = (opcode & 0b000_11_000) >> 3;

        // Cc Condition Flag
        // 00 NZ        Z = 0
        // 01 Z         Z = 1
        // 10 NC        CY = 0
        // 11 C         CY = 1

        let mut condition = false;
        match cc {
            0 => {
                //nz
                condition = self.f & Z_FLAG_MASK == 0;
            }
            1 => {
                //z
                condition = self.f & Z_FLAG_MASK != 0;
            }
            2 => {
                //nc
                condition = self.f & C_FLAG_MASK == 0;
            }
            3 => {
                //c
                condition = self.f & C_FLAG_MASK != 0;
            }
            _ => {}
        }

        if condition {
            let offset = self.read_mem(self.pc + 1) as i8;
            let mut target = self.pc as u16;
            if offset >= 0 {
                target = target.wrapping_add(offset as u16);
            } else {
                target = target.wrapping_sub((offset * -1) as u16);
            }
            target += 2;
            self.pc = target as usize;
            12
        } else {
            self.pc += 2;
            8
        }
    }

    //0xc7,d7,e7,f7,cf,df,ef,ff
    fn rst(&mut self, opcode: u8) -> CycleCount {
        //0b11xxx111
        let operand = (opcode & 0b00111000) >> 3;
        let mut target = 0x0000 as u16;
        match operand {
            0 => target = 0x0000,
            1 => target = 0x0008,
            2 => target = 0x0010,
            3 => target = 0x0018,
            4 => target = 0x0020,
            5 => target = 0x0028,
            6 => target = 0x0030,
            7 => target = 0x0038,
            _ => {}
        }

        self.helper_call(target, self.pc + 1);

        16
    }

    //0xa8
    fn xor_8bit_a_b(&mut self, _: u8) -> CycleCount {
        self.a = self.a ^ self.b;

        if self.a == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        self.f &= !N_FLAG_MASK;
        self.f &= !H_FLAG_MASK;
        self.f &= !C_FLAG_MASK;

        self.pc += 1;
        4
    }

    //0xa9
    fn xor_8bit_a_c(&mut self, _: u8) -> CycleCount {
        self.a = self.a ^ self.c;

        if self.a == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        self.f &= !N_FLAG_MASK;
        self.f &= !H_FLAG_MASK;
        self.f &= !C_FLAG_MASK;

        self.pc += 1;
        4
    }

    //0xaa
    fn xor_8bit_a_d(&mut self, _: u8) -> CycleCount {
        self.a = self.a ^ self.d;

        if self.a == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        self.f &= !N_FLAG_MASK;
        self.f &= !H_FLAG_MASK;
        self.f &= !C_FLAG_MASK;

        self.pc += 1;
        4
    }

    //0xab
    fn xor_8bit_a_e(&mut self, _: u8) -> CycleCount {
        self.a = self.a ^ self.e;

        if self.a == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        self.f &= !N_FLAG_MASK;
        self.f &= !H_FLAG_MASK;
        self.f &= !C_FLAG_MASK;

        self.pc += 1;
        4
    }

    //0xac
    fn xor_8bit_a_h(&mut self, _: u8) -> CycleCount {
        self.a = self.a ^ self.h;

        if self.a == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        self.f &= !N_FLAG_MASK;
        self.f &= !H_FLAG_MASK;
        self.f &= !C_FLAG_MASK;

        self.pc += 1;
        4
    }

    //0xad
    fn xor_8bit_a_l(&mut self, _: u8) -> CycleCount {
        self.a = self.a ^ self.l;

        if self.a == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        self.f &= !N_FLAG_MASK;
        self.f &= !H_FLAG_MASK;
        self.f &= !C_FLAG_MASK;

        self.pc += 1;
        4
    }

    //0xae
    fn xor_8bit_a_hl_indirect(&mut self, _: u8) -> CycleCount {
        self.a = self.a ^ self.read_mem(self.get_hl() as usize);

        if self.a == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        self.f &= !N_FLAG_MASK;
        self.f &= !H_FLAG_MASK;
        self.f &= !C_FLAG_MASK;

        self.pc += 1;
        4
    }

    //0xaf
    fn xor_8bit_a_a(&mut self, _: u8) -> CycleCount {
        self.a = 0;

        self.f |= Z_FLAG_MASK; //always zero
        self.f &= !N_FLAG_MASK;
        self.f &= !H_FLAG_MASK;
        self.f &= !C_FLAG_MASK;

        self.pc += 1;
        4
    }

    //0xee
    fn xor_8bit_a_immediate(&mut self, _: u8) -> CycleCount {
        let val = self.read_mem(self.pc + 1);
        self.a = self.a ^ val;

        if self.a == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        self.f &= !N_FLAG_MASK;
        self.f &= !H_FLAG_MASK;
        self.f &= !C_FLAG_MASK;

        self.pc += 2;
        4
    }

    //0x80 - 0x87
    fn add_8bit_a_reg(&mut self, opcode: u8) -> CycleCount {
        //opcode 0b01_000_xxx
        let from_reg = opcode & 0b00_000_111;

        let val = self.read_reg(from_reg);
        let (result, overflow_occurred) = self.a.overflowing_add(val);

        //update flags
        self.f &= !N_FLAG_MASK; // always flip N flag off (not subtraction)

        //is result zero flag
        if result == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        //carry flag
        if overflow_occurred {
            self.f |= C_FLAG_MASK; // flip on
        } else {
            self.f &= !C_FLAG_MASK; // flip off
        }

        //half carry flag
        //mask out upper nibble and see if result flips 0x10 bit (meaning
        //there was a half carry as result was greater than 0x0f)
        if ((self.a & 0x0f) + (val & 0x0f)) & 0x10 != 0 {
            self.f |= H_FLAG_MASK; // flip on
        } else {
            self.f &= !H_FLAG_MASK; // flip off
        }

        //store result
        self.a = result;
        self.pc += 1;

        if from_reg == 6 {
            8
        } else {
            4
        }
    }

    // 0xc6
    fn add_8bit_a_immediate(&mut self, _: u8) -> CycleCount {
        let val = self.read_mem(self.pc + 1);
        let (result, overflow_occurred) = self.a.overflowing_add(val);

        //update flags
        self.f &= !N_FLAG_MASK; // always flip N flag off (not subtraction)

        //is result zero flag
        if result == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        //carry flag
        if overflow_occurred {
            self.f |= C_FLAG_MASK; // flip on
        } else {
            self.f &= !C_FLAG_MASK; // flip off
        }

        //half carry flag
        //mask out upper nibble and see if result flips 0x10 bit (meaning
        //there was a half carry as result was greater than 0x0f)
        if ((self.a & 0x0f) + (val & 0x0f)) & 0x10 != 0 {
            self.f |= H_FLAG_MASK; // flip on
        } else {
            self.f &= !H_FLAG_MASK; // flip off
        }

        //store result
        self.a = result;

        self.pc += 2;

        8
    }

    // 0x90 - 0x97
    fn sub_8bit_a_reg(&mut self, opcode: u8) -> CycleCount {
        //0b10010_xxx
        let from_reg = opcode & 0b00_000_111;

        let val = self.read_reg(from_reg);
        let (result, overflow_occurred) = self.a.overflowing_sub(val);

        //update flags
        self.f |= N_FLAG_MASK; // always flip N flag on

        //is result zero flag
        if result == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        //carry flag
        if overflow_occurred {
            self.f |= C_FLAG_MASK; // flip on
        } else {
            self.f &= !C_FLAG_MASK; // flip off
        }

        //half carry flag
        //mask out upper nibble and see if result flips 0x10 bit (meaning
        //there was a half carry as result was greater than 0x0f)
        if (self.a & 0x0f).wrapping_sub(val & 0x0f) & 0x10 != 0 {
            self.f |= H_FLAG_MASK; // flip on
        } else {
            self.f &= !H_FLAG_MASK; // flip off
        }

        //store result
        self.a = result;

        self.pc += 1;

        if from_reg == 6 {
            8
        } else {
            4
        }
    }

    //0xd6
    fn sub_8bit_a_immediate(&mut self, _: u8) -> CycleCount {
        let val = self.read_mem(self.pc + 1);
        let (result, overflow_occurred) = self.a.overflowing_sub(val);

        //update flags
        self.f |= N_FLAG_MASK; // always flip N flag on

        //is result zero flag
        if result == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        //carry flag
        if overflow_occurred {
            self.f |= C_FLAG_MASK; // flip on
        } else {
            self.f &= !C_FLAG_MASK; // flip off
        }

        //half carry flag
        //mask out upper nibble and see if result flips 0x10 bit (meaning
        //there was a half carry as result was greater than 0x0f)
        if (self.a & 0x0f).wrapping_sub(val & 0x0f) & 0x10 != 0 {
            self.f |= H_FLAG_MASK; // flip on
        } else {
            self.f &= !H_FLAG_MASK; // flip off
        }

        //store result
        self.a = result;

        self.pc += 2;

        8
    }

    // 0xa0 - 0xa7
    fn and_8bit_a_reg(&mut self, opcode: u8) -> CycleCount {
        //0b10100xxx
        let from_reg = opcode & 0b00_000_111;

        let val = self.read_reg(from_reg);
        let result = self.a & val;

        self.f &= !N_FLAG_MASK;
        self.f |= H_FLAG_MASK;
        self.f &= !C_FLAG_MASK;

        //is result zero flag
        if result == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        //store result
        self.a = result;
        self.pc += 1;

        if from_reg == 6 {
            8
        } else {
            4
        }
    }

    //0xe6
    fn and_8bit_a_immediate(&mut self, _: u8) -> CycleCount {
        let val = self.read_mem(self.pc + 1);
        let result = self.a & val;

        self.f &= !N_FLAG_MASK;
        self.f |= H_FLAG_MASK;
        self.f &= !C_FLAG_MASK;

        //is result zero flag
        if result == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        //store result
        self.a = result;
        self.pc += 2;

        8
    }

    //0xb0 - 0xb7
    fn or_8bit_a_reg(&mut self, opcode: u8) -> CycleCount {
        //0b10100xxx
        let from_reg = opcode & 0b00_000_111;

        let val = self.read_reg(from_reg);
        let result = self.a | val;

        self.f &= !N_FLAG_MASK;
        self.f &= !H_FLAG_MASK;
        self.f &= !C_FLAG_MASK;

        //is result zero flag
        if result == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        //store result
        self.a = result;
        self.pc += 1;

        if from_reg == 6 {
            8
        } else {
            4
        }
    }

    fn or_8bit_a_immediate(&mut self, _: u8) -> CycleCount {
        let val = self.read_mem(self.pc + 1);
        let result = self.a | val;

        self.f &= !N_FLAG_MASK;
        self.f &= !H_FLAG_MASK;
        self.f &= !C_FLAG_MASK;

        //is result zero flag
        if result == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        //store result
        self.a = result;
        self.pc += 2;

        8
    }

    // 0xb8 - 0xbf
    fn cp_8bit_a_reg(&mut self, opcode: u8) -> CycleCount {
        // basically the same, except we don't store the result
        //0b10111_xxx
        let from_reg = opcode & 0b00_000_111;

        let val = self.read_reg(from_reg);
        let (result, overflow_occurred) = self.a.overflowing_sub(val);

        //update flags
        self.f |= N_FLAG_MASK; // always flip N flag on

        //is result zero flag
        if result == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        //carry flag
        if overflow_occurred {
            self.f |= C_FLAG_MASK; // flip on
        } else {
            self.f &= !C_FLAG_MASK; // flip off
        }

        //half carry flag
        //mask out upper nibble and see if result flips 0x10 bit (meaning
        //there was a half carry as result was greater than 0x0f)
        if (self.a & 0x0f).wrapping_sub(val & 0x0f) & 0x10 != 0 {
            self.f |= H_FLAG_MASK; // flip on
        } else {
            self.f &= !H_FLAG_MASK; // flip off
        }

        //do NOT store the result (this is compare)
        self.pc += 1;

        if from_reg == 6 {
            8
        } else {
            4
        }
    }

    //0xfe
    fn cp_8bit_a_immediate(&mut self, _: u8) -> CycleCount {
        // basically the same, except we don't store the result
        let val = self.read_mem(self.pc + 1);
        let (result, overflow_occurred) = self.a.overflowing_sub(val);

        //update flags
        self.f |= N_FLAG_MASK; // always flip N flag on

        //is result zero flag
        if result == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        //carry flag
        if overflow_occurred {
            self.f |= C_FLAG_MASK; // flip on
        } else {
            self.f &= !C_FLAG_MASK; // flip off
        }

        //half carry flag
        //mask out upper nibble and see if result flips 0x10 bit (meaning
        //there was a half carry as result was greater than 0x0f)
        if (self.a & 0x0f).wrapping_sub(val & 0x0f) & 0x10 != 0 {
            self.f |= H_FLAG_MASK; // flip on
        } else {
            self.f &= !H_FLAG_MASK; // flip off
        }

        //do NOT store the result (this is compare)
        self.pc += 2;

        8
    }

    //0x88 - 0x8f
    fn adc_8bit_a_reg(&mut self, opcode: u8) -> CycleCount {
        //0b10001_xxx
        let from_reg = opcode & 0b00_000_111;
        let val = self.read_reg(from_reg);
        let last_carry_bit = (self.f & C_FLAG_MASK) >> 4;

        let (result_a, overflow_a) = self.a.overflowing_add(val);
        let (result_b, overflow_b) = result_a.overflowing_add(last_carry_bit);

        //update flags
        self.f &= !N_FLAG_MASK; // always flip N flag off (not subtraction)

        //is result zero flag
        if result_b == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        //carry flag
        if overflow_a || overflow_b {
            self.f |= C_FLAG_MASK; // flip on
        } else {
            self.f &= !C_FLAG_MASK; // flip off
        }

        //half carry flag
        //mask out upper nibble and see if result flips 0x10 bit (meaning
        //there was a half carry as result was greater than 0x0f)
        if ((self.a & 0x0f) + (val & 0x0f) + last_carry_bit) & 0x10 != 0 {
            self.f |= H_FLAG_MASK; // flip on
        } else {
            self.f &= !H_FLAG_MASK; // flip off
        }

        // store result into a reg
        self.a = result_b;
        self.pc += 1;

        if from_reg == 6 {
            8
        } else {
            4
        }
    }

    //0xce
    fn adc_8bit_a_immediate(&mut self, _: u8) -> CycleCount {
        let val = self.read_mem(self.pc + 1);
        let last_carry_bit = (self.f & C_FLAG_MASK) >> 4;

        let (result_a, overflow_a) = self.a.overflowing_add(val);
        let (result_b, overflow_b) = result_a.overflowing_add(last_carry_bit);

        //update flags
        self.f &= !N_FLAG_MASK; // always flip N flag off (not subtraction)

        //is result zero flag
        if result_b == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        //carry flag
        if overflow_a || overflow_b {
            self.f |= C_FLAG_MASK; // flip on
        } else {
            self.f &= !C_FLAG_MASK; // flip off
        }

        //half carry flag
        //mask out upper nibble and see if result flips 0x10 bit (meaning
        //there was a half carry as result was greater than 0x0f)
        if ((self.a & 0x0f) + (val & 0x0f) + last_carry_bit) & 0x10 != 0 {
            self.f |= H_FLAG_MASK; // flip on
        } else {
            self.f &= !H_FLAG_MASK; // flip off
        }

        // store result into a reg
        self.a = result_b;
        self.pc += 2;

        8
    }

    //0x98 - 0x9f
    fn sbc_8bit_a_reg(&mut self, opcode: u8) -> CycleCount {
        //0b10011_xxx
        let from_reg = opcode & 0b00_000_111;
        let val = self.read_reg(from_reg);
        let last_carry_bit = (self.f & C_FLAG_MASK) >> 4;

        let (result_a, overflow_a) = self.a.overflowing_sub(val);
        let (result_b, overflow_b) = result_a.overflowing_sub(last_carry_bit);

        //update flags
        self.f |= N_FLAG_MASK; // always flip N flag on

        //is result zero flag
        if result_b == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        //carry flag
        if overflow_a || overflow_b {
            self.f |= C_FLAG_MASK; // flip on
        } else {
            self.f &= !C_FLAG_MASK; // flip off
        }

        //half carry flag
        //mask out upper nibble and see if result flips 0x10 bit (meaning
        //there was a half carry as result was greater than 0x0f)
        let sub_a = (self.a & 0x0f).wrapping_sub(val & 0x0f);
        let sub_b = sub_a.wrapping_sub(last_carry_bit);
        if sub_b & 0x10 != 0 {
            self.f |= H_FLAG_MASK; // flip on
        } else {
            self.f &= !H_FLAG_MASK; // flip off
        }

        //store result
        self.a = result_b;
        self.pc += 1;

        if from_reg == 6 {
            8
        } else {
            4
        }
    }

    //0xde
    fn sbc_8bit_a_immediate(&mut self, _: u8) -> CycleCount {
        let val = self.read_mem(self.pc + 1);
        let last_carry_bit = (self.f & C_FLAG_MASK) >> 4;

        let (result_a, overflow_a) = self.a.overflowing_sub(val);
        let (result_b, overflow_b) = result_a.overflowing_sub(last_carry_bit);

        //update flags
        self.f |= N_FLAG_MASK; // always flip N flag on

        //is result zero flag
        if result_b == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        //carry flag
        if overflow_a || overflow_b {
            self.f |= C_FLAG_MASK; // flip on
        } else {
            self.f &= !C_FLAG_MASK; // flip off
        }

        //half carry flag
        //mask out upper nibble and see if result flips 0x10 bit (meaning
        //there was a half carry as result was greater than 0x0f)
        let sub_a = (self.a & 0x0f).wrapping_sub(val & 0x0f);
        let sub_b = sub_a.wrapping_sub(last_carry_bit);
        if sub_b & 0x10 != 0 {
            self.f |= H_FLAG_MASK; // flip on
        } else {
            self.f &= !H_FLAG_MASK; // flip off
        }

        //store result
        self.a = result_b;
        self.pc += 2;

        8
    }

    //0x04,14,24,34,0c,1c,2c,3c
    fn inc_8bit_reg(&mut self, opcode: u8) -> CycleCount {
        //0b00_xxx_100
        let from_reg = (opcode & 0b00_111_000) >> 3;
        let val = self.read_reg(from_reg);
        let result = val.wrapping_add(1);

        self.f &= !N_FLAG_MASK;

        //is result zero flag
        if result == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        if (val & 0x0f) == 0x0f {
            self.f |= H_FLAG_MASK; // flip on
        } else {
            self.f &= !H_FLAG_MASK; // flip off
        }

        self.write_reg(from_reg, result);
        self.pc += 1;

        if from_reg == 6 {
            12
        } else {
            4
        }
    }

    //0x05,15,25,35,0d,1d,2d,3d
    fn dec_8bit_reg(&mut self, opcode: u8) -> CycleCount {
        //0b00_xxx_101
        let from_reg = (opcode & 0b00_111_000) >> 3;
        let val = self.read_reg(from_reg);
        let result = val.wrapping_sub(1);

        self.f |= N_FLAG_MASK;

        //is result zero flag
        if result == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        if (val & 0x0f) == 0x00 {
            self.f |= H_FLAG_MASK; // flip on
        } else {
            self.f &= !H_FLAG_MASK; // flip off
        }

        self.write_reg(from_reg, result);
        self.pc += 1;

        if from_reg == 6 {
            12
        } else {
            4
        }
    }

    //0x27
    fn daa_8bit(&mut self, _: u8) -> CycleCount {
        // using flags, determines the binary coded decimal format of the last result
        // used https://forums.nesdev.org/viewtopic.php?t=15944
        // the gameboy cpu has specific quirks that makes this tricky to figure out
        // if you're relying on z80 documentation
        let n_flag = (self.f & N_FLAG_MASK) >> 6;
        let h_flag = (self.f & H_FLAG_MASK) >> 5;
        let c_flag = (self.f & C_FLAG_MASK) >> 4;

        if n_flag == 0 {
            // after an addition, adjust if (half-)carry occurred or if result is out of bounds
            if c_flag == 1 || self.a > 0x99 {
                self.a = self.a.wrapping_add(0x60);
                self.f |= C_FLAG_MASK;
            }
            if h_flag == 1 || (self.a & 0x0f) > 0x09 {
                self.a = self.a.wrapping_add(0x06);
            }
        } else {
            // after a subtraction, only adjust if (half-)carry occurred
            if c_flag == 1 {
                self.a = self.a.wrapping_sub(0x60);
            }
            if h_flag == 1 {
                self.a = self.a.wrapping_sub(0x06);
            }
        }

        // // these flags are always updated
        if self.a == 0 {
            self.f |= Z_FLAG_MASK; // flip on
        } else {
            self.f &= !Z_FLAG_MASK; // flip off
        }

        self.f &= !H_FLAG_MASK; //turn off h flag

        self.pc += 1;

        4
    }

    //0x37
    fn scf_8bit(&mut self, _: u8) -> CycleCount {
        self.f |= C_FLAG_MASK;
        self.f &= !N_FLAG_MASK; // turn off n flag
        self.f &= !H_FLAG_MASK; //turn off h flag
        self.pc += 1;
        4
    }

    //0x3f
    fn ccf_8bit(&mut self, _: u8) -> CycleCount {
        self.f ^= C_FLAG_MASK;
        self.f &= !N_FLAG_MASK; // turn off n flag
        self.f &= !H_FLAG_MASK; //turn off h flag
        self.pc += 1;
        4
    }

    //0x2f
    fn cpl_8bit(&mut self, _: u8) -> CycleCount {
        self.a ^= 0xff;
        self.f |= N_FLAG_MASK; //turn on n flag
        self.f |= H_FLAG_MASK; //turn on h flag
        self.pc += 1;
        4
    }

    // note: this 16bit loads are little endian
    //0x01
    fn load_16bit_bc_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = (self.read_mem(self.pc + 2) as u16) << 8 | self.read_mem(self.pc + 1) as u16;
        self.set_bc(value);
        self.pc += 3;
        12
    }

    //0x11
    fn load_16bit_de_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = (self.read_mem(self.pc + 2) as u16) << 8 | self.read_mem(self.pc + 1) as u16;
        self.set_de(value);
        self.pc += 3;
        12
    }

    //0x21
    fn load_16bit_hl_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = (self.read_mem(self.pc + 2) as u16) << 8 | self.read_mem(self.pc + 1) as u16;
        self.set_hl(value);
        self.pc += 3;
        12
    }

    //0x31
    fn load_16bit_sp_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = (self.read_mem(self.pc + 2) as u16) << 8 | self.read_mem(self.pc + 1) as u16;
        self.sp = value as usize;
        self.pc += 3;
        12
    }

    //0x08
    fn load_16bit_sp_indirect_value(&mut self, _: u8) -> CycleCount {
        let target = (self.read_mem(self.pc + 2) as u16) << 8 | self.read_mem(self.pc + 1) as u16;
        // set lower byte
        self.write_mem(target as usize, (self.sp & 0x00ff) as u8);
        // set higher byte
        self.write_mem((target + 1) as usize, (self.sp >> 8) as u8);

        self.pc += 3;
        20
    }

    //0xf8
    fn load_16bit_hl_sp_offset(&mut self, _: u8) -> CycleCount {
        //LD HL, SP+e: H from bit 3, C from bit 7 (flags from low byte op)
        let val = (self.read_mem(self.pc + 1) as i8) as i16;
        let mut sp = self.sp as u16;
        let result;
        if val >= 0 {
            let val = val as u16;
            result = sp.wrapping_add(val);
            //because we're adding an 8bit number, half carry occurs at bit 4, not 11
            let carry_bits = sp ^ val ^ result;
            if carry_bits & 0x0010 != 0 {
                self.f |= H_FLAG_MASK;
            } else {
                self.f &= !H_FLAG_MASK;
            }
            if carry_bits & 0x0100 != 0 {
                self.f |= C_FLAG_MASK;
            } else {
                self.f &= !C_FLAG_MASK;
            }
        } else {
            let val_unsigned = (val * -1) as u16;
            let literal_cast = val as u16;
            result = sp.wrapping_sub(val_unsigned);
            let carry_bits = sp ^ literal_cast ^ result;
            if carry_bits & 0x0010 != 0 {
                self.f |= H_FLAG_MASK;
            } else {
                self.f &= !H_FLAG_MASK;
            }
            if carry_bits & 0x0100 != 0 {
                self.f |= C_FLAG_MASK;
            } else {
                self.f &= !C_FLAG_MASK;
            }
        }

        sp = result;

        self.f &= !Z_FLAG_MASK;
        self.f &= !N_FLAG_MASK;

        self.set_hl(sp as u16);
        self.pc += 2;

        12
    }

    //0xf9
    fn load_16bit_sp_from_hl(&mut self, _: u8) -> CycleCount {
        self.sp = self.get_hl() as usize;
        self.pc += 1;
        8
    }

    //0xe8
    fn add_16bit_sp_r8_immediate(&mut self, _: u8) -> CycleCount {
        //ADD SP, e: H from bit 3, C from bit 7 (flags from low byte op)
        //flags 00hc
        //SP = SP +/- dd ; dd is 8-bit signed number
        let val = (self.read_mem(self.pc + 1) as i8) as i16;
        let sp = self.sp as u16;
        let result;
        if val >= 0 {
            let val = val as u16;
            result = sp.wrapping_add(val);
            //because we're adding an 8bit number, half carry occurs at bit 4, not 11
            let carry_bits = sp ^ val ^ result;
            if carry_bits & 0x0010 != 0 {
                self.f |= H_FLAG_MASK;
            } else {
                self.f &= !H_FLAG_MASK;
            }
            if carry_bits & 0x0100 != 0 {
                self.f |= C_FLAG_MASK;
            } else {
                self.f &= !C_FLAG_MASK;
            }
        } else {
            let val_unsigned = (val * -1) as u16;
            let literal_cast = val as u16;
            result = sp.wrapping_sub(val_unsigned);
            let carry_bits = sp ^ literal_cast ^ result;
            if carry_bits & 0x0010 != 0 {
                self.f |= H_FLAG_MASK;
            } else {
                self.f &= !H_FLAG_MASK;
            }
            if carry_bits & 0x0100 != 0 {
                self.f |= C_FLAG_MASK;
            } else {
                self.f &= !C_FLAG_MASK;
            }
        }

        self.sp = result as usize;

        self.f &= !Z_FLAG_MASK;
        self.f &= !N_FLAG_MASK;

        self.pc += 2;

        16
    }

    //0x09,19,29,39
    fn add_16bit_hl_from_reg(&mut self, opcode: u8) -> CycleCount {
        //0b00xx1001
        // flags -0hc
        let reg_code = (opcode & 0b00110000) >> 4;
        let val;
        match reg_code {
            0 => val = self.get_bc(),
            1 => val = self.get_de(),
            2 => val = self.get_hl(),
            3 => val = self.sp as u16,
            _ => panic!("invalid reg code"),
        }

        let hl = self.get_hl();
        let (result, overflow) = hl.overflowing_add(val);

        if overflow {
            self.f |= C_FLAG_MASK;
        } else {
            self.f &= !C_FLAG_MASK;
        }
        if ((hl & 0x0fff) + (val & 0x0fff)) & 0x1000 != 0 {
            self.f |= H_FLAG_MASK;
        } else {
            self.f &= !H_FLAG_MASK;
        }
        self.f &= !N_FLAG_MASK;

        self.set_hl(result);

        self.pc += 1;

        8
    }

    //0x03,13,23,33
    fn inc_16bit_reg(&mut self, opcode: u8) -> CycleCount {
        //0b00xx0011
        let reg_code = (opcode & 0b00110000) >> 4;
        match reg_code {
            0 => self.set_bc(self.get_bc().wrapping_add(1)),
            1 => self.set_de(self.get_de().wrapping_add(1)),
            2 => self.set_hl(self.get_hl().wrapping_add(1)),
            3 => self.sp = (self.sp as u16).wrapping_add(1) as usize,
            _ => panic!("invalid reg code"),
        }

        self.pc += 1;

        8
    }

    //0x0b,1b,2b,3b
    fn dec_16bit_reg(&mut self, opcode: u8) -> CycleCount {
        //0b00xx1011
        let reg_code = (opcode & 0b00110000) >> 4;
        match reg_code {
            0 => self.set_bc(self.get_bc().wrapping_sub(1)),
            1 => self.set_de(self.get_de().wrapping_sub(1)),
            2 => self.set_hl(self.get_hl().wrapping_sub(1)),
            3 => self.sp = (self.sp as u16).wrapping_sub(1) as usize,
            _ => panic!("invalid reg code"),
        }

        self.pc += 1;

        8
    }

    //0x06
    fn load_8bit_b_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = self.read_mem(self.pc + 1);
        self.b = value;
        self.pc += 2;
        8
    }

    //0x0e
    fn load_8bit_c_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = self.read_mem(self.pc + 1);
        self.c = value;
        self.pc += 2;
        8
    }

    // 0x16
    fn load_8bit_d_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = self.read_mem(self.pc + 1);
        self.d = value;
        self.pc += 2;
        8
    }

    // 0x1e
    fn load_8bit_e_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = self.read_mem(self.pc + 1);
        self.e = value;
        self.pc += 2;
        8
    }

    //0x26
    fn load_8bit_h_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = self.read_mem(self.pc + 1);
        self.h = value;
        self.pc += 2;
        8
    }

    //0x2e
    fn load_8bit_l_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = self.read_mem(self.pc + 1);
        self.l = value;
        self.pc += 2;
        8
    }

    //0x36
    fn load_8bit_hl_indirect_from_immediate_value(&mut self, _: u8) -> CycleCount {
        // load value to hl address
        let value = self.read_mem(self.pc + 1);
        let hl = self.get_hl() as usize;
        self.write_mem(hl, value);
        self.pc += 2;
        12
    }

    //0x3e
    fn load_8bit_a_immediate_value(&mut self, _: u8) -> CycleCount {
        let value = self.read_mem(self.pc + 1);
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
        self.write_mem(address, self.a);
        self.pc += 1;
        8
    }

    //0x12
    fn load_8bit_de_indirect_from_a(&mut self, _: u8) -> CycleCount {
        let address = self.get_de() as usize;
        self.write_mem(address, self.a);
        self.pc += 1;
        8
    }

    //0x22
    fn load_8bit_hl_inc_indirect_from_a(&mut self, _: u8) -> CycleCount {
        //Load to the absolute address specified by the 16-bit register HL,
        //data from the 8-bit A register. The value of HL is incremented
        //after the memory write.
        let address = self.get_hl() as usize;
        self.write_mem(address, self.a);
        self.set_hl((address as u16).wrapping_add(1));
        self.pc += 1;
        8
    }

    //0x32
    fn load_8bit_hl_dec_indirect_from_a(&mut self, _: u8) -> CycleCount {
        //Load to the absolute address specified by the 16-bit register HL,
        //data from the 8-bit A register. The value of HL is decremented
        //after the memory write.
        let address = self.get_hl() as usize;
        self.write_mem(address, self.a);
        self.set_hl(address as u16 - 1);
        self.pc += 1;
        8
    }

    //load to accumulator (indirect BC, DE, HL+, HL-)
    // 0a 1a 2a 3a (BC), (DE), (HL+), or (HL-) stored to A register
    //0x0a
    fn load_8bit_a_from_bc_indirect(&mut self, _: u8) -> CycleCount {
        let address = self.get_bc() as usize;
        self.a = self.read_mem(address);
        self.pc += 1;
        8
    }

    //0x1a
    fn load_8bit_a_from_de_indirect(&mut self, _: u8) -> CycleCount {
        let address = self.get_de() as usize;
        self.a = self.read_mem(address);
        self.pc += 1;
        8
    }

    //0x2a
    fn load_8bit_a_from_hl_inc_indirect(&mut self, _: u8) -> CycleCount {
        //Load to the 8-bit A register, data from the absolute address
        //specified by the 16-bit register HL. The value of HL is
        //incremented after the memory read.

        let address = self.get_hl() as usize;
        self.a = self.read_mem(address);
        self.set_hl(address as u16 + 1);
        self.pc += 1;
        8
    }

    //0x3a
    fn load_8bit_a_from_hl_dec_indirect(&mut self, _: u8) -> CycleCount {
        let address = self.get_hl() as usize;
        self.a = self.read_mem(address);
        self.set_hl(address as u16 - 1);
        self.pc += 1;
        8
    }

    //0x40 through 0x7f, but NOT 0x76 that's HALT
    fn load_8bit_reg_to_reg(&mut self, opcode: u8) -> CycleCount {
        //opcode 0b01_xxx_yyy
        let to_reg = (opcode & 0b00_111_000) >> 3;
        let from_reg = opcode & 0b00_000_111;

        let read_val = self.read_reg(from_reg);
        self.write_reg(to_reg, read_val);
        self.pc += 1;

        if from_reg == 6 || to_reg == 6 {
            8 // indirect mem access using hl takes 8 cycles
        } else {
            4 // all other loads are 4 cycles
        }
    }

    //0xe0
    fn load_8bit_io_from_a_immediate_offset(&mut self, _: u8) -> CycleCount {
        let offset = self.read_mem(self.pc + 1) as usize;
        self.write_mem(0xff00 + offset, self.a);
        self.pc += 2;
        12
    }

    //0xf0
    fn load_8bit_a_from_io_immediate_offset(&mut self, _: u8) -> CycleCount {
        let offset = self.read_mem(self.pc + 1) as usize;
        self.a = self.read_mem(0xff00 + offset);
        self.pc += 2;
        12
    }

    //0xe2
    fn load_8bit_io_from_a_c_offset(&mut self, _: u8) -> CycleCount {
        self.write_mem(0xff00 + self.c as usize, self.a);
        self.pc += 1;
        8
    }

    //0xf2
    fn load_8bit_a_from_io_c_offset(&mut self, _: u8) -> CycleCount {
        self.a = self.read_mem(0xff00 + self.c as usize);
        self.pc += 1;
        8
    }

    //0xea
    fn load_8bit_memory_from_a(&mut self, _: u8) -> CycleCount {
        let address =
            ((self.read_mem(self.pc + 2) as u16) << 8 | self.read_mem(self.pc + 1) as u16) as usize;
        self.write_mem(address, self.a);
        self.pc += 3;
        16
    }

    //0xfa
    fn load_8bit_a_from_memory(&mut self, _: u8) -> CycleCount {
        let address =
            ((self.read_mem(self.pc + 2) as u16) << 8 | self.read_mem(self.pc + 1) as u16) as usize;
        self.a = self.read_mem(address);
        self.pc += 3;
        16
    }

    //8bit bit, rot, shift
    //0x07
    fn rlca_8bit(&mut self, _: u8) -> CycleCount {
        //Rotates a to the left with bit 7 being moved to bit 0 and also stored into the carry

        //store highest bit into carry
        self.f = (self.f & !C_FLAG_MASK) | ((self.a & 0x80) >> 3);
        self.f &= !Z_FLAG_MASK;
        self.f &= !H_FLAG_MASK;
        self.f &= !N_FLAG_MASK;

        self.a = self.a.rotate_left(1);

        self.pc += 1;

        4
    }

    //0x0f
    fn rrca_8bit(&mut self, _: u8) -> CycleCount {
        //Rotates a to the right with bit 0 moved to bit 7 and also stored into the carry.

        //store lowest bit into carry
        self.f = (self.f & !C_FLAG_MASK) | ((self.a & 0x1) << 4);
        self.f &= !Z_FLAG_MASK;
        self.f &= !H_FLAG_MASK;
        self.f &= !N_FLAG_MASK;

        self.a = self.a.rotate_right(1);

        self.pc += 1;

        4
    }

    //0x17
    fn rla_8bit(&mut self, _: u8) -> CycleCount {
        // Rotates a register to the left with the carry's value put into bit 0 and bit 7 is put into the carry.
        let high_bit = self.a >> 7;
        let carry_bit = (self.f & C_FLAG_MASK) >> 4;

        //the current carry bit is placed as the lowest bit in a
        self.a <<= 1;
        self.a |= carry_bit;

        //store highest bit into carry
        self.f = (self.f & !C_FLAG_MASK) | (high_bit << 4);
        self.f &= !Z_FLAG_MASK;
        self.f &= !H_FLAG_MASK;
        self.f &= !N_FLAG_MASK;

        self.pc += 1;

        4
    }

    //0x1f
    fn rra_8bit(&mut self, _: u8) -> CycleCount {
        //Rotates a to the right with the carry put into bit 7 and bit 0 put into the carry flag.
        let low_bit = self.a & 0x01;
        let carry_bit = (self.f & C_FLAG_MASK) >> 4;

        //the current carry bit is placed as the highest bit in a
        self.a >>= 1;
        self.a |= carry_bit << 7;

        //store lowest bit into carry
        self.f = (self.f & !C_FLAG_MASK) | (low_bit << 4);
        self.f &= !Z_FLAG_MASK;
        self.f &= !H_FLAG_MASK;
        self.f &= !N_FLAG_MASK;

        self.pc += 1;

        4
    }

    //call and returns
    // note: stack starts at 0xfffe, (and goes down in addresses)
    // our SP is set to this address by default
    // each stack entry is a 16 bit (2 byte) memory address
    // 127 bytes of available memory for stack (max call stack of 63)
    //      why its not an even 128, I have no idea
    // if we try to push to the stack more than available, do a panic I guess
    //      def don't let it overwrite the i/o registers lol

    // 0xcd
    fn call(&mut self, _: u8) -> CycleCount {
        let target = ((self.read_mem(self.pc + 2) as u16) << 8) | self.read_mem(self.pc + 1) as u16;
        // offset the return PC by 3 as this is a 3 byte instruction that should
        // not be repeated
        self.helper_call(target, self.pc + 3);
        24
    }

    // 0xc4 0xcc 0xd4 0xdc
    // 0b110cc100
    // 0xc4 nz cc = 00
    fn call_conditional_nz(&mut self, _: u8) -> CycleCount {
        let target = ((self.read_mem(self.pc + 2) as u16) << 8) | self.read_mem(self.pc + 1) as u16;
        //24 cycles if condition true, 12 if not
        if self.f & Z_FLAG_MASK != 0 {
            self.pc += 3;
            return 12;
        }
        self.helper_call(target, self.pc + 3);
        24
    }
    // 0xcc z  cc = 01
    fn call_conditional_z(&mut self, _: u8) -> CycleCount {
        let target = ((self.read_mem(self.pc + 2) as u16) << 8) | self.read_mem(self.pc + 1) as u16;
        //24 cycles if condition true, 12 if not
        if self.f & Z_FLAG_MASK == 0 {
            self.pc += 3;
            return 12;
        }
        self.helper_call(target, self.pc + 3);
        24
    }
    // 0xd4 nc cc = 10
    fn call_conditional_nc(&mut self, _: u8) -> CycleCount {
        let target = ((self.read_mem(self.pc + 2) as u16) << 8) | self.read_mem(self.pc + 1) as u16;
        //24 cycles if condition true, 12 if not
        if self.f & C_FLAG_MASK != 0 {
            self.pc += 3;
            return 12;
        }
        self.helper_call(target, self.pc + 3);
        24
    }
    // 0xdc c  cc = 11
    fn call_conditional_c(&mut self, _: u8) -> CycleCount {
        let target = ((self.read_mem(self.pc + 2) as u16) << 8) | self.read_mem(self.pc + 1) as u16;
        //24 cycles if condition true, 12 if not
        if self.f & C_FLAG_MASK == 0 {
            self.pc += 3;
            return 12;
        }
        self.helper_call(target, self.pc + 3);
        24
    }

    // handles common call functionality
    fn helper_call(&mut self, target: u16, return_pc: usize) {
        log::trace!("({:#06x}) calling function at {:#06x}", self.pc, target);
        let lesser_pc = return_pc as u8;
        let major_pc = (return_pc >> 8) as u8;

        self.write_mem(self.sp.wrapping_sub(1) & 0xffff, major_pc);
        self.write_mem(self.sp.wrapping_sub(2) & 0xffff, lesser_pc);
        self.sp = self.sp.wrapping_sub(2) & 0xffff;

        self.pc = target as usize;
    }

    // 0xc9
    fn ret(&mut self, _: u8) -> CycleCount {
        self.helper_return();
        16
    }

    // 0xc0, 0xd0, 0xc8, 0xd8
    // 0b110cc000
    // 0xc0 NZ flags (subtract & zero)  cc = 00
    fn ret_conditional_nz(&mut self, _: u8) -> CycleCount {
        // 20 cycles if condition true, 8 if not
        if self.f & Z_FLAG_MASK != 0 {
            self.pc += 1;
            return 8;
        }
        self.helper_return();
        20
    }
    // 0xc8 Z flag (zero)               cc = 01
    fn ret_conditional_z(&mut self, _: u8) -> CycleCount {
        // 20 cycles if condition true, 8 if not
        if self.f & Z_FLAG_MASK == 0 {
            self.pc += 1;
            return 8;
        }
        self.helper_return();
        20
    }
    // 0xd0 NC flags (subtract & carry) cc = 10
    fn ret_conditional_nc(&mut self, _: u8) -> CycleCount {
        // 20 cycles if condition true, 8 if not
        if self.f & C_FLAG_MASK != 0 {
            self.pc += 1;
            return 8;
        }
        self.helper_return();
        20
    }
    // 0xd8 C flag  (carry)             cc = 11
    fn ret_conditional_c(&mut self, _: u8) -> CycleCount {
        // 20 cycles if condition true, 8 if not
        if self.f & C_FLAG_MASK == 0 {
            self.pc += 1;
            return 8;
        }
        self.helper_return();
        20
    }

    //return and enable interrupts (IME = 1)
    //0xd9
    fn reti(&mut self, _: u8) -> CycleCount {
        self.helper_return();
        self.interrupt_master_enable = true;
        16
    }

    // handles common return functionality
    fn helper_return(&mut self) {
        let dest = ((self.read_mem(self.sp + 1) as usize) << 8) | self.read_mem(self.sp) as usize;
        log::trace!(
            "({:#06x}) returning from function (dest) {:#06x}",
            self.pc,
            dest
        );
        self.pc = dest;
        self.sp += 2;
    }

    // push & pop
    // 0xc1,d1,e1,f1
    fn pop(&mut self, opcode: u8) -> CycleCount {
        //0b11_xx_0001
        let qq_code = (opcode & 0b00_11_0000) >> 4;
        let stack_high = self.read_mem(self.sp + 1);
        let stack_low = self.read_mem(self.sp);
        //BC 00, DE 01, HL 10, AF 11
        match qq_code {
            0 => (self.b, self.c) = (stack_high, stack_low),
            1 => (self.d, self.e) = (stack_high, stack_low),
            2 => (self.h, self.l) = (stack_high, stack_low),
            3 => (self.a, self.f) = (stack_high, stack_low),
            _ => panic!("invalid qq code"),
        }

        self.sp += 2;
        self.pc += 1;

        12
    }

    // 0xc5,d5,e5,f5
    fn push(&mut self, opcode: u8) -> CycleCount {
        //0b11_xx_0101
        let qq_code = (opcode & 0b00_11_0000) >> 4;
        let (high_value, low_value);
        //BC 00, DE 01, HL 10, AF 11
        match qq_code {
            0 => (high_value, low_value) = (self.b, self.c),
            1 => (high_value, low_value) = (self.d, self.e),
            2 => (high_value, low_value) = (self.h, self.l),
            3 => (high_value, low_value) = (self.a, self.f),
            _ => panic!("invalid qq code"),
        }
        self.write_mem(self.sp - 1, high_value);
        self.write_mem(self.sp - 2, low_value);

        self.sp -= 2;
        self.pc += 1;

        16
    }

    // interrupt enable / disable

    //0xf3
    fn disable_interrupt(&mut self, _: u8) -> CycleCount {
        log::trace!("({:#06x}) disabling IME", self.pc);
        self.interrupt_master_enable = false;
        self.ei_delay = 0;
        self.pc += 1;
        4
    }

    //0xfb
    fn enable_interrupt(&mut self, _: u8) -> CycleCount {
        log::trace!("({:#06x}) enabling IME", self.pc);
        self.ei_delay = 2;
        self.pc += 1;
        4
    }

    //0xcb 0x00-07
    fn cb_rlc(&mut self, opcode: u8) -> CycleCount {
        //Rotates a to the left with bit 7 being moved to bit 0 and also stored into the carry
        //0b0000_0rrr
        //flags Z 0 0 C
        let reg = opcode & 0b0000_0111;
        let val = self.read_reg(reg);

        //store highest bit into carry
        self.f = (self.f & !C_FLAG_MASK) | ((val & 0x80) >> 3);

        // update zero flag
        if val == 0 {
            self.f |= Z_FLAG_MASK;
        } else {
            self.f &= !Z_FLAG_MASK;
        }

        self.f &= !H_FLAG_MASK;
        self.f &= !N_FLAG_MASK;

        self.write_reg(reg, val.rotate_left(1));

        self.pc += 2;
        if reg == 0x06 {
            16
        } else {
            8
        }
    }

    //0xcb 0x08-0f
    fn cb_rrc(&mut self, opcode: u8) -> CycleCount {
        //Rotates a to the right with bit 0 moved to bit 7 and also stored into the carry.
        //0b0000_1rrr
        //flags Z 0 0 C
        let reg = opcode & 0b0000_0111;
        let val = self.read_reg(reg);

        //store highest bit into carry
        self.f = (self.f & !C_FLAG_MASK) | ((val & 0x01) << 4);

        // update zero flag
        if val == 0 {
            self.f |= Z_FLAG_MASK;
        } else {
            self.f &= !Z_FLAG_MASK;
        }

        self.f &= !H_FLAG_MASK;
        self.f &= !N_FLAG_MASK;

        self.write_reg(reg, val.rotate_right(1));

        self.pc += 2;
        if reg == 0x06 {
            16
        } else {
            8
        }
    }

    //0xcb 0x10-17
    fn cb_rl(&mut self, opcode: u8) -> CycleCount {
        // Rotates a register to the left with the carry's value put into bit 0 and bit 7 is put into the carry.
        //0b0001_0rrr
        //flags Z 0 0 C
        let reg = opcode & 0b0000_0111;
        let mut val = self.read_reg(reg);
        let high_bit = val >> 7;
        let carry_bit = (self.f & C_FLAG_MASK) >> 4;

        //the current carry bit is placed as the lowest bit in a
        val <<= 1;
        val |= carry_bit;

        //store highest bit into carry
        self.f = (self.f & !C_FLAG_MASK) | (high_bit << 4);
        if val == 0 {
            self.f |= Z_FLAG_MASK;
        } else {
            self.f &= !Z_FLAG_MASK;
        }
        self.f &= !H_FLAG_MASK;
        self.f &= !N_FLAG_MASK;

        self.write_reg(reg, val);

        self.pc += 2;
        if reg == 0x06 {
            16
        } else {
            8
        }
    }

    //0xcb 0x18-1f
    fn cb_rr(&mut self, opcode: u8) -> CycleCount {
        //Rotates a to the right with the carry put into bit 7 and bit 0 put into the carry flag.
        //0b0001_1rrr
        //flags Z 0 0 C
        let reg = opcode & 0b0000_0111;
        let mut val = self.read_reg(reg);
        let low_bit = val & 0x01;
        let carry_bit = (self.f & C_FLAG_MASK) >> 4;

        //the current carry bit is placed as the highest bit in a
        val >>= 1;
        val |= carry_bit << 7;

        //store highest bit into carry
        self.f = (self.f & !C_FLAG_MASK) | (low_bit << 4);
        if val == 0 {
            self.f |= Z_FLAG_MASK;
        } else {
            self.f &= !Z_FLAG_MASK;
        }
        self.f &= !H_FLAG_MASK;
        self.f &= !N_FLAG_MASK;

        self.write_reg(reg, val);

        self.pc += 2;
        if reg == 0x06 {
            16
        } else {
            8
        }
    }

    //0xcb 0x20-27
    fn cb_sla(&mut self, opcode: u8) -> CycleCount {
        //0b0010_0rrr
        //flags Z 0 0 C
        let reg = opcode & 0b0000_0111;
        let mut val = self.read_reg(reg);

        //store high bit in carry flag
        self.f = (self.f & !C_FLAG_MASK) | ((val & 0x80) >> 3);
        val <<= 1;
        self.write_reg(reg, val);

        if val == 0 {
            self.f |= Z_FLAG_MASK;
        } else {
            self.f &= !Z_FLAG_MASK;
        }

        self.f &= !H_FLAG_MASK;
        self.f &= !N_FLAG_MASK;

        self.pc += 2;
        if reg == 0x06 {
            16
        } else {
            8
        }
    }

    //0xcb 0x28-2f
    fn cb_sra(&mut self, opcode: u8) -> CycleCount {
        //0b0010_1rrr
        //flags Z 0 0 C
        let reg = opcode & 0b0000_0111;
        let mut val = self.read_reg(reg);
        let low_bit = 1 & val;
        let high_bit = 0b1000_0000 & val;

        //store low bit in carry flag
        self.f = (self.f & !C_FLAG_MASK) | (low_bit << 4);

        //shift
        val >>= 1;

        //restore high bit (arithmetic preservation)
        val |= high_bit;

        self.write_reg(reg, val);

        if val == 0 {
            self.f |= Z_FLAG_MASK;
        } else {
            self.f &= !Z_FLAG_MASK;
        }

        self.f &= !H_FLAG_MASK;
        self.f &= !N_FLAG_MASK;

        self.pc += 2;
        if reg == 0x06 {
            16
        } else {
            8
        }
    }

    //0xcb 0x38-3f
    fn cb_srl(&mut self, opcode: u8) -> CycleCount {
        //0b0011_1rrr
        //flags Z 0 0 C
        let reg = opcode & 0b0000_0111;
        let mut val = self.read_reg(reg);
        let low_bit = 1 & val;

        //store low bit in carry flag
        self.f = (self.f & !C_FLAG_MASK) | (low_bit << 4);

        //shift (no arithmetic preservation)
        val >>= 1;

        self.write_reg(reg, val);

        if val == 0 {
            self.f |= Z_FLAG_MASK;
        } else {
            self.f &= !Z_FLAG_MASK;
        }

        self.f &= !H_FLAG_MASK;
        self.f &= !N_FLAG_MASK;

        self.pc += 2;
        if reg == 0x06 {
            16
        } else {
            8
        }
    }

    //0xcb 0x30-37
    fn cb_swap(&mut self, opcode: u8) -> CycleCount {
        //0b0011_0rrr
        //flags Z 0 0 C
        let reg = opcode & 0b0000_0111;
        let mut val = self.read_reg(reg);

        //swap
        let lower_bits = val & 0x0f;
        val >>= 4;
        val |= lower_bits << 4;

        self.write_reg(reg, val);

        if val == 0 {
            self.f |= Z_FLAG_MASK;
        } else {
            self.f &= !Z_FLAG_MASK;
        }

        self.f &= !H_FLAG_MASK;
        self.f &= !N_FLAG_MASK;
        self.f &= !C_FLAG_MASK;

        self.pc += 2;
        if reg == 0x06 {
            16
        } else {
            8
        }
    }

    //0xcb 0x40-7f
    fn cb_bit(&mut self, opcode: u8) -> CycleCount {
        //0b01_bbb_rrr
        //flags Z 0 1 -
        let reg = opcode & 0b0000_0111;
        let bit = (opcode & 0b00_111_000) >> 3;
        let val = self.read_reg(reg);

        let bit_mask = 1 << bit;
        let bit_val = (val & bit_mask) >> bit;

        // set Z flag to opposite of bit value
        if bit_val == 0 {
            self.f |= Z_FLAG_MASK;
        } else {
            self.f &= !Z_FLAG_MASK;
        }

        self.f |= H_FLAG_MASK;
        self.f &= !N_FLAG_MASK;

        self.pc += 2;
        if reg == 0x06 {
            12
        } else {
            8
        }
    }

    //0xcb 0x80-bf
    fn cb_res(&mut self, opcode: u8) -> CycleCount {
        //0b10_bbb_rrr
        //flags - - - -
        let reg = opcode & 0b0000_0111;
        let bit = (opcode & 0b00_111_000) >> 3;
        let mut val = self.read_reg(reg);

        let bit_mask = 1 << bit;
        val &= !bit_mask;
        self.write_reg(reg, val);

        self.pc += 2;
        if reg == 0x06 {
            16
        } else {
            8
        }
    }

    //0xcb 0xc0-ff
    fn cb_set(&mut self, opcode: u8) -> CycleCount {
        //0b11_bbb_rrr
        //flags - - - -
        let reg = opcode & 0b0000_0111;
        let bit = (opcode & 0b00_111_000) >> 3;
        let mut val = self.read_reg(reg);

        let bit_mask = 1 << bit;
        val |= bit_mask;
        self.write_reg(reg, val);

        self.pc += 2;
        if reg == 0x06 {
            16
        } else {
            8
        }
    }

    //0x76
    fn halt(&mut self, _: u8) -> CycleCount {
        self.is_halted = true;

        // halt bug cases
        // https://gbdev.io/pandocs/halt.html#halt-bug
        let interrupt_pending =
            (self.mem[INTERRUPT_ENABLE_ADDRESS] & self.mem[INTERRUPT_FLAG_ADDRESS]) != 0;
        let case0 = !self.interrupt_master_enable && interrupt_pending;
        const IE_OPCODE: u8 = 0xfb;
        let case1 = self.last_opcode == IE_OPCODE && interrupt_pending;

        if !case0 && !case1 {
            self.pc += 1;
        }

        if case1 {
            self.interrupt_master_enable = true;
            self.ei_delay = 0;
        }

        4
    }

    //0x10 0x00
    fn stop(&mut self, _: u8) -> CycleCount {
        //the full instruction is 0x10_00, we need to read the next opcode for this
        if self.mem[self.pc + 1] == 0x00 {
            self.is_stopped = true;

            // timer obscura https://gbdev.io/pandocs/Timer_and_Divider_Registers.html#ff04--div-divider-register
            self.write_mem(DIV_ADDRESS, 0);
        }
        //if the full instruction isn't present, we do nothing and move on
        4
    }
}
