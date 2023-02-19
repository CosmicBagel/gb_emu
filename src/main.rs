use cpu::Cpu;

mod cpu;

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

    let filename = "01-special.gb";
    let mut cpu = Cpu::new();
    cpu.load_rom(filename);
    cpu.run();
}
