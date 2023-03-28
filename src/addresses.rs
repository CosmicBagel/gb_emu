pub const ROM_BANK_00_START_ADDRESS: usize = 0x0000;
pub const ROM_BANK_00_END_ADDRESS: usize = 0x3fff;
pub const ROM_BANK_NN_START_ADDRESS: usize = 0x4000;
pub const ROM_BANK_NN_END_ADDRESS: usize = 0x7fff;

pub const INTERRUPT_ENABLE_ADDRESS: usize = 0xffff; // program allows interrupts
                                                    /*
                                                    Bit 0: VBlank   Interrupt Enable  (INT $40)  (1=Enable)
                                                    Bit 1: LCD STAT Interrupt Enable  (INT $48)  (1=Enable)
                                                    Bit 2: Timer    Interrupt Enable  (INT $50)  (1=Enable)
                                                    Bit 3: Serial   Interrupt Enable  (INT $58)  (1=Enable)
                                                    Bit 4: Joypad   Interrupt Enable  (INT $60)  (1=Enable)
                                                    */
pub const INTERRUPT_FLAG_ADDRESS: usize = 0xff0f; // program explicitly requests interrupt
                                                  /*
                                                  Bit 0: VBlank   Interrupt Request (INT $40)  (1=Request)
                                                  Bit 1: LCD STAT Interrupt Request (INT $48)  (1=Request)
                                                  Bit 2: Timer    Interrupt Request (INT $50)  (1=Request)
                                                  Bit 3: Serial   Interrupt Request (INT $58)  (1=Request)
                                                  Bit 4: Joypad   Interrupt Request (INT $60)  (1=Request)
                                                   */

pub const DIV_ADDRESS: usize = 0xff04; //divider register
pub const TIMA_ADDRESS: usize = 0xff05; //timer counter
pub const TMA_ADDRESS: usize = 0xff06; //timer modulo
pub const TAC_ADDRESS: usize = 0xff07; //timer control
pub const OAM_DMA_ADDRESS: usize = 0xff46; //trigger OAM copy

pub const FORBIDDEN_ADDRESS_START: usize = 0xfea0;
pub const FORBIDDEN_ADDRESS_END: usize = 0xfeff;

pub const LY_ADDRESS: usize = 0xff44; //horizontal line register (0-153)
pub const LYC_ADDRESS: usize = 0xff45; //LY Compare (interrupt is triggered when matches LY) (0-153)
pub const STAT_ADDRESS: usize = 0xff41; //LCD Stat
                                        /*
                                        LCD STAT register (0xff41)
                                        Bit 6 - LYC=LY STAT Interrupt source         (1=Enable) (Read/Write)
                                        Bit 5 - Mode 2 OAM STAT Interrupt source     (1=Enable) (Read/Write)
                                        Bit 4 - Mode 1 VBlank STAT Interrupt source  (1=Enable) (Read/Write)
                                        Bit 3 - Mode 0 HBlank STAT Interrupt source  (1=Enable) (Read/Write)
                                        Bit 2 - LYC=LY Flag                          (0=Different, 1=Equal) (Read Only)
                                        Bit 1-0 - Mode Flag                          (Mode 0-3, see below) (Read Only)
                                                  0: HBlank
                                                  1: VBlank
                                                  2: Searching OAM
                                                  3: Transferring Data to LCD Controller
                                        */

/// LCD Control
/// - 7 LCD and PPU enable             0=Off, 1=On
/// - 6 Window tile map area           0=9800-9BFF, 1=9C00-9FFF
/// - 5 Window enable                  0=Off, 1=On
/// - 4 BG and Window tile data area   0=8800-97FF, 1=8000-8FFF
/// - 3 BG tile map area               0=9800-9BFF, 1=9C00-9FFF
/// - 2 OBJ size                       0=8x8, 1=8x16
/// - 1 OBJ enable                     0=Off, 1=On
/// - 0 BG and Window enable/priority  0=Off, 1=On
pub const LCDC_ADDRESS: usize = 0xff40; 

pub const OAM_TABLE_ADDRESS: usize = 0xfe00; //Sprite table, max 40 sprites of 4 bytes each
                                             /*
                                             byte 0 - y position
                                             byte 1 - x position
                                             byte 2 - tile index
                                             byte 3 - attributes/flags
                                                 Bit7   BG and Window over OBJ (0=No, 1=BG and Window colors 1-3 over the OBJ)
                                                 Bit6   Y flip          (0=Normal, 1=Vertically mirrored)
                                                 Bit5   X flip          (0=Normal, 1=Horizontally mirrored)
                                                 Bit4   Palette number  **Non CGB Mode Only** (0=OBP0, 1=OBP1)
                                                 Bit3   Tile VRAM-Bank  **CGB Mode Only**     (0=Bank 0, 1=Bank 1)
                                                 Bit2-0 Palette number  **CGB Mode Only**     (OBP0-7)
                                             */
pub const OAM_TABLE_END_ADDRESS: usize = 0xfe9f;

pub const SCX_ADDRESS: usize = 0xff43;
pub const SCY_ADDRESS: usize = 0xff42;
pub const BGP_ADDRESS: usize = 0xff47;
// pub const OBP0_ADDRESS: usize = 0xff48;
// pub const OBP1_ADDRESS: usize = 0xff49;

///each tile block holds 128 tiles (16 bytes each)
// pub const TILE_DATA_BLOCK_SIZE: usize = 128 * 16;
pub const TILE_DATA_BLOCK_0_ADDRESS: usize = 0x8000;
pub const TILE_DATA_BLOCK_1_ADDRESS: usize = 0x8800;
pub const TILE_DATA_BLOCK_2_ADDRESS: usize = 0x9000;

///each tile map is 32x32 bytes (1024 bytes)
// pub const TILE_MAP_SIZE: usize = 32 * 32;
pub const TILE_MAP_0_ADDRESS: usize = 0x9800;
pub const TILE_MAP_1_ADDRESS: usize = 0x9c00;

// high ram is 126 bytes total
pub const HIGH_RAM_START_ADDRESS: usize = 0xff80;
pub const HIGH_RAM_END_ADDRESS: usize = 0xfffe;

pub const CARTRIDGE_TYPE_ROM_ADDRESS: usize = 0x0147;

pub const VRAM_ADDRESS: usize = 0x8000;
pub const VRAM_END_ADDRESS: usize = 0x9fff;

pub const SB_SERIAL_OUT_ADDRESS: usize = 0xff01;
pub const SC_SERIAL_CONTROL_ADDRESS: usize = 0xff02;

pub const JOYPAD_ADDRESS: usize = 0xff00;

/// NR52 Sound on/off  
/// * Bit 7 - All sound on/off  (0: turn the APU off) (Read/Write)  
///     * When APU is off, all audio registers are cleared, and READ ONLY
/// * Bit 3 - Channel 4 ON flag (Read Only)  
/// * Bit 2 - Channel 3 ON flag (Read Only)  
/// * Bit 1 - Channel 2 ON flag (Read Only)  
/// * Bit 0 - Channel 1 ON flag (Read Only)  
pub const NR52_SOUND_ON_OFF_ADDRESS: usize = 0xff26;

/// NR52 Sound Panning
/// * Bit 7 - Mix channel 4 into left output
/// * Bit 6 - Mix channel 3 into left output
/// * Bit 5 - Mix channel 2 into left output
/// * Bit 4 - Mix channel 1 into left output
/// * Bit 3 - Mix channel 4 into right output
/// * Bit 2 - Mix channel 3 into right output
/// * Bit 1 - Mix channel 2 into right output
/// * Bit 0 - Mix channel 1 into right output
pub const NR51_SOUND_PANNING_ADDRESS: usize = 0xff25;

/// NR50 Master volume & VIN panning
/// * Bit 7   - Mix VIN into left output  (1=Enable)
/// * Bit 6-4 - Left output volume        (0-7)
/// * Bit 3   - Mix VIN into right output (1=Enable)
/// * Bit 2-0 - Right output volume       (0-7)
pub const NR50_VOLUME_VIN_PANNING_ADDRESS: usize = 0xff24;

/// NR10 Channel 1 Sweep
/// * Bit 6-4 - Sweep pace
/// * Bit 3   - Sweep increase/decrease
///     * 0: Addition    (wavelength increases)
///     * 1: Subtraction (wavelength decreases)
/// * Bit 2-0 - Sweep slope control (n: 0-7)
pub const NR10_CHANNEL1_SWEEP_ADDRESS: usize = 0xff10;

/// NR11 Channel 1 length timer & duty cycle
/// * Bit 7-6 - Wave duty (Read/Write)
///     * 0b00: 12.5 %
///     * 0b01: 25.0 %
///     * 0b10: 50.0 %
///     * 0b11: 75.0 %
/// * Bit 5-0 - Initial length timer (Write Only)
pub const NR11_CHANNEL1_LENGTH_DUTY_ADDRESS: usize = 0xff11;

/// NR12 Channel 1 volume & envelope
/// * Bit 7-4 - Initial volume of envelope (0-F) (0=No Sound)
/// * Bit 3   - Envelope direction (0=Decrease, 1=Increase)
/// * Bit 2-0 - Sweep pace (0=No Sweep)
pub const NR12_CHANNEL1_VOLUME_ENVELOPE_ADDRESS: usize = 0xff12;

/// NR13 Channel 1 wavelength low (write-only)
/// * Stores the low 8bits of the 11-bit wavelength
pub const NR13_CHANNEL1_WAVELENGTH_LOW_ADDRESS: usize = 0xff13;

///NR14 Channel 1 wavelength high & control
/// * Bit 7   - Trigger (1=Restart channel)  (Write Only)
/// * Bit 6   - Sound Length enable          (Read/Write)
///     * (1=Stop output when length in NR11 expires)
/// * Bit 2-0 - "Wavelength"'s higher 3 bits (Write Only)
pub const NR14_CHANNEL1_WAVELENGTH_HIGH_AND_CONTROL_ADDRESS: usize = 0xff14;

/// NR21 Channel 2 length timer & duty cycle
/// * Bit 7-6 - Wave duty (Read/Write)
///     * 0b00: 12.5 %
///     * 0b01: 25.0 %
///     * 0b10: 50.0 %
///     * 0b11: 75.0 %
/// * Bit 5-0 - Initial length timer (Write Only)
pub const NR21_CHANNEL2_LENGTH_DUTY_ADDRESS: usize = 0xff16;

/// NR22 Channel 2 volume & envelope
/// * Bit 7-4 - Initial volume of envelope (0-F) (0=No Sound)
/// * Bit 3   - Envelope direction (0=Decrease, 1=Increase)
/// * Bit 2-0 - Sweep pace (0=No Sweep)
pub const NR22_CHANNEL2_VOLUME_ENVELOPE_ADDRESS: usize = 0xff17;

/// NR23 Channel 2 wavelength low (write-only)
/// * Stores the low 8bits of the 11-bit wavelength
pub const NR23_CHANNEL2_WAVELENGTH_LOW_ADDRESS: usize = 0xff18;

/// NR24 Channel 2 wavelength high & control
/// * Bit 7   - Trigger (1=Restart channel)  (Write Only)
/// * Bit 6   - Sound Length enable          (Read/Write)
///     * (1=Stop output when length in NR21 expires)
/// * Bit 2-0 - "Wavelength"'s higher 3 bits (Write Only)
pub const NR24_CHANNEL2_WAVELENGTH_HIGH_AND_CONTROL_ADDRESS: usize = 0xff19;

/// NR30 Channel 3 DAC Enable
/// * Bit 7 - Sound Channel 3 DAC  (0=Off, 1=On)
pub const NR30_CHANNEL3_DAC_ENABLE_ADDRESS: usize = 0xff1a;

/// NR31 Channel 3 length time (write only)
/// *  Bit 7-0 - length timer
pub const NR31_CHANNEL3_LENGTH_TIMER_ADDRESS: usize = 0xff1b;

/// NR32 Channel 3 output level
/// * Bits 6-5 - Output level selection
///     * 0b00 - Mute
///     * 0b01 - 100% volume (use samples read from Wave RAM as-is)
///     * 0b10 - 50% volume (shift samples read from Wave RAM right once)
///     * 0b11 - 25% volume (shift samples read from Wave RAM right twice)
pub const NR32_CHANNEL3_OUTPUT_LEVEL_ADDRESS: usize = 0xff1c;

/// NR33 Channel 3 wavelength low (write-only)
/// * Stores the low 8bits of the 11-bit wavelength
pub const NR33_CHANNEL3_WAVELENGTH_LOW_ADDRESS: usize = 0xff1d;

/// NR34 Channel 3 wavelength high & control
/// * Bit 7   - Trigger (1=Restart channel)  (Write Only)
/// * Bit 6   - Sound Length enable          (Read/Write)
///     * (1=Stop output when length in NR31 expires)
/// * Bit 2-0 - "Wavelength"'s higher 3 bits (Write Only)
pub const NR34_CHANNEL3_WAVELENGTH_HIGH_AND_CONTROL_ADDRESS: usize = 0xff1e;

/// NR41 Channel 4 length time (write only)
/// *  Bit 5-0 - length timer
pub const NR41_CHANNEL4_LENGTH_TIMER_ADDRESS: usize = 0xff20;

/// NR42 Channel 4 volume & envelope
/// * Bit 7-4 - Initial volume of envelope (0-F) (0=No Sound)
/// * Bit 3   - Envelope direction (0=Decrease, 1=Increase)
/// * Bit 2-0 - Sweep pace (0=No Sweep)
pub const NR42_CHANNEL_4_VOLUME_ENVELOPE_ADDRESS: usize = 0xff21;

/// NR43 Channel 4 frequency and randomness
/// * Bit 7-4 - Clock shift (s)
/// * Bit 3   - LFSR width (0=15 bits, 1=7 bits)
///     * LFSR = linear-feedback shift register?
/// * Bit 2-0 - Clock divider (r)
pub const NR43_CHANNEL_4_FREQUENCY_AND_RANDOMNESS_ADDRESS: usize = 0xff22;

/// NR44 Channel 4 control
/// * Bit 7   - Trigger (1=Restart channel)  (Write Only)
/// * Bit 6   - Sound Length enable          (Read/Write)
/// * (1=Stop output when length in NR41 expires)
pub const NR44_CHANNEL_4_CONTROL_ADDRESS: usize = 0xff23;

/// Wave pattern ram
/// * 16 bytes long, 'samples' 4 bits each
pub const WAVE_PATTERN_RAM_START_ADDRESS: usize = 0xff30;

/// Wave pattern ram
/// * 16 bytes long, 'samples' 4 bits each
pub const WAVE_PATTERN_RAM_END_ADDRESS: usize = 0xff3f;
