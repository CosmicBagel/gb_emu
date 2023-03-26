//ppu = picture processing unit
use crate::cpu::Cpu;
use crate::cpu::InterruptFlags;

use crate::addresses::*;
use crate::constants::*;

/*
LCD controller runs at same speed as cpu 2^22hz (4.194_304 MHz)
Each scanline is 456 cycles (HBlank 9.198KHz)
Full frame is 154 scanlines so VBlank 59.727_5Hz (70_224 cycles)
There's an interrupt every HBlank and VBlank (HBlank occurs when VBlank occurs,
    VBlank is higher priority)
only 144 lines on screen, the rest of the scanlines are invisible

gb uses tile and sprites
each tile 8x8 pixels
screen res is 160x144
'canvas' of render space is 256x256 (32x32 tiles)
    screen pans across this canvas

background (tiles) - don't move
window (in between, moves with screen) eg can display health and stats
sprites (foreground moving objects)


The following sequence is typical when the display is enabled:
Mode 2  2_____2_____2_____2_____2_____2___________________2____
Mode 3  _33____33____33____33____33____33__________________3___
Mode 0  ___000___000___000___000___000___000________________000
Mode 1  ____________________________________11111111111111_____
 */

#[derive(PartialEq)]
pub enum PpuStepResult {
    NoAction,
    Draw,
}

pub struct Ppu {
    pixels: [PixelShade; GB_WIDTH * GB_HEIGHT],
    current_mode_counter: u32,
    last_mode3_duration: u32,
    mode_func: PpuModeFunc,
    current_line_sprites: Vec<OAM>,
    lcdc_last_enabled: bool,
    current_mode_oam_dma: bool,

    //debug
    debug_mode0_cycles: u32,
    debug_mode1_cycles: u32,
    debug_mode2_cycles: u32,
    debug_mode3_cycles: u32,
}

struct OAM {
    y_position: u8,
    x_position: u8,
    tile_index: u8,
    attributes: u8,
    /* attribute bits
    Bit7   BG and Window over OBJ (0=No, 1=BG and Window colors 1-3 over the OBJ)
    Bit6   Y flip          (0=Normal, 1=Vertically mirrored)
    Bit5   X flip          (0=Normal, 1=Horizontally mirrored)
    Bit4   Palette number  **Non CGB Mode Only** (0=OBP0, 1=OBP1)
    Bit3   Tile VRAM-Bank  **CGB Mode Only**     (0=Bank 0, 1=Bank 1)
    Bit2-0 Palette number  **CGB Mode Only**     (OBP0-7) */
}

#[derive(Clone, Copy)]
pub enum PixelShade {
    White = 0b00,
    Light = 0b01,
    Medium = 0b10,
    Dark = 0b11,
    Disabled = 0xff,
}

impl From<u8> for PixelShade {
    fn from(value: u8) -> Self {
        match value {
            0b00 => PixelShade::White,
            0b01 => PixelShade::Light,
            0b10 => PixelShade::Medium,
            0b11 => PixelShade::Dark,
            0xff => PixelShade::Disabled,
            _ => PixelShade::White,
        }
    }
}

#[derive(PartialEq)]
enum LcdStatModeFlag {
    HBlank = 0b00,
    VBlank = 0b01,
    OAMSearch = 0b10,
    ///aka image gen
    DataToLCD = 0b11,
}

enum StatInterrupt {
    LycEqLy = 0b0100_0000,
    OAMSearch = 0b0010_0000,
    VBlank = 0b0001_0000,
    HBlank = 0b0000_1000,
}

impl From<u8> for LcdStatModeFlag {
    fn from(value: u8) -> Self {
        match value {
            v if v == LcdStatModeFlag::HBlank as u8 => LcdStatModeFlag::HBlank,
            v if v == LcdStatModeFlag::VBlank as u8 => LcdStatModeFlag::VBlank,
            v if v == LcdStatModeFlag::OAMSearch as u8 => LcdStatModeFlag::OAMSearch,
            v if v == LcdStatModeFlag::DataToLCD as u8 => LcdStatModeFlag::DataToLCD,
            _ => LcdStatModeFlag::HBlank,
        }
    }
}

type PpuModeFunc = fn(&mut Ppu, &mut Cpu, cycles: u32) -> (u32, PpuStepResult);

impl Ppu {
    pub fn new() -> Ppu {
        Ppu {
            pixels: [PixelShade::Disabled; GB_WIDTH * GB_HEIGHT],
            current_mode_counter: 0,
            last_mode3_duration: 0,
            mode_func: Ppu::handle_mode0_hblank,
            current_line_sprites: vec![],
            lcdc_last_enabled: false,
            current_mode_oam_dma: false,

            debug_mode0_cycles: 0,
            debug_mode1_cycles: 0,
            debug_mode2_cycles: 0,
            debug_mode3_cycles: 0,
        }
    }

    pub fn do_step(&mut self, cpu: &mut Cpu, cycle_count: u32) -> PpuStepResult {
        //update cycle count
        // switch mode as appropriate if enough cycles reached
        // ensure excess cycles are added to next mode's cycle duration
        // note 'dots' are 2^22 hz (4.19 MHz), the same as the gameboy's normal speed clock
        // on CGB this does not change even if the CPU is running at double speed, hence dots
        // for consistency when discussing rendering

        let mut cycles = cycle_count;
        let mut result = PpuStepResult::NoAction;

        while cycles > 0 {
            let (c, r) = (self.mode_func)(self, cpu, cycles);

            cycles = c;
            //ensure that a draw result is never overwritten
            if result != PpuStepResult::Draw {
                result = r;
            }
        }

        Ppu::stat_update_ly_eq_lyc_bit(cpu);

        //tracking the state of lcd enabled for vblank
        self.lcdc_last_enabled = self.lcdc_get_lcd_ppu_enabled(cpu);

        if !self.current_mode_oam_dma && cpu.is_oam_dma_active {
            self.current_mode_oam_dma = true;
        }

        result
    }

    fn start_mode0_hblank(&mut self, cpu: &mut Cpu, cycles: u32) -> (u32, PpuStepResult) {
        // this 'start' state consumes no dots/cycles, ie we just pass the cycles value
        self.mode_func = Ppu::handle_mode0_hblank;
        Ppu::stat_set_mode_flag(cpu, LcdStatModeFlag::HBlank);
        Ppu::stat_flag_interrupt(cpu, StatInterrupt::HBlank);
        self.current_mode_counter = 0;
        self.current_mode_oam_dma = false;
        (cycles, PpuStepResult::NoAction)
    }

    fn handle_mode0_hblank(&mut self, cpu: &mut Cpu, cycles: u32) -> (u32, PpuStepResult) {
        // Mode 0
        // Action: Nothing (HBlank)
        // Duration: 85 to 208 dots, depending on previous mode 3 duration
        // Accessible v-mem: VRAM, OAM, CGB palettes

        //always tries to make the scanline last for 456 dots
        //essentially 376 - duration of mode3
        let target = 376 - self.last_mode3_duration;
        let mut remainder = 0;

        self.current_mode_counter += cycles;
        if self.current_mode_counter >= target {
            remainder = self.current_mode_counter - target;
            self.debug_mode0_cycles += cycles - remainder;

            let ly_next = cpu.read_hw_reg(LY_ADDRESS) + 1;
            cpu.write_hw_reg(LY_ADDRESS, ly_next);

            if ly_next >= GB_HEIGHT as u8 {
                //end of 'visible' scanlines, we switch to vblank for 10 invisible scanlines
                self.mode_func = Ppu::start_mode1_vblank;
            } else {
                //next scanline is visible
                self.mode_func = Ppu::start_mode2_object_search;
            }
        } else {
            self.debug_mode0_cycles += cycles;
        }

        (remainder, PpuStepResult::NoAction)
    }

    fn start_mode1_vblank(&mut self, cpu: &mut Cpu, cycles: u32) -> (u32, PpuStepResult) {
        // this 'start' state consumes no dots/cycles, ie we just pass the cycles value
        self.mode_func = Ppu::handle_mode1_vblank;
        // set flag interrupt
        // update LCD state mode
        Ppu::stat_set_mode_flag(cpu, LcdStatModeFlag::VBlank);
        // stat interrupt
        Ppu::stat_flag_interrupt(cpu, StatInterrupt::VBlank);
        // normal interrupt
        let int_flag = cpu.read_hw_reg(INTERRUPT_FLAG_ADDRESS);
        cpu.write_hw_reg(
            INTERRUPT_FLAG_ADDRESS,
            int_flag | InterruptFlags::VBlank as u8,
        );

        self.current_mode_counter = 0;
        self.current_mode_oam_dma = false;

        let lcd_enabled = self.lcdc_get_lcd_ppu_enabled(cpu);
        if !lcd_enabled {
            self.pixels = [PixelShade::Disabled; GB_WIDTH * GB_HEIGHT];
        }
        (cycles, PpuStepResult::Draw)
    }

    fn handle_mode1_vblank(&mut self, cpu: &mut Cpu, cycles: u32) -> (u32, PpuStepResult) {
        // Mode 1
        // Action: Nothing (VBlank)
        // Duration: 4560 dots (10 scanlines), 456 dots per "invisible scanline"
        // Accessible v-mem: VRAM, OAM, CGB palettes

        let lcd_enabled = self.lcdc_get_lcd_ppu_enabled(cpu);

        //when lcd is toggled back on, go to mode 2, reset current scanline to 0
        if !self.lcdc_last_enabled && lcd_enabled {
            cpu.write_hw_reg(LY_ADDRESS, 0);
            self.pixels = [PixelShade::White; GB_WIDTH * GB_HEIGHT];
            self.mode_func = Ppu::start_mode2_object_search;
            return (0, PpuStepResult::Draw);
        }

        // when lcd is disabled, clear the screen
        if !lcd_enabled && self.lcdc_last_enabled {
            self.pixels = [PixelShade::Disabled; GB_WIDTH * GB_HEIGHT];
            return (0, PpuStepResult::Draw);
        }

        // while lcd is disabled, do not leave vblank mode
        if !lcd_enabled {
            return (0, PpuStepResult::NoAction);
        }

        let target = 456;
        let mut left_over_cycles = 0;
        // if count > target and we're on LY = 153, LY = 0 and leave vblank
        // if count > target and LV <= 153, increment LY
        if self.current_mode_counter + cycles >= target {
            left_over_cycles = self.current_mode_counter + cycles - target;
            self.debug_mode1_cycles += cycles - left_over_cycles;

            let ly_next = cpu.read_hw_reg(LY_ADDRESS) + 1;
            if ly_next < (GB_HEIGHT + INVISIBLE_VBLANK_LINES) as u8 {
                cpu.write_hw_reg(LY_ADDRESS, ly_next);
                self.current_mode_counter = 0;
            } else {
                //start over at top of the screen
                cpu.write_hw_reg(LY_ADDRESS, 0);
                self.mode_func = Ppu::start_mode2_object_search;

                // println!(
                //     "0: {} (~{} per line)\n1: {}\n2: {} (~{} per line)\n3: {} (~{} per line)\nframe: {}",
                //     self.mode0_cycles,
                //     self.mode0_cycles / GB_HEIGHT as u32,
                //     self.mode1_cycles,
                //     self.mode2_cycles,
                //     self.mode2_cycles / GB_HEIGHT as u32,
                //     self.mode3_cycles,
                //     self.mode3_cycles / GB_HEIGHT as u32,
                //     self.mode0_cycles + self.mode1_cycles + self.mode2_cycles + self.mode3_cycles
                // );
                self.debug_mode0_cycles = 0;
                self.debug_mode1_cycles = 0;
                self.debug_mode2_cycles = 0;
                self.debug_mode3_cycles = 0;
            }
        } else {
            self.current_mode_counter += cycles;
            self.debug_mode1_cycles += cycles;
        }

        (left_over_cycles, PpuStepResult::NoAction)
    }

    fn start_mode2_object_search(&mut self, cpu: &mut Cpu, cycles: u32) -> (u32, PpuStepResult) {
        // this 'start' state consumes no dots/cycles, ie we just pass the cycles value
        self.mode_func = Ppu::handle_mode2_object_search;
        // set flag interrupt
        // update LCD state mode
        Ppu::stat_set_mode_flag(cpu, LcdStatModeFlag::OAMSearch);
        Ppu::stat_flag_interrupt(cpu, StatInterrupt::OAMSearch);
        self.current_mode_counter = 0;
        self.current_mode_oam_dma = false;

        (cycles, PpuStepResult::NoAction)
    }

    fn handle_mode2_object_search(&mut self, cpu: &mut Cpu, cycles: u32) -> (u32, PpuStepResult) {
        // Mode 2
        // Action: Searching OAM for OBJs whose Y coordinate overlap this line
        // Duration: 80 dots
        // Accessible v-mem: VRAM, CGB palettes
        let target = 80;
        let mut remainder = 0;

        self.current_mode_counter += cycles;
        if self.current_mode_counter >= target {
            remainder = self.current_mode_counter - target;
            self.debug_mode2_cycles += cycles - remainder;
            self.mode_func = Ppu::start_mode3_picture_gen;

            self.current_line_sprites.clear();
            // if oam dma ran at all during this mode, skip finding any sprites
            if !self.current_mode_oam_dma {
                //mode 2's actual job: see what sprites are on this LY
                //10 sprites max, sorted in order of greatest x to least x
                //  so that the sprite with least x value will draw above sprites with greater x values
                const BIG_SPRITE_SIZE: u8 = 16;
                const NORMAL_SPRITE_SIZE: u8 = 8;
                let sprite_size = if self.lcdc_get_obj_size(cpu) {
                    BIG_SPRITE_SIZE
                } else {
                    NORMAL_SPRITE_SIZE
                };

                //determine sprites effecting line
                let mut offset = 0x00;
                let ly = cpu.read_hw_reg(LY_ADDRESS);
                for _ in 0..OAM_TABLE_SIZE {
                    const OAM_BYTE_SIZE: usize = 4;
                    let oam = OAM {
                        y_position: cpu.read_hw_reg(OAM_TABLE_ADDRESS + offset),
                        x_position: cpu.read_hw_reg(OAM_TABLE_ADDRESS + offset + 1),
                        tile_index: cpu.read_hw_reg(OAM_TABLE_ADDRESS + offset + 2),
                        attributes: cpu.read_hw_reg(OAM_TABLE_ADDRESS + offset + 3),
                    };

                    if ly >= oam.y_position && ly - oam.y_position < sprite_size {
                        // on our line
                        self.current_line_sprites.push(oam);
                    }

                    offset += OAM_BYTE_SIZE;
                }

                // sort by x postilion least to greatest (sort the sprites left to right)
                self.current_line_sprites
                    .sort_by(|a, b| b.x_position.cmp(&a.x_position));
                // only 10 sprites per line (truncate sprites on line after sprite 10 left to right)
                self.current_line_sprites.truncate(10);
                // draw sprites will be drawn right to left
                self.current_line_sprites.reverse();
            }
        } else {
            self.debug_mode2_cycles += cycles;
        }
        (remainder, PpuStepResult::NoAction)
    }

    fn start_mode3_picture_gen(&mut self, cpu: &mut Cpu, cycles: u32) -> (u32, PpuStepResult) {
        // this 'start' state consumes no dots/cycles, ie we just pass the cycles value
        self.mode_func = Ppu::handle_mode3_picture_gen;
        Ppu::stat_set_mode_flag(cpu, LcdStatModeFlag::DataToLCD);
        self.current_mode_counter = 0;
        self.current_mode_oam_dma = false;

        (cycles, PpuStepResult::NoAction)
    }

    fn handle_mode3_picture_gen(&mut self, cpu: &mut Cpu, cycles: u32) -> (u32, PpuStepResult) {
        // Mode 3
        // Action: Reading OAM and VRAM to generate the picture
        // Duration: 168 to 291 dots, depending on sprite count
        // Accessible v-mem: None

        // Things that can effect a scanline mid render
        //  - BGP, OBP1, or OBP0 are changed before FIFO pixels are rendered
        //  - LCDC.1 is disabled during OAM fetch, which will abort sprite fetching
        //  - OAM DMA is triggered

        // how to deal with mode 3, which has variable length?
        // 0 sprites rendered in a scanline => 168 cycles
        // 40 sprites max for whole screen, 10 sprites max per scanline => 291 dots (291 - 168 = 109)
        // approx 10.9 dots per sprite (after 10 sprites are skipped on that scanline left to right)
        // total cycles for mode 3 = 168 + (109 * min(10, sprite_count) / 10)
        // this is a crude approximation, more accurate timing would be
        //      11 - min(5, (x + SCX) % 8) where SCX is the viewport position, and x is presumably
        //      the sprite position within the viewport
        //      if the sprite is over a window then replace SCX with (255 - WX)
        // additionally an active window pauses for at least 6 dots, as the background fetching
        //      mechanism starts over at the left side of the window.
        // finally background scrolling: If SCX % 8 is not zero at the start of the scanline,
        //      rendering is paused for that many dots while the shifter discards that many pixels
        //      from the leftmost tile.

        // quirk to emulate: When the window has already started rendering there is a bug that occurs
        // when WX is changed mid-scanline. When the value of WX changes after the window has started
        // rendering and the new value of WX is reached again, a pixel with color value of 0 and the
        // lowest priority is pushed onto the background FIFO.

        //need to obey configuration set in LCD Control (LCDC)
        // 7 LCD and PPU enable             0=Off, 1=On
        // 6 Window tile map area           0=9800-9BFF, 1=9C00-9FFF
        // 5 Window enable                  0=Off, 1=On
        // 4 BG and Window tile data area   0=8800-97FF, 1=8000-8FFF
        // 3 BG tile map area               0=9800-9BFF, 1=9C00-9FFF
        // 2 OBJ size                       0=8x8, 1=8x16
        // 1 OBJ enable                     0=Off, 1=On
        // 0 BG and Window enable/priority  0=Off, 1=On

        /*
        in mode 3 start
            max 10 sprites per line
            1. determine vec of sprites (indices/offsets relative to 0xfe00)
            2. calc delay
        mode 3 just collect cycles until we can proc
        */

        // each sprite adds 10.9 cycles to a base of 168, not accurate, just an approximation
        let target = 168 + (109 * self.current_line_sprites.len() as u32 / 10);
        let mut remainder = 0;
        self.current_mode_counter += cycles;
        if self.current_mode_counter >= target {
            remainder = self.current_mode_counter - target;
            self.debug_mode3_cycles += cycles - remainder;

            self.last_mode3_duration = target;
            self.mode_func = Ppu::start_mode0_hblank;

            let ly = cpu.read_hw_reg(LY_ADDRESS) as usize;
            let mut line = [PixelShade::White; GB_WIDTH];

            if self.lcdc_get_bg_and_window_enable(cpu) {
                let bg_line = self.render_bg_line(cpu, ly);

                //temp copy bg_line to line
                line = bg_line;
            }

            // if oam dma ran at all during this line, don't draw __SPRITE__ pixels to it
            if !self.current_mode_oam_dma {
                // todo draw line, but not sprites idk
            }

            //copy line to self.pixels for ly
            let base = ly * GB_WIDTH;
            for ind in 0..line.len() {
                self.pixels[base + ind] = line[ind];
            }
        } else {
            self.debug_mode3_cycles += cycles;
        }

        (remainder, PpuStepResult::NoAction)
    }

    fn render_bg_line(&mut self, cpu: &mut Cpu, ly: usize) -> [PixelShade; GB_WIDTH] {
        // do background
        let scx = cpu.read_hw_reg(SCX_ADDRESS) as usize;
        let scy = cpu.read_hw_reg(SCY_ADDRESS) as usize;

        let mut bg_line = [PixelShade::White; GB_WIDTH];

        let tile_map_y: usize = ((scy + ly) & 0xff) / TILE_SIZE;
        let tile_y_offset: usize = (scy + ly) % TILE_SIZE;

        let mut x_pos = 0;
        while x_pos < GB_WIDTH {
            //determine tile at pixel
            let tile_map_x = ((scx + x_pos) / TILE_SIZE) & 0x1f;
            let tile_x_offset = (scx + x_pos) % TILE_SIZE;
            let tile_x_pixel_count = TILE_SIZE - tile_x_offset;
            let tile_id = self.bg_tile_map_fetch(cpu, tile_map_y, tile_map_x);

            let tile = self.bg_tile_fetch(cpu, tile_id as usize);
            let start = (tile_y_offset * TILE_SIZE) + tile_x_offset;
            let end = start + tile_x_pixel_count;
            let tile_slice = &tile[start as usize..end as usize];

            if x_pos + tile_x_pixel_count < GB_WIDTH {
                //copy tile line to bg line
                for ind in 0..tile_x_pixel_count {
                    bg_line[x_pos + ind] = tile_slice[ind];
                }

                x_pos += tile_x_pixel_count;
            } else {
                //copy subset to bg_line
                for ind in 0..(GB_WIDTH - x_pos) {
                    bg_line[x_pos + ind] = tile_slice[ind];
                }
                break;
            }
        }

        if self.lcdc_get_window_enable(cpu) {
            // do window
        }
        bg_line
    }

    fn bg_tile_map_fetch(&self, cpu: &Cpu, tile_map_y: usize, tile_map_x: usize) -> usize {
        let offset = tile_map_y * 32 + tile_map_x;
        let base_address = if self.lcdc_get_bg_tile_map_area(cpu) {
            TILE_MAP_1_ADDRESS
        } else {
            TILE_MAP_0_ADDRESS
        };

        cpu.read_hw_reg(base_address + offset) as usize
    }

    fn bg_tile_fetch(&self, cpu: &Cpu, tile_id: usize) -> [PixelShade; TILE_SIZE * TILE_SIZE] {
        let bgp = cpu.read_hw_reg(BGP_ADDRESS);
        let shade_0 = PixelShade::from(bgp & 0b11);
        let shade_1 = PixelShade::from((bgp >> 2) & 0b11);
        let shade_2 = PixelShade::from((bgp >> 4) & 0b11);
        let shade_3 = PixelShade::from((bgp >> 6) & 0b11);

        let address = if self.lcdc_get_bg_and_window_tile_data_area(cpu) {
            TILE_DATA_BLOCK_0_ADDRESS + (tile_id * TILE_BYTES)
        } else {
            if tile_id < 128 {
                TILE_DATA_BLOCK_2_ADDRESS + (tile_id * TILE_BYTES)
            } else {
                TILE_DATA_BLOCK_1_ADDRESS + (tile_id - 128) * TILE_BYTES
            }
        };

        let tile_color_ids = Ppu::tile_fetch(cpu, address, false, false);

        let mut tile_pixels = [PixelShade::White; TILE_SIZE * TILE_SIZE];
        for (ind, color_id) in tile_color_ids.iter().enumerate() {
            tile_pixels[ind] = match color_id {
                0 => shade_0,
                1 => shade_1,
                2 => shade_2,
                3 => shade_3,
                _ => shade_0,
            }
        }

        tile_pixels
    }

    fn tile_fetch(
        cpu: &Cpu,
        tile_address: usize,
        mirror_x: bool,
        mirror_y: bool,
    ) -> [u8; TILE_SIZE * TILE_SIZE] {
        let mut tile_bytes = [0u8; TILE_BYTES];
        for ind in 0..TILE_BYTES {
            tile_bytes[ind] = cpu.read_hw_reg(tile_address + ind);
        }

        let mut color_ids = [0u8; TILE_SIZE * TILE_SIZE];
        for pair_ind in 0..TILE_SIZE {
            let ind = if mirror_y {
                //14 - (pair_ind * 2); bottom-to-top indexing
                (TILE_SIZE - TILE_BYTES_PER_ROW) - (pair_ind * TILE_BYTES_PER_ROW)
            } else {
                //pair_ind * 2; top-to-bottom indexing
                pair_ind * TILE_BYTES_PER_ROW
            };
            //each row is made out of two bytes, first byte contains the low bits
            let low_bits = tile_bytes[ind];
            //second the high bits
            let high_bits = tile_bytes[ind + 1];

            //column left-to-right
            for col in 0..TILE_SIZE {
                let bit_shift = if mirror_x {
                    // column and bit order have inverse relationship
                    // 0 to 7 effectively right-to-left
                    col
                } else {
                    //7 to 0; left-to-right
                    (TILE_SIZE - 1) - col
                };

                // we interleave the bits from the two bytes
                let high_bit = ((high_bits >> bit_shift) & 1) << 1;
                let low_bit = (low_bits >> bit_shift) & 1;

                // the 2 bit values are the color_ids
                let row = pair_ind * 8;
                color_ids[row + col] = high_bit | low_bit;
            }
        }

        color_ids
    }

    pub fn get_pixels(&self) -> [PixelShade; GB_WIDTH * GB_HEIGHT] {
        self.pixels
    }

    /** This indicates whether the LCD is on and the PPU is active. When false both off,
    which grants **immediate and full access** to VRAM, OAM, etc.
    This is not allowed to be changed outside of vblank */
    fn lcdc_get_lcd_ppu_enabled(&self, cpu: &Cpu) -> bool {
        cpu.read_hw_reg(LCDC_ADDRESS) & 0b1000_0000 != 0
    }

    /** Indicates which background map the Window uses for rendering. When it’s false, the 0x9800
    tilemap is used, otherwise it’s the 0x9c00 one.
    https://gbdev.io/pandocs/Tile_Maps.html#vram-tile-maps */
    // fn lcdc_get_window_tile_map_area(&self, cpu: &Cpu) -> bool {
    //     cpu.read_hw_reg(LCDC_ADDRESS) & 0b0100_0000 != 0
    // }

    /** Indicates whether the window shall be displayed or not. */
    fn lcdc_get_window_enable(&self, cpu: &Cpu) -> bool {
        cpu.read_hw_reg(LCDC_ADDRESS) & 0b0010_0000 != 0
    }

    /** Indicates which addressing mode the BG and Window use to pick tiles. Sprites aren’t affected
    by this, and will always use $8000 addressing mode.
    0x8000 - 0x8fff when true
    0x8800 - 0x87ff when false
    https://gbdev.io/pandocs/Tile_Data.html#vram-tile-data */
    fn lcdc_get_bg_and_window_tile_data_area(&self, cpu: &Cpu) -> bool {
        cpu.read_hw_reg(LCDC_ADDRESS) & 0b0001_0000 != 0
    }

    /** Similar to LCDC bit 6: if false, the BG uses tilemap 0x9800, otherwise tilemap 0x9c00. */
    fn lcdc_get_bg_tile_map_area(&self, cpu: &Cpu) -> bool {
        cpu.read_hw_reg(LCDC_ADDRESS) & 0b0000_1000 != 0
    }

    /** Indicates the sprite size (1 tile or 2 stacked vertically). False 8x8, true 8x16. */
    fn lcdc_get_obj_size(&self, cpu: &Cpu) -> bool {
        cpu.read_hw_reg(LCDC_ADDRESS) & 0b0000_0100 != 0
    }

    /** Indicates whether sprites are displayed or not. */
    // fn lcdc_get_obj_enable(&self, cpu: &Cpu) -> bool {
    //     cpu.read_hw_reg(LCDC_ADDRESS) & 0b0000_0010 != 0
    // }

    /** When false, both background and window become blank (white), and the Window Display is
     * ignored in that case. Only Sprites may still be displayed (if enabled). */
    fn lcdc_get_bg_and_window_enable(&self, cpu: &Cpu) -> bool {
        cpu.read_hw_reg(LCDC_ADDRESS) & 0b0000_0001 != 0
    }

    // fn stat_get_mode_flag(cpu: &Cpu) -> LcdStatModeFlag {
    //     LcdStatModeFlag::from(cpu.read_hw_reg(STAT_ADDRESS) & 0b0000_0011)
    // }

    fn stat_set_mode_flag(cpu: &mut Cpu, flag: LcdStatModeFlag) {
        let stat = cpu.read_hw_reg(STAT_ADDRESS) & 0b1111_1100;
        cpu.write_hw_reg(STAT_ADDRESS, stat | flag as u8);
    }

    /// If the interrupt is enabled in the stat register, flag the interrupt
    fn stat_flag_interrupt(cpu: &mut Cpu, int: StatInterrupt) {
        let interrupt_enabled = cpu.read_hw_reg(STAT_ADDRESS) & (int as u8) != 0;
        if interrupt_enabled {
            //flag STAT interrupt
            let interrupt_flag = cpu.read_hw_reg(INTERRUPT_FLAG_ADDRESS);
            cpu.write_hw_reg(
                INTERRUPT_FLAG_ADDRESS,
                interrupt_flag | InterruptFlags::LcdStat as u8,
            );
        }
    }

    fn stat_update_ly_eq_lyc_bit(cpu: &mut Cpu) {
        let stat = cpu.read_hw_reg(STAT_ADDRESS);
        let ly = cpu.read_hw_reg(LY_ADDRESS);
        let lyc = cpu.read_hw_reg(LYC_ADDRESS);

        let new_bit2 = ((ly == lyc) as u8) << 2;
        let last_bit2 = stat & 0b0000_0100;

        if new_bit2 != 0 && last_bit2 == 0 {
            //only flag for interrupt when bit2 goes from 0 to 1
            Ppu::stat_flag_interrupt(cpu, StatInterrupt::LycEqLy);
        }

        let stat_updated = stat & 0b1111_1011 | new_bit2;
        cpu.write_hw_reg(STAT_ADDRESS, stat_updated);
    }
}
