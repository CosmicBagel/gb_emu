use std::cmp::min_by;

//ppu = picture processing unit
use crate::cpu::Cpu;

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

const LY_ADDRESS: usize = 0xff44; //horizontal line register (0-153)
const LYC_ADDRESS: usize = 0xff45; //LY Compare (interrupt is triggered when matches LY) (0-153)
const STAT_ADDRESS: usize = 0xff41; //LCD Stat
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
const LCDC_ADDRESS: usize = 0xff40; //LCD Control
                                    /*
                                    7 LCD and PPU enable             0=Off, 1=On
                                    6 Window tile map area           0=9800-9BFF, 1=9C00-9FFF
                                    5 Window enable                  0=Off, 1=On
                                    4 BG and Window tile data area   0=8800-97FF, 1=8000-8FFF
                                    3 BG tile map area               0=9800-9BFF, 1=9C00-9FFF
                                    2 OBJ size                       0=8x8, 1=8x16
                                    1 OBJ enable                     0=Off, 1=On
                                    0 BG and Window enable/priority  0=Off, 1=On
                                    */

const OAM_TABLE_ADDRESS: usize = 0xfe00; //Sprite table, max 40 sprites of 4 bytes each
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

#[derive(PartialEq)]
pub enum PpuStepResult {
    NoAction,
    Draw,
}

pub struct Ppu {
    pixels: [PixelShade; 160 * 144],
    current_mode_counter: u32,
    last_mode3_duration: u32,
    mode_func: PpuModeFunc,
    current_line_sprites: Vec<OAM>,
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
enum PixelShade {
    White = 0b00,
    Light = 0b01,
    Medium = 0b10,
    Dark = 0b11,
}

type PpuModeFunc = fn(&mut Ppu, &mut Cpu, cycles: u32) -> (u32, PpuStepResult);

impl Ppu {
    pub fn new() -> Ppu {
        Ppu {
            pixels: [PixelShade::White; 160 * 144],
            current_mode_counter: 0,
            last_mode3_duration: 0,
            mode_func: Ppu::handle_mode0_hblank,
            current_line_sprites: vec![],
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
        //todo: when a mode switches to another mode,
        //      update mode in lcd stat register
        //      update interrupt flags
        todo!("update lcd stat & interrupt flags");
        while cycles > 0 {
            let (c, r) = (self.mode_func)(self, cpu, cycles);

            cycles = c;
            //ensure that a draw result is never overwritten
            if result != PpuStepResult::Draw {
                result = r;
            }
        }
        //todo: sync LY=LYC status in LCD STAT register
        //      sync LY (LCD Y coordinate aka which scanline we're on)
        //      if LY=LYC went from 0 to 1, then set the interrupt bit in STAT register
        todo!("sync LY=LYC");

        result
    }

    fn start_mode0_hblank(&mut self, cpu: &mut Cpu, cycles: u32) -> (u32, PpuStepResult) {
        // this 'start' state consumes no dots/cycles, ie we just pass the cycles value
        self.mode_func = Ppu::handle_mode0_hblank;
        // set flag interrupt hblank
        // update LCD state mode
        todo!("lcd state mode & flag interrupt");
        self.current_mode_counter = 0;
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

            let ly = cpu.read_hw_reg(LY_ADDRESS);
            cpu.write_hw_reg(LY_ADDRESS, ly + 1);

            if ly >= 144 {
                //end of 'visible' scanlines, we switch to vblank for 10 invisible scannlines
                self.mode_func = Ppu::start_mode1_vblank;
            } else {
                //next scanline is visible
                self.mode_func = Ppu::start_mode2_object_search;
            }
        }

        (remainder, PpuStepResult::NoAction)
    }

    fn start_mode1_vblank(&mut self, cpu: &mut Cpu, cycles: u32) -> (u32, PpuStepResult) {
        // this 'start' state consumes no dots/cycles, ie we just pass the cycles value
        self.mode_func = Ppu::handle_mode1_vblank;
        // set flag interrupt
        // update LCD state mode
        todo!("lcd state mode & flag interrupt");
        self.current_mode_counter = 0;
        (cycles, PpuStepResult::Draw)
    }

    fn handle_mode1_vblank(&mut self, cpu: &mut Cpu, cycles: u32) -> (u32, PpuStepResult) {
        // Mode 1
        // Action: Nothing (VBlank)
        // Duration: 4560 dots (10 scanlines), 456 dots per "invisible scanline"
        // Accessible v-mem: VRAM, OAM, CGB palettes
        let target = 456;
        let mut left_over_cycles = 0;

        // if count > target and we're on LY = 153, LY = 0 and leave vblank
        // if count > target and LV <= 153, increment LY
        if self.current_mode_counter + cycles >= target {
            left_over_cycles = self.current_mode_counter + cycles - target;
            let ly = cpu.read_hw_reg(LY_ADDRESS);
            if ly <= 153 {
                cpu.write_hw_reg(LY_ADDRESS, ly + 1);
                self.current_mode_counter = 0;
            } else {
                //start over at top of the screen
                cpu.write_hw_reg(LY_ADDRESS, 0);
                self.mode_func = Ppu::start_mode2_object_search;
            }
        } else {
            self.current_mode_counter += cycles;
        }

        (left_over_cycles, PpuStepResult::NoAction)
    }

    fn start_mode2_object_search(&mut self, cpu: &mut Cpu, cycles: u32) -> (u32, PpuStepResult) {
        // this 'start' state consumes no dots/cycles, ie we just pass the cycles value
        self.mode_func = Ppu::handle_mode2_object_search;
        // set flag interrupt
        // update LCD state mode
        todo!("lcd state mode & flag interrupt");
        self.current_mode_counter = 0;

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
            self.mode_func = Ppu::start_mode3_picture_gen;

            //mode 2's actual job: see what sprites are on this LY
            //10 sprites max, sorted in order of greatest x to least x
            //  so that the sprite with least x value will draw above sprites with greater x values
            self.current_line_sprites.clear();
            let sprite_size = if self.lcdc_get_obj_size(cpu) { 15 } else { 7 };

            //determine sprites effecting line
            let mut offset = 0x00;
            let ly = cpu.read_hw_reg(LY_ADDRESS);
            for _ in 0..40 {
                let oam = OAM {
                    y_position: cpu.read_hw_reg(OAM_TABLE_ADDRESS + offset),
                    x_position: cpu.read_hw_reg(OAM_TABLE_ADDRESS + offset + 1),
                    tile_index: cpu.read_hw_reg(OAM_TABLE_ADDRESS + offset + 2),
                    attributes: cpu.read_hw_reg(OAM_TABLE_ADDRESS + offset + 3),
                };

                if ly >= oam.y_position && ly - oam.y_position <= sprite_size {
                    // on our line
                    self.current_line_sprites.push(oam);
                }

                offset += 4;
            }

            // sort by x postilion least to greatest (sort the sprites left to right)
            self.current_line_sprites
                .sort_by(|a, b| b.x_position.cmp(&a.x_position));
            // only 10 sprites per line (truncate sprites on line after sprite 10 left to right)
            self.current_line_sprites.truncate(10);
            // draw sprites will be drawn right to left
            self.current_line_sprites.reverse();
        }
        (remainder, PpuStepResult::NoAction)
    }

    fn start_mode3_picture_gen(&mut self, cpu: &mut Cpu, cycles: u32) -> (u32, PpuStepResult) {
        // this 'start' state consumes no dots/cycles, ie we just pass the cycles value
        self.mode_func = Ppu::handle_mode3_picture_gen;
        // set flag interrupt
        // update LCD state mode
        todo!("lcd state mode & flag interrupt");
        self.current_mode_counter = 0;

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
            self.mode_func = Ppu::start_mode0_hblank;
        }

        todo!("impl mode3");

        (remainder, PpuStepResult::NoAction)
    }

    pub fn render_to_screen(&mut self) {
        // will take another param that will allow drawing to screen (pixels lib)
        //might turn this into a 'get image' func and have pixels lib
        //interfaced with outside of ppu

        todo!()
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
    fn lcdc_get_window_tile_map_area(&self, cpu: &Cpu) -> bool {
        cpu.read_hw_reg(LCDC_ADDRESS) & 0b0100_0000 != 0
    }

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
    fn lcdc_get_obj_enable(&self, cpu: &Cpu) -> bool {
        cpu.read_hw_reg(LCDC_ADDRESS) & 0b0000_0010 != 0
    }

    /** When false, both background and window become blank (white), and the Window Display is
     * ignored in that case. Only Sprites may still be displayed (if enabled). */
    fn lcdc_get_bg_and_window_enable(&self, cpu: &Cpu) -> bool {
        cpu.read_hw_reg(LCDC_ADDRESS) & 0b0000_0001 != 0
    }
}
