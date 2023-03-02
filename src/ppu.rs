//ppu = picture processing unit
use crate::cpu::Cpu;

pub enum PpuStepResult {
    Normal,
    VBlank
}

pub struct Ppu {
    v_ram: Vec<u8>, //wait no all memory is in the cpu
    hblank_cycle_counter: u32,
    vblank_cycle_counter: u32,
    mode: PpuMode,
}

enum PpuMode {
    Mode0HBlank,
    Mode1VBlank,
    Mode2ObjectSearch,
    Mode3PictureGen,
}

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

LCD control register (0xff40)
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

Mode 2  
    Action: Searching OAM for OBJs whose Y coordinate overlap this line	
    Duration: 80 dots	
    Accessible v-mem: VRAM, CGB palettes
Mode 3	
    Action: Reading OAM and VRAM to generate the picture	
    Duration: 168 to 291 dots, depending on sprite count	
    Accessible v-mem: None
Mode 0	
    Action: Nothing (HBlank)	
    Duration: 85 to 208 dots, depending on previous mode 3 duration	
    Accessible v-mem: VRAM, OAM, CGB palettes
Mode 1	    
    Action: Nothing (VBlank)	
    Duration: 4560 dots (10 scanlines)	
    Accessible v-mem: VRAM, OAM, CGB palettes

The following sequence is typical when the display is enabled:
Mode 2  2_____2_____2_____2_____2_____2___________________2____
Mode 3  _33____33____33____33____33____33__________________3___
Mode 0  ___000___000___000___000___000___000________________000
Mode 1  ____________________________________11111111111111_____
 */


impl Ppu {
    pub fn new() -> Ppu{
        Ppu {
            v_ram: vec![0; 0x2_000], //8 KiB ram lol
            hblank_cycle_counter: 0,
            vblank_cycle_counter: 0,
            mode: PpuMode::Mode0HBlank,
        }
    }

    pub fn do_step(&mut self, cpu: &mut Cpu, cycle_count: u32) -> PpuStepResult {

        //update cycle count
        // switch mode as appropriate if enough cycles reached
        // ensure excess cycles are added to next mode's cycle duration

        // how to deal with modes of variable length?

        PpuStepResult::Normal
    }


    pub fn render_to_screen(&mut self) {
        // will take another param that will allow drawing to screen (pixels lib)
        todo!()
    }
}
