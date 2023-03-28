pub const DEFAULT_ROM: &str = "02-interrupts.gb";

pub const DR_GB_LOGGING_ENABLED: bool = false;

pub const GB_WIDTH: usize = 160;
pub const GB_HEIGHT: usize = 144;
pub const INVISIBLE_VBLANK_LINES: usize = 10;

pub const CLOCKS_PER_FRAME: u32 = 70224;

pub const WINDOW_WIDTH: u32 = 800;
pub const WINDOW_HEIGHT: u32 = 720;
pub const WINDOW_TITLE: &str = "gb_emu";

pub const OAM_TABLE_SIZE: usize = 40;
/// obj canvas is offset from the screen by -8, -16, this means an obj at 0,0 is offscreen
/// and an obj 0,2 has two pixels offscreen at LY = 0 and LY = 1. 
pub const OBJ_CANVAS_Y_OFFSET: u8 = 16;
pub const OBJ_CANVAS_X_OFFSET: u8 = 8;

pub const OBJ_TRANSPARENT_COLOR_ID: u8 = 0b00;

pub const WHITE_SHADE: [u8; 4] = [0xff, 0xff, 0xff, 0xff];
pub const LIGHT_SHADE: [u8; 4] = [0xaa, 0xaa, 0xaa, 0xff];
pub const MEDIUM_SHADE: [u8; 4] = [0x55, 0x55, 0x55, 0xff];
pub const DARK_SHADE: [u8; 4] = [0x00, 0x00, 0x00, 0xff];
pub const DISABLED_SHADE: [u8; 4] = [0xff, 0x00, 0x00, 0xff];

/// Tiles are 8*8, each row is two bytes, each pixel is 2 bits
/// the bits of the two bytes are interleaved together to produce
/// the 2 bit color id values
pub const TILE_SIZE: usize = 8;
pub const TILE_BYTES_PER_ROW: usize = 2;
pub const TILE_BYTES: usize = TILE_SIZE * TILE_BYTES_PER_ROW;

pub const IS_GUI_ACTIVE_DEFAULT: bool = true;

pub const CPU_CYCLES_PER_HALTED_STEP: u32 = 4;
pub const OAM_DMA_CYCLES: u32 = 160;

pub const SUPPORTED_MBC_TYPES: [u8; 1] = [0];
