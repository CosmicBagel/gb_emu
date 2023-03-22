pub const DEFAULT_ROM: &str = "10-bit ops.gb";

pub const DR_GB_LOGGING_ENABLED: bool = false;

pub const GB_WIDTH: usize = 160;
pub const GB_HEIGHT: usize = 144;
pub const INVISIBLE_VBLANK_LINES: usize = 10;

pub const WINDOW_WIDTH: u32 = 640;
pub const WINDOW_HEIGHT: u32 = 480;
pub const WINDOW_TITLE: &str = "gb_emu";

pub const OAM_TABLE_SIZE: usize = 40;

pub const WHITE_SHADE: [u8; 4] = [0xff, 0xff, 0xff, 0xff];
pub const LIGHT_SHADE: [u8; 4] = [0xaa, 0xaa, 0xaa, 0xff];
pub const MEDIUM_SHADE: [u8; 4] = [0x66, 0x66, 0x66, 0xff];
pub const DARK_SHADE: [u8; 4] = [0x22, 0x22, 0x22, 0xff];
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
