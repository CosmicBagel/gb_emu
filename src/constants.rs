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

pub const IS_GUI_ACTIVE_DEFAULT: bool = true;
