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
pub const LCDC_ADDRESS: usize = 0xff40; //LCD Control
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