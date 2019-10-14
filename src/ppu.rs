use crate::cpu::{Interrupt, MemoryAddress};
use crate::util;

const REG_START: MemoryAddress = 0xFF40;
const REG_END: MemoryAddress = 0xFF4B;

pub const WIDTH: usize = 160;
pub const HEIGHT: usize = 144;
pub const RAM_SIZE: usize = 0x2000;
pub const OAM_SIZE: usize = 0xA0;

pub const RAM_START: MemoryAddress = 0x8000;
pub const RAM_END: MemoryAddress = RAM_START + RAM_SIZE as MemoryAddress;
pub const OAM_START: MemoryAddress = 0xFE00;
pub const OAM_END: MemoryAddress = OAM_START + OAM_SIZE as MemoryAddress;

const LCDC: MemoryAddress = 0xFF40;
const STAT: MemoryAddress = 0xFF41;
const SCY: MemoryAddress = 0xFF42;
const SCX: MemoryAddress = 0xFF43;
const LY: MemoryAddress = 0xFF44;
const LYC: MemoryAddress = 0xFF45;

const BGP: MemoryAddress = 0xFF47;
const OBP0: MemoryAddress = 0xFF48;
const OBP1: MemoryAddress = 0xFF49;
const WY: MemoryAddress = 0xFF4A;
const WX: MemoryAddress = 0xFF4B;

const SCANLINE_CPU_CYCLES: i64 = 456;

const WHITE: u8 = 255;
const LIGHT_GRAY: u8 = 0xCC;
const DARK_GRAY: u8 = 0x77;
const BLACK: u8 = 0;

enum TileType {
    Background,
    Window,
}

#[derive(PartialEq)]
pub enum Lcd {
    HBlank,
    VBlank,
    SearchingRam,
    TransferringDataToDriver,
}

impl std::convert::From<u8> for Lcd {
    fn from(n: u8) -> Lcd {
        match n {
            0b00 => Lcd::HBlank,
            0b01 => Lcd::VBlank,
            0b10 => Lcd::SearchingRam,
            0b11 => Lcd::TransferringDataToDriver,
            _ => unreachable!(),
        }
    }
}

impl std::convert::From<Lcd> for u8 {
    fn from(l: Lcd) -> u8 {
        match l {
            Lcd::HBlank => 0,
            Lcd::VBlank => 1,
            Lcd::SearchingRam => 2,
            Lcd::TransferringDataToDriver => 3,
        }
    }
}

pub enum LcdControl {
    BackgroundEnable,
    ObjectEnable,
    ObjectSize,
    BackgroundTileMapSelect,
    BackgroundAndWindodwTileDataSelect,
    WindowDisplayEnable,
    WindowTileMapSelect,
    Enable,
}

pub struct Ppu {
    ram: [u8; RAM_SIZE],
    oam: [u8; OAM_SIZE],
    lcdc: u8,
    stat: u8,
    scy: u8,
    scx: u8,
    bgp: u8,
    obp0: u8,
    obp1: u8,
    wy: u8,
    wx: u8,
    ly: u8,
    lyc: u8,
    pub frame_buffer: [u32; WIDTH * HEIGHT],
}

impl Ppu {
    pub fn new() -> Self {
        // TODO: What if boot rom is enabled?
        Ppu {
            ram: [0; RAM_SIZE],
            oam: [0; OAM_SIZE],
            lcdc: 0x91,
            stat: 0,
            scy: 0,
            scx: 0,
            bgp: 0xFC,
            obp0: 0xFF,
            obp1: 0xFF,
            wy: 0,
            wx: 0,
            ly: 0,
            lyc: 0,
            frame_buffer: [0; WIDTH * HEIGHT],
        }
    }

    pub fn read(&self, address: MemoryAddress) -> u8 {
        self.ram[address as usize]
    }

    pub fn read_ram(&self, address: MemoryAddress) -> u8 {
        self.ram[address as usize]
    }

    pub fn write(&self, address: MemoryAddress) -> u8 {
        self.ram[address as usize]
    }

    pub fn write_ram(&self, address: MemoryAddress) -> u8 {
        self.ram[address as usize]
    }

    pub fn update(&mut self, cycles: u8) -> u8 {
        let mut interrupt = 0;
        let is_enabled = util::ith_bit(self.lcdc, LcdControl::Enable as u8);

        let set_mode_bits = |status, mode| (status & 0b1111_1100) | mode as u8;

        if !is_enabled {
            // set the mode to 1 during lcd disabled and reset scan line
            self.scan_counter = SCANLINE_CPU_CYCLES;
            self.ly = 0;
            let default_status = set_mode_bits(self.stat, Lcd::VBlank);
            self.stat = default_status;
            interrupt
        }

        let line = self.ly;
        let mode = Lcd::from(self.stat & 0b0000_0011);

        let (new_mode, should_interrupt) = if line >= 144 {
            (Lcd::VBlank, util::ith_bit(self.stat, 4))
        } else if self.scan_counter >= 456 - 80 {
            (Lcd::SearchingRam, util::ith_bit(self.stat, 5))
        } else if self.scan_counter >= 456 - 80 - 172 {
            (Lcd::TransferringDataToDriver, false)
        } else {
            (Lcd::HBlank, util::ith_bit(self.stat, 3))
        };

        if new_mode != mode && should_interrupt {
            interrupt = util::set_bit(interrupt, Interrupt::LCD as u8);
        }

        // check the coincidence flag
        let new_status = if line == self.lyc {
            if util::ith_bit(self.stat, 6) {
                interrupt = util::set_bit(interrupt, Interrupt::LCD as u8);
            }
            self.stat | (1 << 2)
        } else {
            self.stat & !(1 << 2)
        };

        self.stat = set_mode_bits(new_status, new_mode);

        self.scan_counter -= i64::from(cycles);

        if self.scan_counter <= 0 {
            self.ly = line + 1;

            self.scan_counter = 456;

            if line < 144 {
                if util::ith_bit(self.lcdc, LcdControl::BackgroundEnable as u8) {
                    self.render_tiles(line);
                }

                if util::ith_bit(self.lcdc, LcdControl::ObjectEnable as u8) {
                    self.render_sprites(line);
                }
            } else if line == 144 {
                interrupt = util::set_bit(interrupt, Interrupt::VBlank as u8);
            } else if line == 154 {
                self.ly = 0;
            }
        }
        interrupt
    }

    fn get_tile_data(&self, x: u8, y: u8, tile_type: TileType) -> u16 {
        let get_tile_num = |x, y| -> u8 {
            let (row, col) = (y / 8, x / 8);

            let map_select_bit = match tile_type {
                TileType::Background => LcdControl::BackgroundTileMapSelect as u8,
                TileType::Window => LcdControl::WindowTileMapSelect as u8,
            };

            // The Background Tile Map stores a 32x32 tile grid with corresponding tile numbers (one byte each).
            let tile_num_base = if util::ith_bit(self.lcdc, map_select_bit) {
                0x9C00
            } else {
                0x9800
            } - RAM_START;
            let tile_num_address = tile_num_base + row as u16 * 32 + col as u16;

            self.ram[tile_num_address]
        };

        // Tiles consist of 8x8 pixels. Each line of the tile occupies two bytes in memory (16 consecutive bytes total for any given tile).
        let tile_address = if util::ith_bit(
            self.lcdc,
            LcdControl::BackgroundAndWindodwTileDataSelect as u8,
        ) {
            let tile_num = get_tile_num(x, y) as u8;
            0x8000 + tile_num as u16 * 16
        } else {
            let tile_num = get_tile_num(x, y) as i8;
            0x8800 + (tile_num as i16 + 128) as u16 * 16
        } - RAM_START;

        let address = (tile_address + (2 * (y % 8)) as u16) as usize;
        u16::from_le_bytes(self.ram[address..address + 2])
    }

    fn render_tiles(&mut self, line: u8) {
        let (scroll_x, scroll_y) = (self.scx, self.scy);
        let (window_x, window_y) = (self.wx, self.wy);

        let is_window_enabled = util::ith_bit(self.lcdc, LcdControl::WindowDisplayEnable as u8);
        let is_window_visible = is_window_enabled && window_y <= line;

        for x in 0..VIDEO_WIDTH as u8 {
            let (x_pos, y_pos, tile_type) = if is_window_visible && x >= window_x {
                (x - window_x, line - window_y, TileType::Window)
            } else {
                (
                    x.wrapping_add(scroll_x),
                    line.wrapping_add(scroll_y),
                    TileType::Background,
                )
            };

            let (byte2, byte1) =
                util::little_endian::u8(self.get_tile_data(x_pos, y_pos, tile_type));

            let colour_bit_num = 7 - (x_pos & 0b0111);
            let color_num = ((util::ith_bit(byte2, colour_bit_num) as u8) << 1)
                | util::ith_bit(byte1, colour_bit_num) as u8;

            let c = self.get_color(color_num, self.bgp) as u32;
            self.frame_buffer[line as usize * VIDEO_WIDTH as usize + x as usize] =
                (0xff << 24) | (c << 16) | (c << 8) | c;
        }
    }

    fn get_color(&self, colour_id: u8, palette: u8) -> u8 {
        let colour_bits = (palette >> (2 * colour_id)) & 0b11;
        match colour_bits {
            3 => BLACK,
            2 => DARK_GRAY,
            1 => LIGHT_GRAY,
            0 => WHITE,
            _ => unreachable!(),
        }
    }

    fn render_sprites(&mut self, scan_line: u8) {
        let y_size = if util::ith_bit(self.lcdc, LcdControl::ObjectSize as u8) {
            16
        } else {
            8
        };

        struct Sprite {
            x: u8,
            y: u8,
            pattern: u8,
            priority: bool,
            x_flip: bool,
            y_flip: bool,
            palette: MemoryAddress,
        };

        impl Sprite {
            pub fn new(bytes: &[u8]) -> Self {
                Sprite {
                    x: bytes[1].wrapping_sub(8),
                    y: bytes[0].wrapping_sub(16),
                    pattern: bytes[2],
                    priority: util::ith_bit(bytes[3], 7),
                    x_flip: util::ith_bit(bytes[3], 5),
                    y_flip: util::ith_bit(bytes[3], 6),
                    palette: if util::ith_bit(bytes[3], 4) {
                        OBP1
                    } else {
                        OBP0
                    },
                }
            }
        };

        const NUM_SPRITES: u16 = 40;
        for i in (0..4 * NUM_SPRITES as u16).step_by(4) {
            let sprite = Sprite::new(self.oam[i..i + 4]);

            let does_sprite_intercept_line =
                scan_line >= sprite.y && (scan_line < (sprite.y + y_size));
            if does_sprite_intercept_line {
                let line = 2 * if sprite.y_flip {
                    // TODO: WHY?1
                    -(1 + scan_line as i16 - sprite.y as i16 - y_size as i16)
                } else {
                    scan_line as i16 - sprite.y as i16
                };

                let address = sprite.pattern as u16 * 16 + line as u16;
                let (data2, data1) = util::little_endian::u8(self.ram[address]);

                for pixel in 0..=7 {
                    let pixel_bit = if sprite.x_flip { pixel } else { 7 - pixel };
                    let color_num = ((util::ith_bit(data2, pixel_bit) as u8) << 1)
                        | util::ith_bit(data1, pixel_bit) as u8;
                    let c = self.get_color(color_num, sprite.palette) as u32;

                    // white is transparent
                    if c != (WHITE as u32) {
                        self.frame_buffer
                            [scan_line as usize * 160 + sprite.x as usize + pixel as usize] =
                            (0xff << 24) | (c << 16) | (c << 8) | c;
                    }
                }
            }
        }
    }
    pub fn dma(&mut self, chunk: &[u8]) {

    }
}
