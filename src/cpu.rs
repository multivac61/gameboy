use crate::util::{little_endian, ith_bit};
use crate::instructions::Instruction::*;
use crate::memory_bus::MemoryBus;
use crate::instructions::Instruction;

pub const VIDEO_WIDTH: u32 = 160;
pub const VIDEO_HEIGHT: u32 = 144;

const CLOCK_FREQUENCY: f64 = 4_194_304.0;

pub const OAM: u16 = 0xFE00;

pub const DIV: u16 = 0xFF04;
const TIMA: u16 = 0xFF05;
const TMA: u16 = 0xFF06;
const TMC: u16 = 0xFF07;

const LCDC: u16 = 0xFF40;
const STAT: u16 = 0xFF41;
const SCY: u16 = 0xFF42;
const SCX: u16 = 0xFF43;
const BGP: u16 = 0xFF47;
const OBP0: u16 = 0xFF48;
const OBP1: u16 = 0xFF49;
const WY: u16 = 0xFF4A;
const WX: u16 = 0xFF4B;
const LY: u16 = 0xFF44;
const LYC: u16 = 0xFF45;
pub const DMA: u16 = 0xFF46;

const SCANLINE_CPU_CYCLES: i64 = 456;

const WHITE: u8 = 255;
const LIGHT_GRAY: u8 = 0xCC;
const DARK_GRAY: u8 = 0x77;
const BLACK: u8 = 0;

const INTERRUPT_REQUEST_REGISTER: u16 = 0xFF0F;
const INTERRUPT_ENABLE_REGISTER: u16 = 0xFFFF;

pub type MemoryAddress = u16;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Register { B, C, D, E, H, L, A, F }

impl std::convert::From<u8> for Register {
    fn from(n: u8) -> Register {
        match n {
            0 => Register::B,
            1 => Register::C,
            2 => Register::D,
            3 => Register::E,
            4 => Register::H,
            5 => Register::L,
            6 => Register::A,
            7 => Register::F,
            _ => unreachable!()
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Register16bit {
    BC = Register::B as isize,
    DE = Register::D as isize,
    HL = Register::H as isize,
    AF = Register::A as isize,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ConditionalFlag {
    Zero = 7,
    Subtract = 6,
    HalfCarry = 5,
    Carry = 4,
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
            _ => unreachable!()
        }
    }
}

pub enum LcdControl {
//Bit 7 - LCD Display Enable (0=Off, 1=On)
//Bit 6 - Window Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)
//Bit 5 - Window Display Enable (0=Off, 1=On)
//Bit 4 - BG & Window Tile Data Select (0=8800-97FF, 1=8000-8FFF)
//Bit 3 - BG Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)
//Bit 2 - OBJ (Sprite) Size (0=8x8, 1=8x16)
//Bit 1 - OBJ (Sprite) Display Enable (0=Off, 1=On)
//Bit 0 - BG Display (for CGB see below) (0=Off, 1=On)

    BackgroundEnable,
    ObjectEnable,
    ObjectSize,
    BackgroundTileMapSelect,
    BackgroundAndWindodwTileDataSelect,
    WindowDisplayEnable,
    WindowTileMapSelect,
    Enable,
}

// Bit locations
pub enum Interrupt {
    VBlank,
    LCD,
    Timer,
    SerialTxComplete,
    Joypad,
}

impl std::convert::From<u8> for Interrupt {
    fn from(n: u8) -> Interrupt {
        match n {
            0b000 => Interrupt::VBlank,
            0b001 => Interrupt::LCD,
            0b010 => Interrupt::Timer,
            0b011 => Interrupt::SerialTxComplete,
            0b100 => Interrupt::Joypad,
            _ => unreachable!()
        }
    }
}

#[derive(Clone, Copy)]
pub enum Rom {
    Local = 1,
    Bank1 = 2,
    Bank2 = 3,
}

impl std::convert::From<u8> for Rom {
    fn from(n: u8) -> Rom {
        match n {
            1 => Rom::Local,
            2 => Rom::Bank1,
            3 => Rom::Bank2,
            _ => unreachable!()
        }
    }
}

#[derive(Clone, Copy)]
pub enum Ram {
    Bank0,
//    Bank1,
//    Bank2,
//    Bank3,
}

pub struct Cpu {
    reg: [u8; 8],
    pc: MemoryAddress,
    mem: MemoryBus,
    sp: MemoryAddress,
    is_halted: bool,
    is_stopped: bool,
    are_interrupts_enabled: bool,
    timer_counter: i64,
    scan_counter: i64,
    divider_counter: u16,
    pub display: [[u8; 140]; 160],
}

struct Flags {
    z: bool,
    n: bool,
    h: bool,
    c: bool,
}

impl Cpu {
    pub fn new(cartridge: &[u8]) -> Self {
        Cpu {
            reg: [0x01, 0xB0, 0x00, 0x13, 0x00, 0xD8, 0x01, 0x4D],
            pc: 0,
            mem: MemoryBus::new(cartridge),
            sp: 0xFFFF,
            is_halted: false,
            is_stopped: false,
            are_interrupts_enabled: false,
            timer_counter: 1024,
            scan_counter: 0,
            divider_counter: 0,
            display: [[0; 140]; 160],
        }
    }

    fn update_timers(&mut self, cycles: u8) {
        self.divider_counter += u16::from(cycles);
        if self.divider_counter >= 255 {
            self.divider_counter = 0;
            let (val, _) = Cpu::add_8bit(self.mem.read(DIV), 1);
            self.mem.write(DIV, val);
        }

        let is_clock_enabled = ith_bit(self.mem.read(TMC), 2);
        if is_clock_enabled {
            self.timer_counter -= i64::from(cycles);

            if self.timer_counter <= 0 {
                self.timer_counter = match self.mem.read(TMA) {
                    0 => 1024,
                    1 => 16,
                    2 => 64,
                    3 => 256,
                    _ => unreachable!(),
                };

                if self.mem.read(TIMA) > 0 {
                    self.mem.write(TIMA, self.mem.read(TMA));
                    self.request_interrupt(Interrupt::Timer);
                } else {
                    let (val, _) = Cpu::add_8bit(self.mem.read(TIMA), 1);
                    self.mem.write(TIMA, val);
                }
            }
        }
    }

    fn update_graphics(&mut self, cycles: u8) {
        let status = self.mem.read(STAT);
        let control = self.mem.read(LCDC);
        let is_enabled = ith_bit(control, LcdControl::Enable as u8);

        let set_mode_bits = |status, mode| (status & 0b1111_1100) | mode as u8;

        if is_enabled {
            // set the mode to 1 during lcd disabled and reset scan line
            self.scan_counter = SCANLINE_CPU_CYCLES;
            self.mem.write(LY, 0);
            let default_status = set_mode_bits(status, Lcd::VBlank);
            self.mem.write(STAT, default_status);
            return;
        }

        let line = self.mem.read(LY);
        let mode = Lcd::from(status & 0b0000_0011);

        let (new_mode, should_interrupt) = if line >= 144 {
            (Lcd::VBlank, ith_bit(status, 4))
        } else if self.scan_counter >= 456 - 80 {
            (Lcd::SearchingRam, ith_bit(status, 5))
        } else if self.scan_counter >= 456 - 80 - 172 {
            (Lcd::TransferringDataToDriver, false)
        } else {
            (Lcd::HBlank, ith_bit(status, 3))
        };

        if new_mode != mode && should_interrupt {
            self.request_interrupt(Interrupt::LCD);
        }

        // check the conincidence flag
        let new_status = if line == self.mem.read(LYC) {
            if ith_bit(status, 6) {
                self.request_interrupt(Interrupt::LCD);
            }
            status | (1 << 2)
        } else {
            status & !(1 << 2)
        };

        self.mem.write(STAT, new_status);

        self.scan_counter -= i64::from(cycles);

        if self.scan_counter <= 0 {
            self.mem.write(LY, line + 1);

            self.scan_counter = 456;

            if line < 144 {
                if ith_bit(control, LcdControl::BackgroundEnable as u8) {
                    self.render_tiles();
                }

                if ith_bit(control, LcdControl::ObjectEnable as u8) {
                    self.render_sprites();
                }
            } else if line == 144 {
                self.request_interrupt(Interrupt::VBlank);
            } else if line == 154 {
                self.mem.write(LY, 0);
            }
        }
    }

    fn render_tiles(&mut self) {
        // where to draw the visual area and the window
        let (scroll_x, scroll_y) = (self.mem.read(SCX), self.mem.read(SCY));
        let (window_x, window_y) = (self.mem.read(WX), self.mem.read(WY));

        // is the window enabled?
        let line = self.mem.read(LY);
        let control = self.mem.read(LCDC);

        // which tile data are we using?
        let tile_data_address: u16 = if ith_bit(
            control,
            LcdControl::BackgroundAndWindodwTileDataSelect as u8,
        ) {
            0x8000
        } else {
            0x8800
        };

        let is_window_enabled = ith_bit(control, LcdControl::WindowDisplayEnable as u8);
        let is_window_visible = is_window_enabled && window_y <= line;

        let map_select = if is_window_visible {
            LcdControl::WindowTileMapSelect
        } else {
            LcdControl::BackgroundTileMapSelect
        };
        let bg_address: u16 = if ith_bit(control, map_select as u8) {
            0x9C00
        } else {
            0x9800
        };

        let y_pos = if is_window_visible {
            line + scroll_y
        } else {
            line - window_y
        };

        // which of the 8 vertical pixels of the current tile is the scanline on?
        let row = (y_pos / 8) * 32;

        // time to start drawing the 160 horizontal pixels // for this scanline
        for pixel in 0..160 {
            // translate the current x pos to window space if necessary
            let x_pos = if is_window_visible && pixel >= window_x {
                pixel - window_x
            } else {
                pixel + scroll_x
            };

            // which of the 32 horizontal tiles does this xPos fall within?
            let col = x_pos / 8;

            // get the tile identity number. Remember it can be signed
            // or unsigned
            let tile_num = self.mem.read(bg_address + u16::from(row) + u16::from(col));

            // deduce where this tile identifier is in memory. Remember i shown this algorithm earlier
            let tile_location = if tile_data_address == 0x8800 {
                tile_data_address + u16::from(tile_num) * 16
            } else {
                (i32::from(tile_data_address) + (i32::from(tile_num) + 128) * 16) as u16
            };

            // find the correct vertical line we're on of the tile to get the tile data from in memory
            // each vertical line takes up two bytes of memory
            let object_y = 2 * (y_pos % 8);
            let (data2, data1) =
                little_endian::u8(self.mem.read_word(tile_location + u16::from(object_y)));

            let colour_bit_num = 1 << (x_pos & 0b0111);
            let color_num = ((ith_bit(data2, colour_bit_num) as u8) << 1)
                | ith_bit(data1, colour_bit_num) as u8;

            let c = self.get_color(color_num, BGP);
            self.display[line as usize][pixel as usize] = c;
        }
    }

    fn get_color(&self, colour_id: u8, address: u16) -> u8 {
        let palette = self.mem.read(address);
        let colour_bits = (palette >> colour_id) & 0b11;
        match colour_bits {
            3 => BLACK,
            2 => DARK_GRAY,
            1 => LIGHT_GRAY,
            0 => WHITE,
            _ => unreachable!()
        }
    }

    fn render_sprites(&mut self) {
        let control = self.mem.read(LCDC);

        let y_size = if ith_bit(control, LcdControl::ObjectSize as u8) { 16 } else { 8 };

        // sprite occupies 4 bytes in the sprite attributes table
        const NUM_SPRITES: u16 = 40;
        for i in (0..4 * NUM_SPRITES as u16).step_by(4) {
            let (x_pos, y_pos) = (self.mem.read(OAM + i + 1) - 8, self.mem.read(OAM + i) - 16);
            let (tile_location, attribute) = (self.mem.read(OAM + i + 2), self.mem.read(OAM + i + 3));
            let (x_flip, y_flip) = (ith_bit(attribute, 5), ith_bit(attribute, 6));

            let scan_line = self.mem.read(LY);

            let does_sprite_intercept_line = scan_line >= y_pos && (scan_line < (y_pos + y_size));
            if does_sprite_intercept_line {
                let line = 2 * if y_flip { -(scan_line as i16 - y_pos as i16 - y_size as i16) } else { scan_line as i16 - y_pos as i16 };

                let address = ((0x8000 + (tile_location as i32 * 16)) + line as i32) as u16;
                let (data2, data1) = little_endian::u8(self.mem.read_word(address));

                for pixel in 0..=7 {
                    let pixel_bit = if x_flip { pixel } else { 7 - pixel };

                    let color_num = ((ith_bit(data2, pixel_bit) as u8) << 1)
                        | ith_bit(data1, pixel_bit) as u8;

                    let color_address = if ith_bit(attribute, 4) { OBP1 } else { OBP0 };

                    let c = self.get_color(color_num, color_address);

                    // white is transparent
                    if c != WHITE {
                        self.display[line as usize][x_pos as usize + pixel as usize] = c;
                    }
                }
            }
        }
    }

    fn request_interrupt(&mut self, interrupt: Interrupt) {
        let val = self.mem.read(INTERRUPT_REQUEST_REGISTER);
        self.mem
            .write(INTERRUPT_REQUEST_REGISTER, val & (1 << interrupt as u8));
    }

    fn do_interrupts(&mut self) {
        let is_requested = self.mem.read(INTERRUPT_REQUEST_REGISTER);
        let is_enabled = self.mem.read(INTERRUPT_ENABLE_REGISTER);
        let is_requested_and_enabled = is_requested & is_enabled;

        for i in 0..5 {
            if ith_bit(is_requested_and_enabled, i) && self.are_interrupts_enabled {
                self.are_interrupts_enabled = false;
                let not_requested_anymore = is_requested & !(1 << i);
                self.mem
                    .write(INTERRUPT_REQUEST_REGISTER, not_requested_anymore);

                self.sp -= 2;
                self.mem.write_word(self.sp, self.pc);

                self.pc = match Interrupt::from(i) {
                    Interrupt::VBlank => 0x40,
                    Interrupt::LCD => 0x48,
                    Interrupt::Timer => 0x50,
                    Interrupt::SerialTxComplete => 0x58,
                    Interrupt::Joypad => 0x60,
                };
            }
        }
    }

    pub fn cycle(&mut self, s: f64) {
        if self.is_halted {
            return;
        }

        let total_cycles = (s * CLOCK_FREQUENCY).round() as u64;
        let mut cur_num_cycles = 0;
        while cur_num_cycles < total_cycles {
            let (instruction, pc_increments) = self.fetch_instruction();

            #[cfg(debug_assertions)]
                let v = self.mem.raw_memory[self.pc as usize..self.pc as usize + pc_increments as usize].to_vec();
            let tabs = match v.len() {
                1 => "\t\t\t",
                2 => "\t\t",
                3 => "\t",
                _ => unreachable!()
            };
            println!("PC: {:4x?}, SP: {:4x?}, bytes: {:2x?}, {} regs: {:2x?}, \t {:08b}, \t {:x?}", self.pc, self.sp, v, tabs, self.reg, self.reg[Register::F as usize], instruction);

            self.pc += pc_increments;
            let cycles = self.execute_instruction(instruction);
            cur_num_cycles += u64::from(cycles);
            self.update_timers(cycles);
            self.update_graphics(cycles);
            self.do_interrupts();
        }
    }

    fn write_word(&mut self, r: Register16bit, val: u16) {
        self.reg[r as usize] = little_endian::msb(val);
        self.reg[r as usize + 1] = little_endian::lsb(val);
    }

    fn read_word(&self, r: Register16bit) -> u16 {
        little_endian::u16(self.reg[r as usize + 1], self.reg[r as usize])
    }

    fn write(&mut self, r: Register, val: u8) {
        assert_ne!(r, Register::F);
        self.reg[r as usize] = val;
    }

    fn read(&self, r: Register) -> u8 {
        assert_ne!(r, Register::F);
        self.reg[r as usize]
    }

    fn set_flag(&mut self, flag: ConditionalFlag, state: bool) {
        let val = if state {
            self.reg[Register::F as usize] | (1 << flag as u8)
        } else {
            self.reg[Register::F as usize] & !(1 << flag as u8)
        };
        self.reg[Register::F as usize] = val;
    }


    fn get_flag(&self, flag: ConditionalFlag) -> bool {
        ith_bit(self.reg[Register::F as usize], flag as u8)
    }

    fn set_flags(&mut self, flags: Flags) {
        self.set_flag(ConditionalFlag::Zero, flags.z);
        self.set_flag(ConditionalFlag::Subtract, flags.n);
        self.set_flag(ConditionalFlag::HalfCarry, flags.h);
        self.set_flag(ConditionalFlag::Carry, flags.c);
    }

    fn add_16bit(a: u16, b: u16) -> (u16, Flags) {
        let res = a as u32 + b as u32;
        (
            res as u16,
            Flags {
                z: (res as u16) == 0,
                n: false,
                h: ((a as u32 ^ b as u32 ^ res) & 0x1000) == 0x1000,
                c: res > 0xFFFF,
            },
        )
    }

    fn sub_16bit(a: u16, b: u16) -> (u16, Flags) {
        let res = a as i32 - b as i32;
        (
            res as u16,
            Flags {
                z: (res as u16) == 0,
                n: true,
                h: ((a as i32 ^ -(b as i32) ^ res) & 0x1000) == 0x1000,
                c: res < 0,
            },
        )
    }

    fn add_8bit(a: u8, b: u8) -> (u8, Flags) {
        let res = a as u16 + b as u16;
        (
            res as u8,
            Flags {
                z: (res as u8) == 0,
                n: false,
                h: ((a as u16 ^ b as u16 ^ res) & 0x0010) == 0x0010,
                c: res > 0xFF,
            },
        )
    }

    fn sub_8bit(a: u8, b: u8) -> (u8, Flags) {
        let res = a as i16 - b as i16;
        (
            res as u8,
            Flags {
                z: (res as u8) == 0,
                n: true,
                h: ((a as i16 ^ -(b as i16) ^ res) & 0x0010) == 0x0010,
                c: res < 0,
            },
        )
    }

    fn execute_instruction(&mut self, instr: Instruction) -> u8 {
        let rotate_left = |x| ((x << 1) | ith_bit(x, 7) as u8, ith_bit(x, 7));
        let rotate_left_carry = |x| ((x << 1) | self.get_flag(ConditionalFlag::Carry) as u8, ith_bit(x, 7));

        let rotate_right = |x| ((x >> 1) | ((ith_bit(x, 0) as u8) << 7), ith_bit(x, 0), );
        let rotate_right_carry = |x| ((x >> 1) | ((self.get_flag(ConditionalFlag::Carry) as u8) << 7), ith_bit(x, 0), );

        match instr {
            // GMB 8bit-Load commands
            // ld   r,r         xx         4 ---- r=r
            LoadReg { to, from } => {
                self.write(to, self.read(from));
                4
            }
            // ld   r,n         xx nn      8 ---- r=n
            LoadRegWithConstant { to, n } => {
                self.write(to, n);
                8
            }
            // ld   r,(HL)      xx         8 ---- r=(HL)
            LoadRegWithMemoryHL { to } => {
                let address = self.read_word(Register16bit::HL);
                self.write(to, self.mem.read(address));
                8
            }
            // ld   (HL),r      7x         8 ---- (HL)=r
            LoadMemoryHLwithRegister { from } => {
                let address = self.read_word(Register16bit::HL);
                self.mem.write(address, self.read(from));
                8
            }
            // ld   (HL),n      36 nn     12 ----
            LoadMemoryHLwithConstant { n } => {
                let address = self.read_word(Register16bit::HL);
                self.mem.write(address, n);
                12
            }
            // ld   A,(BC)      0A         8 ----
            LoadAwithValBC => {
                let address = self.read_word(Register16bit::BC);
                self.write(Register::A, self.mem.read(address));
                8
            }
            // ld   A,(DE)      1A         8 ----
            LoadAwithValDE => {
                let address = self.read_word(Register16bit::DE);
                self.write(Register::A, self.mem.read(address));
                8
            }
            // ld   A,(nn)      FA        16 ----
            LoadAwithMemory { nn } => {
                assert!(nn < 0xFFFF);
                self.write(Register::A, self.mem.read(nn));
                16
            }
            // ld   (BC),A      02         8 ----
            LoadMemoryBCwithA => {
                let address = self.read_word(Register16bit::BC);
                self.mem.write(address, self.read(Register::A));
                8
            }
            // ld   (DE),A      12         8 ----
            LoadMemoryDEwithA => {
                let address = self.read_word(Register16bit::DE);
                self.mem.write(address, self.read(Register::A));
                8
            }
            // ld   (nn),A      EA        16 ----
            LoadMemoryNNwithA { nn } => {
                self.mem.write(nn, self.read(Register::A));
                16
            }
            // ld   A,(FF00+n)  F0 nn     12 ---- read from io-port n (memory FF00+n)
            LoadAwithFF00plusN { n } => {
                self.write(Register::A, self.mem.read(u16::from(n) + 0xFF00));
                12
            }
            // ld   (FF00+n),A  E0 nn     12 ---- write to io-port n (memory FF00+n)
            LoadMemoryFF00plusNwithA { nn } => {
                self.mem
                    .write(u16::from(nn) + 0xFF00, self.read(Register::A));
                12
            }
            // ld   A,(FF00+C)  F2         8 ---- read from io-port C (memory FF00+C)
            LoadAwithFF00plusC => {
                let address = 0xFF00 + u16::from(self.read(Register::C));
                self.write(Register::A, self.mem.read(address));
                8
            }
            // ld   (FF00+C),A  E2         8 ---- write to io-port C (memory FF00+C)
            LoadMemoryFF00plusCwithA => {
                let address = 0xFF00 + u16::from(self.read(Register::C));
                self.mem.write(address, self.read(Register::A));

                8
            }
            // ldi  (HL),A      22         8 ---- (HL)=A, HL=HL+1
            LoadMemoryHLwithAandIncr => {
                let address = self.read_word(Register16bit::HL);
                self.mem.write(address, self.read(Register::A));

                self.write_word(Register16bit::HL, address + 1);
                8
            }
            // ldi  A,(HL)      2A         8 ---- A=(HL), HL=HL+1
            LoadAwithValHLandIncr => {
                let address = self.read_word(Register16bit::HL);
                let val = self.mem.read(address);

                self.write(Register::A, val);

                self.write_word(Register16bit::HL, address + 1);
                8
            }
            // ldd  (HL),A      32         8 ---- (HL)=A, HL=HL-1
            LoadMemoryHLwithAandDecr => {
                let address = self.read_word(Register16bit::HL);
                let val = self.read(Register::A);

                self.mem.write(address, val);

                self.write_word(Register16bit::HL, address - 1);
                8
            }
            // ldd  A,(HL)      3A         8 ---- A=(HL), HL=HL-1
            LoadAwithValHLandDecr => {
                let address = self.read_word(Register16bit::HL);
                let val = self.mem.read(address);

                self.write(Register::A, val);

                self.write_word(Register16bit::HL, address - 1);
                8
            }

            // GMB 16bit-Load Commands
            // ld   rr,nn       x1 nn nn  12 ---- rr=nn (rr may be BC,DE,HL or SP)
            LoadRegWith16BitConstant { rr, nn } => {
                self.write_word(rr, nn);

                12
            }
            LoadSPWith16BitConstant { nn } => {
                self.sp = nn;

                12
            }
            // ld   SP,HL       F9         8 ---- SP=HL
            LoadSPwithHL => {
                self.sp = self.read_word(Register16bit::HL);
                8
            }
            // push rr          x5 (c5, d5, e5, f5)        16 ---- SP=SP-2  (SP)=rr   (rr may be BC,DE,HL,AF)
            Push { rr } => {
                self.sp -= 2;
                let val = self.read_word(rr);
                self.mem.write_word(self.sp, val);

                16
            }
            // pop  rr          x1 (c1, d1, e1, f1)      12 (AF) rr=(SP)  SP=SP+2   (rr may be BC,DE,HL,AF)
            Pop { rr } => {
                let val = self.mem.read_word(self.sp);
                self.write_word(rr, val);
                self.sp += 2;
                12
            }

            // GMB 8bit-Arithmetic/logical Commands
            //  add  A,r         8x         4 z0hc A=A+r
            AddRegister { r } => {
                let (val, f) = Cpu::add_8bit(self.read(Register::A), self.read(r));

                self.write(Register::A, val);
                self.set_flags(f);
                4
            }
            //  add  A,n         C6 nn      8 z0hc A=A+n
            AddConstant { n } => {
                let (val, f) = Cpu::add_8bit(self.read(Register::A), n);

                self.write(Register::A, val);
                self.set_flags(f);
                8
            }
            //  add  A,(HL)      86         8 z0hc A=A+(HL)
            AddMemoryHL => {
                let address = self.read_word(Register16bit::HL);
                let (val, f) =
                    Cpu::add_8bit(self.read(Register::A), self.mem.read(address));

                self.write(Register::A, val);
                self.set_flags(f);
                8
            }
            //  adc  A,r         8x         4 z0hc A=A+r+cy
            AddRegisterWithCarry { r } => {
                let b = self.read(r) + self.get_flag(ConditionalFlag::Carry) as u8;
                let (val, f) = Cpu::add_8bit(self.read(Register::A), b);

                self.write(Register::A, val);
                self.set_flags(f);
                4
            }
            //  adc  A,n         CE nn      8 z0hc A=A+n+cy
            AddConstantWithCarry { n } => {
                let b = n + self.get_flag(ConditionalFlag::Carry) as u8;
                let (val, f) = Cpu::add_8bit(self.read(Register::A), b);

                self.write(Register::A, val);
                self.set_flags(f);
                8
            }
            //  adc  A,(HL)      8E         8 z0hc A=A+(HL)+cy
            AddMemoryHLWithCarry => {
                let address = self.read_word(Register16bit::HL);
                let b = self.mem.read(address) + self.get_flag(ConditionalFlag::Carry) as u8;
                let (val, f) = Cpu::add_8bit(self.read(Register::A), b);

                self.write(Register::A, val);
                self.set_flags(f);
                8
            }
            //  sub  r           9x         4 z1hc A=A-r
            SubtractRegister { r } => {
                let val = self.read(r);
                let (val, f) = Cpu::sub_8bit(self.read(Register::A), val);
                self.write(Register::A, val);
                self.set_flags(f);
                4
            }
            //  sub  n           D6 nn      8 z1hc A=A-n
            SubtractConstant { n } => {
                let (val, f) = Cpu::sub_8bit(self.read(Register::A), n);
                self.write(Register::A, val);
                self.set_flags(f);
                8
            }
            //  sub  (HL)        96         8 z1hc A=A-(HL)
            SubtractMemoryHL => {
                let address = self.read_word(Register16bit::HL);
                let val = self.mem.read(address);
                let (val, f) = Cpu::sub_8bit(self.read(Register::A), val);
                self.write(Register::A, val);
                self.set_flags(f);
                8
            }
            //  sbc  A,r         9x         4 z1hc A=A-r-cy
            SubtractRegisterWithCarry { r } => {
                let val = self.read(r) - self.get_flag(ConditionalFlag::Carry) as u8;
                let (val, f) = Cpu::sub_8bit(self.read(Register::A), val);
                self.write(Register::A, val);
                self.set_flags(f);
                4
            }
            //  sbc  A,n         DE nn      8 z1hc A=A-n-cy
            SubtractConstantWithCarry { n } => {
                let val = n - self.get_flag(ConditionalFlag::Carry) as u8;
                let (val, f) = Cpu::sub_8bit(self.read(Register::A), val);
                self.write(Register::A, val);
                self.set_flags(f);
                4
            }
            //  sbc  A,(HL)      9E         8 z1hc A=A-(HL)-cy
            SubtractMemoryHLWithCarry => {
                let address = self.read_word(Register16bit::HL);
                let val = self.mem.read(address) - self.get_flag(ConditionalFlag::Carry) as u8;
                let (val, f) = Cpu::sub_8bit(self.read(Register::A), val);

                self.write(Register::A, val);
                self.set_flags(f);
                8
            }
            //  and  r           Ax         4 z010 A=A & r
            AndRegister { r } => {
                self.write(Register::A, self.read(Register::A) & self.read(r));
                self.set_flags(Flags {
                    z: self.read(Register::A) == 0,
                    n: false,
                    h: true,
                    c: false,
                });
                4
            }
            //  and  n           E6 nn      8 z010 A=A & n
            AndConstant { n } => {
                self.write(Register::A, self.read(Register::A) & n);
                self.set_flags(Flags {
                    z: self.read(Register::A) == 0,
                    n: false,
                    h: true,
                    c: false,
                });
                8
            }
            //  and  (HL)        A6         8 z010 A=A & (HL)
            AndMemoryHL => {
                let address = self.read_word(Register16bit::HL);
                self.write(Register::A, self.read(Register::A) & self.mem.read(address));
                self.set_flags(Flags {
                    z: self.read(Register::A) == 0,
                    n: false,
                    h: true,
                    c: false,
                });
                8
            }
            //  xor  r           Ax         4 z000
            XorRegister { r } => {
                self.write(Register::A, self.read(Register::A) ^ self.read(r));
                self.set_flags(Flags {
                    z: self.read(Register::A) == 0,
                    n: false,
                    h: false,
                    c: false,
                });
                4
            }
            //  xor  n           EE nn      8 z000
            XorConstant { n } => {
                self.write(Register::A, self.read(Register::A) ^ n);
                self.set_flags(Flags {
                    z: self.read(Register::A) == 0,
                    n: false,
                    h: false,
                    c: false,
                });
                8
            }
            //  xor  (HL)        AE         8 z000
            XorMemoryHL => {
                let address = self.read_word(Register16bit::HL);
                self.write(Register::A, self.read(Register::A) ^ self.mem.read(address));
                self.set_flags(Flags {
                    z: self.read(Register::A) == 0,
                    n: false,
                    h: false,
                    c: false,
                });
                8
            }
            //  or   r           Bx         4 z000 A=A | r
            OrRegister { r } => {
                self.write(Register::A, self.read(Register::A) | self.read(r));
                self.set_flags(Flags {
                    z: self.read(Register::A) == 0,
                    n: false,
                    h: false,
                    c: false,
                });
                8
            }
            //  or   n           F6 nn      8 z000 A=A | n
            OrConstant { n } => {
                self.write(Register::A, self.read(Register::A) | n);
                self.set_flags(Flags {
                    z: self.read(Register::A) == 0,
                    n: false,
                    h: false,
                    c: false,
                });
                8
            }
            //  or   (HL)        B6         8 z000 A=A | (HL)
            OrMemoryHL => {
                let address = self.read_word(Register16bit::HL);
                self.write(Register::A, self.read(Register::A) | self.mem.read(address));
                self.set_flags(Flags {
                    z: self.read(Register::A) == 0,
                    n: false,
                    h: false,
                    c: false,
                });
                8
            }
            //  cp   r           Bx         4 z1hc compare A-r
            CompareRegister { r } => {
                let (_, f) = Cpu::sub_8bit(self.read(Register::A), self.read(r));
                self.set_flags(f);
                4
            }
            //  cp   n           FE nn      8 z1hc compare A-n
            CompareConstant { n } => {
                let (_, f) = Cpu::sub_8bit(self.read(Register::A), n);
                self.set_flags(f);
                8
            }
            //  cp   (HL)        BE         8 z1hc compare A-(HL)
            CompareMemoryHL => {
                let address = self.read_word(Register16bit::HL);
                let val = self.mem.read(address);
                let (_, f) = Cpu::sub_8bit(self.read(Register::A), val);

                self.set_flags(f);
                8
            }
            //  inc  r           xx         4 z0h- r=r+1
            IncrementRegister { r } => {
                let val = self.read(r);
                let (val, f) = Cpu::add_8bit(val, 1);

                self.write(r, val);
                self.set_flags(Flags {
                    z: f.z,
                    n: false,
                    h: f.h,
                    c: self.get_flag(ConditionalFlag::Carry),
                });
                4
            }
            //  inc  (HL)        34        12 z0h- (HL)=(HL)+1
            IncrementMemoryHL => {
                let address = self.read_word(Register16bit::HL);
                let val = self.mem.read(address);
                let (val, f) = Cpu::add_8bit(val, 1);

                self.mem.write(address, val);
                self.set_flags(Flags {
                    z: f.z,
                    n: false,
                    h: f.h,
                    c: self.get_flag(ConditionalFlag::Carry),
                });
                12
            }
            //  dec  r           xx         4 z1h- r=r-1
            DecrementRegister { r } => {
                let val = self.read(r);
                let (val, f) = Cpu::sub_8bit(val, 1);

                self.write(r, val);
                self.set_flags(Flags {
                    z: f.z,
                    n: true,
                    h: f.h,
                    c: self.get_flag(ConditionalFlag::Carry),
                });
                4
            }
            //  dec  (HL)        35        12 z1h- (HL)=(HL)-1
            DecrementMemoryHL => {
                let address = self.read_word(Register16bit::HL);
                let val = self.mem.read(address);
                let (val, f) = Cpu::sub_8bit(val, 1);

                self.mem.write(address, val);
                self.set_flags(Flags {
                    z: f.z,
                    n: true,
                    h: f.h,
                    c: self.get_flag(ConditionalFlag::Carry),
                });
                12
            }
            //  daa              27         4 z-0x decimal adjust akku
            DecimalAdjust => {
                // The BCD Flags (N, H)
                // These flags are (rarely) used for the DAA instruction only, N Indicates whether the previous instruction has been an addition or subtraction, and H indicates carry for lower 4bits of the result, also for DAA, the C flag must indicate carry for upper 8bits.
                // After adding/subtracting two BCD numbers, DAA is intended to convert the result into BCD format; BCD numbers are ranged from 00h to 99h rather than 00h to FFh.
                // Because C and H flags must contain carry-outs for each digit, DAA cannot be used for 16bit operations (which have 4 digits), or for INC/DEC operations (which do not affect C-flag).

                // TODO: Do we need to set all flags?
                // note: assumes a is a uint8_t and wraps from 0xff to 0
                let val = self.read(Register::A);
                let s = self.get_flag(ConditionalFlag::Subtract);

                if !self.get_flag(ConditionalFlag::Subtract) {
                    // after an addition, adjust if (half-)carry occurred or if result is out of bounds
                    if self.get_flag(ConditionalFlag::Carry) || (val > 0x99) {
                        let (val, f) = Cpu::add_8bit(val, 0x60);
                        self.write(Register::A, val);
                        self.set_flags(f);
                    }
                    if self.get_flag(ConditionalFlag::HalfCarry) || (val & 0x0f) > 0x09 {
                        let (val, f) = Cpu::add_8bit(val, 0x6);
                        self.write(Register::A, val);
                        self.set_flags(f);
                    }
                } else {
                    // after a subtraction, only adjust if (half-)carry occurred
                    if self.get_flag(ConditionalFlag::Carry) {
                        let (val, f) = Cpu::sub_8bit(val, 0x60);
                        self.write(Register::A, val);
                        self.set_flags(f);
                    }
                    if self.get_flag(ConditionalFlag::HalfCarry) {
                        let (val, f) = Cpu::sub_8bit(val, 0x6);
                        self.write(Register::A, val);
                        self.set_flags(f);
                    }
                }

                self.set_flag(ConditionalFlag::HalfCarry, false);
                self.set_flag(ConditionalFlag::Subtract, s);
                4
            }
            //  cpl              2F         4 -11- A = A xor FF
            CPL => {
                // Complement all bits
                self.write(Register::A, self.read(Register::A) ^ 0xFF);
                self.set_flag(ConditionalFlag::Subtract, true);
                self.set_flag(ConditionalFlag::HalfCarry, true);
                4
            }

            //GMB 16bit-Arithmetic/logical Commands
            //add  HL,rr     x9           8 -0hc HL = HL+rr     ;rr may be BC,DE,HL,SP
            AddRegisterHL16bit { r } => {
                let address = self.read_word(Register16bit::HL);
                let (val, f) = Cpu::add_16bit(address, self.read_word(r));
                self.write_word(Register16bit::HL, val);
                self.set_flags(Flags {
                    z: self.get_flag(ConditionalFlag::Zero),
                    n: false,
                    h: f.h,
                    c: f.c,
                });
                8
            }
            AddRegisterSPtoHL16bit {} => {
                let address = self.read_word(Register16bit::HL);
                let (val, f) = Cpu::add_16bit(address, self.sp);
                self.write_word(Register16bit::HL, val);
                self.set_flags(Flags {
                    z: self.get_flag(ConditionalFlag::Zero),
                    n: false,
                    h: f.h,
                    c: f.c,
                });
                8
            }
            //inc  rr        x3           8 ---- rr = rr+1      ;rr may be BC,DE,HL,SP
            IncrementRegister16bit { r } => {
                let (val, _) = Cpu::add_16bit(self.read_word(r), 1);
                self.write_word(r, val);
                8
            }
            IncrementSP {} => {
                self.sp += 1;
                8
            }
            //dec  rr        xB           8 ---- rr = rr-1      ;rr may be BC,DE,HL,SP
            DecrementRegister16bit { r } => {
                let (val, _) = Cpu::sub_16bit(self.read_word(r), 1);
                self.write_word(Register16bit::HL, val);
                8
            }
            DecrementSP {} => {
                self.sp -= 1;
                8
            }
            //add  SP,dd     E8          16 00hc SP = SP +/- dd ;dd is 8bit signed number
            AddSP { d } => {
                let (val, f) = if d > 0 {
                    Cpu::add_16bit(self.sp, d as u16)
                } else {
                    Cpu::sub_16bit(self.sp, d.abs() as u16)
                };
                self.sp = val;
                self.set_flags(Flags {
                    z: false,
                    n: false,
                    h: f.h,
                    c: f.c,
                });
                16
            }
            //ld   HL,SP+dd  F8          12 00hc HL = SP +/- dd ;dd is 8bit signed number
            LoadHLwithSPplus { d } => {
                let (val, f) = if d > 0 {
                    Cpu::add_16bit(self.sp, d as u16)
                } else {
                    Cpu::sub_16bit(self.sp, d.abs() as u16)
                };
                self.write_word(Register16bit::HL, val);
                self.set_flags(Flags {
                    z: false,
                    n: false,
                    h: f.h,
                    c: f.c,
                });
                12
            }

            //GMB Rotate- und Shift-Commands
            //rlca           07           4 000c rotate akku left
            RotateLeft {} => {
                let (val, carry) = rotate_left(self.read(Register::A));
                self.write(Register::A, val);
                self.set_flags(Flags {
                    z: false,
                    n: false,
                    h: false,
                    c: carry,
                });
                4
            }
            //rla            17           4 000c rotate akku left through carry
            RotateLeftThroughCarry {} => {
                let (val, carry) = rotate_left_carry(self.read(Register::A));
                self.write(Register::A, val);
                self.set_flags(Flags {
                    z: false,
                    n: false,
                    h: false,
                    c: carry,
                });
                4
            }
            //rrca           0F           4 000c rotate akku right
            RotateRight => {
                let (val, carry) = rotate_right(self.read(Register::A));
                self.write(Register::A, val);
                self.set_flags(Flags {
                    z: false,
                    n: false,
                    h: false,
                    c: carry,
                });
                4
            }
            //rra            1F           4 000c rotate akku right through carry
            RotateRightThroughCarry => {
                let (val, carry) = rotate_right_carry(self.read(Register::A));
                self.write(Register::A, val);
                self.set_flags(Flags {
                    z: false,
                    n: false,
                    h: false,
                    c: carry,
                });
                4
            }
            //rlc  r         CB 0x        8 z00c rotate left
            RotateLeftRegisterThroughCarry { r } => {
                let (val, carry) = rotate_left(self.read(r));
                self.write(r, val);
                self.set_flags(Flags {
                    z: val == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                4
            }
            //rlc  (HL)      CB 06       16 z00c rotate left
            RotateLeftHLThroughCarry => {
                let address = self.read_word(Register16bit::HL);
                let (val, carry) = rotate_left_carry(self.mem.read(address));
                self.mem.write(address, val);
                self.set_flags(Flags {
                    z: val == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                4
            }
            //rrc  r         CB 0x        8 z00c rotate right
            RotateRightRegisterThroughCarry { r } => {
                let (val, carry) = rotate_right(self.read(r));
                self.write(r, val);
                self.set_flags(Flags {
                    z: val == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                8
            }
            //rrc  (HL)      CB 0E       16 z00c rotate right through carry
            RotateRightHLThroughCarry => {
                let address = self.read_word(Register16bit::HL);
                let (val, carry) = rotate_right_carry(self.mem.read(address));
                self.mem.write(address, val);
                self.set_flags(Flags {
                    z: val == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                8
            }
            //rl   r         CB 1x        8 z00c rotate left
            RotateLeftRegister { r } => {
                let (val, carry) = rotate_left(self.read(r));
                self.write(r, val);
                self.set_flags(Flags {
                    z: val == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                8
            }
            //rl   (HL)      CB 16       16 z00c rotate left
            RotateLeftHL => {
                let address = self.read_word(Register16bit::HL);
                let (val, carry) = rotate_left(self.mem.read(address));
                self.mem.write(address, val);
                self.set_flags(Flags {
                    z: val == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                16
            }
            //rr   r         CB 1x        8 z00c rotate right
            RotateRightRegister { r } => {
                let (val, carry) = rotate_right(self.read(r));
                self.write(r, val);
                self.set_flags(Flags {
                    z: val == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                4
            }
            //rr   (HL)      CB 1E       16 z00c rotate right
            RotateRightHL => {
                let address = self.read_word(Register16bit::HL);
                let val = self.mem.read(address);
                let (val, carry) = rotate_right(val);
                self.mem.write(address, val);
                self.set_flags(Flags {
                    z: val == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                16
            }
            //sla  r         CB 2x        8 z00c shift left arithmetic (b0=0)
            ShiftLeftRegister { r } => {
                let carry = ith_bit(self.read(r), 7);
                self.write(r, self.read(r) << 1);
                self.set_flags(Flags {
                    z: self.read(r) == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                8
            }
            //sla  (HL)      CB 26       16 z00c shift left arithmetic (b0=0)
            ShiftLeftHL => {
                let address = self.read_word(Register16bit::HL);
                let val = self.mem.read(address);
                let carry = ith_bit(val, 7);
                self.mem.write(address, val << 1);
                self.set_flags(Flags {
                    z: self.mem.read(address) == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                8
            }
            //sra  r         CB 2x        8 z00c shift right arithmetic (b7=b7)
            ShiftRightArithmeticRegister { r } => {
                let carry = ith_bit(self.read(r), 0);
                let sign_bit = ith_bit(self.read(r), 7);
                self.write(r, (self.read(r) >> 1) | ((sign_bit as u8) << 7));
                self.set_flags(Flags {
                    z: self.read(r) == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                8
            }
            //sra  (HL)      CB 2E       16 z00c shift right arithmetic (b7=b7)
            ShiftRightArithmeticHL => {
                let address = self.read_word(Register16bit::HL);
                let val = self.mem.read(address);
                let carry = ith_bit(val, 0);
                let sign_bit = ith_bit(val, 7);
                self.mem.write(address, (val >> 1) | ((sign_bit as u8) << 7));
                self.set_flags(Flags {
                    z: self.mem.read(address) == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                16
            }
            //swap r         CB 3x        8 z000 exchange low/hi-nibble
            SwapRegister { r } => {
                self.write(r, (self.read(r) << 4) | (self.read(r) >> 4));
                self.set_flags(Flags {
                    z: self.read(r) == 0,
                    n: false,
                    h: false,
                    c: false,
                });
                8
            }
            //swap (HL)      CB 36       16 z000 exchange low/hi-nibble
            SwapHL => {
                let address = self.read_word(Register16bit::HL);
                let val = self.mem.read(address);
                self.mem.write(address, (val << 4) | (val >> 4));
                self.set_flags(Flags {
                    z: self.mem.read(address) == 0,
                    n: false,
                    h: false,
                    c: false,
                });
                16
            }
            //srl  r         CB 3x        8 z00c shift right logical (b7=0)
            ShiftRightLogicalRegister { r } => {
                let carry = ith_bit(self.read(r), 0);
                self.write(r, self.read(r) >> 1);
                self.set_flags(Flags {
                    z: self.read(r) == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                8
            }
            //srl  (HL)      CB 3E       16 z00c shift right logical (b7=0)
            ShiftRightLogicalHL => {
                let address = self.read_word(Register16bit::HL);
                let val = self.mem.read(address);
                let carry = ith_bit(val, 0);
                self.mem.write(address, val >> 1);
                self.set_flags(Flags {
                    z: self.mem.read(address) == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                8
            }
            //GMB Singlebit Operation Commands
            //bit  n,r       CB xx        8 z01- test bit n
            TestBitRegister { bit, r } => {
                assert!(bit <= 7);
                self.set_flags(Flags {
                    z: !ith_bit(self.read(r), bit),
                    n: false,
                    h: true,
                    c: self.get_flag(ConditionalFlag::Carry),
                });
                8
            }
            //bit  n,(HL)    CB xx       12 z01- test bit n
            TestBitMemoryHL { bit } => {
                assert!(bit <= 7);
                let address = self.read_word(Register16bit::HL);
                self.set_flags(Flags {
                    z: !ith_bit(self.mem.read(address), bit),
                    n: false,
                    h: true,
                    c: self.get_flag(ConditionalFlag::Carry),
                });
                12
            }
            //set  n,r       CB xx        8 ---- set bit n
            SetBitRegister { bit, r } => {
                assert!(bit <= 7);
                self.write(r, self.read(r) | 1 << bit);
                assert!(ith_bit(self.read(r), bit));
                8
            }
            //set  n,(HL)    CB xx       16 ---- set bit n
            SetBitMemoryHL { bit } => {
                assert!(bit <= 7);
                let address = self.read_word(Register16bit::HL);
                let val = self.mem.read(address);
                self.mem.write(address, val | (1 << bit));
                assert!(ith_bit(self.mem.read(address), bit));
                16
            }
            //res  n,r       CB xx        8 ---- reset bit n
            ResetBitRegister { bit, r } => {
                assert!(bit <= 7);
                self.write(r, self.read(r) & !(1 << bit));
                assert!(!ith_bit(self.read(r), bit));
                8
            }
            //res  n,(HL)    CB xx       16 ---- reset bit n
            ResetBitMemoryHL { bit } => {
                assert!(bit <= 7);
                let address = self.read_word(Register16bit::HL);
                let val = self.mem.read(address);
                self.mem.write(address, val & !(1 << bit));
                assert!(!ith_bit(self.mem.read(address), bit));
                16
            }
            //GMB CPU-Control Commands
            //ccf            3F           4 -00c cy=cy xor 1
            CCF => {
                self.set_flags(Flags {
                    z: self.get_flag(ConditionalFlag::Zero),
                    n: false,
                    h: false,
                    c: !self.get_flag(ConditionalFlag::Carry),
                });
                4
            }
            //scf            37           4 -001 cy=1
            SCF => {
                self.set_flags(Flags {
                    z: self.get_flag(ConditionalFlag::Zero),
                    n: false,
                    h: false,
                    c: true,
                });
                4
            }
            //nop            00           4 ---- no operation
            NOP => 4,
            //halt           76         N*4 ---- halt until interrupt occurs (low power)
            HALT => {
                self.is_halted = true;
                4
            }
            //stop           10 00        ? ---- low power standby mode (VERY low power)
            STOP => {
                self.is_stopped = true;
                4
            }
            //di             F3           4 ---- disable interrupts, IME=0
            DI => {
                self.are_interrupts_enabled = false;
                4
            }
            //ei             FB           4 ---- enable interrupts, IME=1
            EI => {
                self.are_interrupts_enabled = true;
                4
            }
            //GMB Jump Commands
            //jp   nn        C3 nn nn    16 ---- jump to nn, PC=nn
            JP { nn } => {
                self.pc = nn;
                16
            }
            //jp   HL        E9           4 ---- jump to HL, PC=HL
            JPtoMemoryHL => {
                let address = self.read_word(Register16bit::HL);
                self.pc = address;
                4
            }
            //jp   f,nn      xx nn nn 16;12 ---- conditional jump if nz,z,nc,c
            CondJP { flag, check_state, nn } => {
                assert!(flag == ConditionalFlag::Zero || flag == ConditionalFlag::Carry);
                if self.get_flag(flag) == check_state {
                    self.pc = nn;
                    16
                } else {
                    12
                }
            }
            //jr   PC+dd     18 dd       12 ---- relative jump to nn (PC=PC+/-7bit)
            JRtoMemory { dd } => {
                self.pc = (i32::from(self.pc) + i32::from(dd)) as u16;
                12
            }
            //jr   f,PC+dd   xx dd     12;8 ---- conditional relative jump if nz,z,nc,c
            CondJR { flag, check_state, dd } => {
                assert!(flag == ConditionalFlag::Zero || flag == ConditionalFlag::Carry);
                if self.get_flag(flag) == check_state {
                    self.pc = (i32::from(self.pc) + i32::from(dd)) as u16;
                    12
                } else {
                    8
                }
            }
            //call nn        CD nn nn    24 ---- call to nn, SP=SP-2, (SP)=PC, PC=nn
            CALL { nn } => {
                self.sp -= 2;
                self.mem.write_word(self.sp, self.pc);
                self.pc = nn;
                24
            }
            //call f,nn      xx nn nn 24;12 ---- conditional call if nz,z,nc,c
            CondCALL { flag, check_state, nn } => {
                assert!(flag == ConditionalFlag::Zero || flag == ConditionalFlag::Carry);
                if self.get_flag(flag) == check_state {
                    self.sp -= 2;
                    self.mem.write_word(self.sp, self.pc);
                    self.pc = nn;
                    24
                } else {
                    12
                }
            }
            //ret            C9          16 ---- return, PC=(SP), SP=SP+2
            RET => {
                self.pc = self.mem.read_word(self.sp);
                self.sp += 2;
                16
            }
            //ret  f         xx        20;8 ---- conditional return if nz,z,nc,c
            CondRET { flag, check_state } => {
                assert!(flag == ConditionalFlag::Zero || flag == ConditionalFlag::Carry);
                if self.get_flag(flag) == check_state {
                    self.pc = self.mem.read_word(self.sp);
                    self.sp += 2;
                    20
                } else {
                    8
                }
            }
            //reti           D9          16 ---- return and enable interrupts (IME=1)
            RETI => {
                self.pc = self.mem.read_word(self.sp);
                self.sp += 2;
                self.are_interrupts_enabled = true;
                16
            }
            //rst  n         xx          16 ---- call to 00,08,10,18,20,28,30,38
            RST { n } => {
                self.sp -= 2;
                self.mem.write_word(self.sp, self.pc);
                self.pc = n;
                24
            }
            LoadAddressWithSP { n } => {
                self.mem.write_word(n, self.sp);
                20
            }
        }
    }

    // (instruction, pc increments)
    fn fetch_instruction(&self) -> (Instruction, u16) {
        let immediate_u8 = || self.mem.read(self.pc + 1);
        let immediate_i8 = || self.mem.read(self.pc + 1) as i8;
        let immediate_u16 = || self.mem.read_word(self.pc + 1);

        let op = self.mem.read(self.pc);

        match op {
            // GMB 8bit-Load commands
            // ld   r,r         xx         4 ---- r=r
            // ld   r,(HL)      xx         8 ---- r=(HL)
            0x40..=0x45 => (
                LoadReg {
                    to: Register::B,
                    from: Register::from(op & 0b0111),
                },
                1,
            ),
            0x46 => (LoadRegWithMemoryHL { to: Register::B }, 1),
            0x47 => (
                LoadReg {
                    to: Register::B,
                    from: Register::A,
                },
                1,
            ),
            0x48..=0x4D => (
                LoadReg {
                    to: Register::C,
                    from: Register::from(op & 0b0111),
                },
                1,
            ),
            0x4E => (LoadRegWithMemoryHL { to: Register::C }, 1),
            0x4F => (
                LoadReg {
                    to: Register::C,
                    from: Register::A,
                },
                1,
            ),
            0x50..=0x55 => (
                LoadReg {
                    to: Register::D,
                    from: Register::from(op & 0b0111),
                },
                1,
            ),
            0x56 => (LoadRegWithMemoryHL { to: Register::D }, 1),
            0x57 => (
                LoadReg {
                    to: Register::D,
                    from: Register::A,
                },
                1,
            ),
            0x58..=0x5D => (
                LoadReg {
                    to: Register::E,
                    from: Register::from(op & 0b0111),
                },
                1,
            ),
            0x5E => (LoadRegWithMemoryHL { to: Register::E }, 1),
            0x5F => (
                LoadReg {
                    to: Register::E,
                    from: Register::A,
                },
                1,
            ),
            0x60..=0x65 => (
                LoadReg {
                    to: Register::H,
                    from: Register::from(op & 0b0111),
                },
                1,
            ),
            0x66 => (LoadRegWithMemoryHL { to: Register::H }, 1),
            0x67 => (
                LoadReg {
                    to: Register::H,
                    from: Register::A,
                },
                1,
            ),
            0x68..=0x6D => (
                LoadReg {
                    to: Register::L,
                    from: Register::from(op & 0b0111),
                },
                1,
            ),
            0x6E => (LoadRegWithMemoryHL { to: Register::L }, 1),
            0x6F => (
                LoadReg {
                    to: Register::L,
                    from: Register::A,
                },
                1,
            ),
            0x78..=0x7D => (
                LoadReg {
                    to: Register::A,
                    from: Register::from(op & 0b0111),
                },
                1,
            ),
            0x7E => (LoadRegWithMemoryHL { to: Register::A }, 1),
            0x7F => (
                LoadReg {
                    to: Register::A,
                    from: Register::A,
                },
                1,
            ),

            // ld   r,n         xx nn      8 ---- r=n
            0x06 => (
                LoadRegWithConstant {
                    to: Register::B,
                    n: immediate_u8(),
                },
                2,
            ),
            0x0E => (
                LoadRegWithConstant {
                    to: Register::C,
                    n: immediate_u8(),
                },
                2,
            ),
            0x16 => (
                LoadRegWithConstant {
                    to: Register::D,
                    n: immediate_u8(),
                },
                2,
            ),
            0x1E => (
                LoadRegWithConstant {
                    to: Register::E,
                    n: immediate_u8(),
                },
                2,
            ),
            0x26 => (
                LoadRegWithConstant {
                    to: Register::H,
                    n: immediate_u8(),
                },
                2,
            ),
            0x2E => (
                LoadRegWithConstant {
                    to: Register::L,
                    n: immediate_u8(),
                },
                2,
            ),
            0x3E => (
                LoadRegWithConstant {
                    to: Register::A,
                    n: immediate_u8(),
                },
                2,
            ),

            // ld   (HL),r      7x         8 ---- (HL)=r
            0x70 => (LoadMemoryHLwithRegister { from: Register::B }, 1),
            0x71 => (LoadMemoryHLwithRegister { from: Register::C }, 1),
            0x72 => (LoadMemoryHLwithRegister { from: Register::D }, 1),
            0x73 => (LoadMemoryHLwithRegister { from: Register::E }, 1),
            0x74 => (LoadMemoryHLwithRegister { from: Register::H }, 1),
            0x75 => (LoadMemoryHLwithRegister { from: Register::L }, 1),
            0x77 => (LoadMemoryHLwithRegister { from: Register::A }, 1),

            // ld   (HL),n      36 nn     12 ----
            0x36 => (LoadMemoryHLwithConstant { n: immediate_u8() }, 2),
            // ld   A,(BC)      0A         8 ----
            0x0A => (LoadAwithValBC, 1),
            // ld   A,(DE)      1A         8 ----
            0x1A => (LoadAwithValDE, 1),
            // ld   A,(nn)      FA        16 ----
            0xFA => (
                LoadAwithMemory {
                    nn: immediate_u16(),
                },
                3,
            ),
            // ld   (BC),A      02         8 ----
            0x02 => (LoadMemoryBCwithA, 1),
            // ld   (DE),A      12         8 ----
            0x12 => (LoadMemoryDEwithA, 1),
            // ld   (nn),A      EA        16 ----
            0xEA => (
                LoadMemoryNNwithA {
                    nn: immediate_u16(),
                },
                3,
            ),
            // ld   A,(FF00+n)  F0 nn     12 ---- read from io-port n (memory FF00+n)
            0xF0 => (LoadAwithFF00plusN { n: immediate_u8() }, 2),
            // ld   (FF00+n),A  E0 nn     12 ---- write to io-port n (memory FF00+n)
            0xE0 => (LoadMemoryFF00plusNwithA { nn: immediate_u8() }, 2),
            // ld   A,(FF00+C)  F2         8 ---- read from io-port C (memory FF00+C)
            0xF2 => (LoadAwithFF00plusC, 1),
            // ld   (FF00+C),A  E2         8 ---- write to io-port C (memory FF00+C)
            0xE2 => (LoadMemoryFF00plusCwithA, 1),
            // ldi  (HL),A      22         8 ---- (HL)=A, HL=HL+1
            0x22 => (LoadMemoryHLwithAandIncr, 1),
            // ldi  A,(HL)      2A         8 ---- A=(HL), HL=HL+1
            0x2A => (LoadAwithValHLandIncr, 1),
            // ldd  (HL),A      32         8 ---- (HL)=A, HL=HL-1
            0x32 => (LoadMemoryHLwithAandDecr, 1),
            // ldd  A,(HL)      3A         8 ---- A=(HL), HL=HL-1
            0x3A => (LoadAwithValHLandDecr, 1),

            // GMB 16bit-Load Commands
            // ld   rr,nn       x1 nn nn  12 ---- rr=nn (rr may be BC,DE,HL or SP)
            0x01 => (
                LoadRegWith16BitConstant {
                    rr: Register16bit::BC,
                    nn: immediate_u16(),
                },
                3,
            ),
            0x11 => (
                LoadRegWith16BitConstant {
                    rr: Register16bit::DE,
                    nn: immediate_u16(),
                },
                3,
            ),
            0x21 => (
                LoadRegWith16BitConstant {
                    rr: Register16bit::HL,
                    nn: immediate_u16(),
                },
                3,
            ),
            0x31 => (
                LoadSPWith16BitConstant {
                    nn: immediate_u16(),
                },
                3,
            ),
            // ld   SP,HL       F9         8 ---- SP=HL
            0xF9 => (LoadSPwithHL, 1),
            // push rr          x5 (c5, d5, e5, f5)        16 ---- SP=SP-2  (SP)=rr   (rr may be BC,DE,HL,AF)
            0xc5 => (
                Push {
                    rr: Register16bit::BC,
                },
                1,
            ),
            0xd5 => (
                Push {
                    rr: Register16bit::DE,
                },
                1,
            ),
            0xe5 => (
                Push {
                    rr: Register16bit::HL,
                },
                1,
            ),
            0xf5 => (
                Push {
                    rr: Register16bit::AF,
                },
                1,
            ),
            // pop  rr          x1 (c1, d1, e1, f1)      12 (AF) rr=(SP)  SP=SP+2   (rr may be BC,DE,HL,AF)
            0xc1 => (
                Pop {
                    rr: Register16bit::BC,
                },
                1,
            ),
            0xd1 => (
                Pop {
                    rr: Register16bit::DE,
                },
                1,
            ),
            0xe1 => (
                Pop {
                    rr: Register16bit::HL,
                },
                1,
            ),
            0xf1 => (
                Pop {
                    rr: Register16bit::AF,
                },
                1,
            ),

            // GMB 8bit-Arithmetic/logical Commands
            //  add  A,r         8x         4 z0hc A=A+r
            0x80..=0x85 => (
                AddRegister {
                    r: Register::from(op & 0b0111),
                },
                1,
            ),
            0x86 => (AddMemoryHL, 1),
            0x87 => (AddRegister { r: Register::A }, 1),
            //  add  A,n         C6 nn      8 z0hc A=A+n
            0xC6 => (AddConstant { n: immediate_u8() }, 2),
            //  add  A,(HL)      86         8 z0hc A=A+(HL)
            //  adc  A,r         8x         4 z0hc A=A+r+cy
            0x88..=0x8D => (
                AddRegisterWithCarry {
                    r: Register::from(op & 0b0111),
                },
                1,
            ),
            0x8E => (AddMemoryHLWithCarry, 1),
            0x8F => (AddRegisterWithCarry { r: Register::A }, 1),
            //  adc  A,n         CE nn      8 z0hc A=A+n+cy
            0xCE => (AddConstantWithCarry { n: immediate_u8() }, 2),
            //  adc  A,(HL)      8E         8 z0hc A=A+(HL)+cy
            //  sub  r           9x         4 z1hc A=A-r
            0x90..=0x95 => (
                SubtractRegister {
                    r: Register::from(op & 0b0111),
                },
                1,
            ),
            0x96 => (SubtractMemoryHL, 1),
            0x97 => (SubtractRegister { r: Register::A }, 1),
            //  sub  n           D6 nn      8 z1hc A=A-n
            0xD6 => (SubtractConstant { n: immediate_u8() }, 2),
            //  sub  (HL)        96         8 z1hc A=A-(HL)
            //  sbc  A,r         9x         4 z1hc A=A-r-cy
            0x98..=0x9D => (
                SubtractRegisterWithCarry {
                    r: Register::from(op & 0b0111),
                },
                1,
            ),
            0x9E => (SubtractMemoryHLWithCarry, 1),
            0x9F => (SubtractRegisterWithCarry { r: Register::A }, 1),
            //  sbc  A,n         DE nn      8 z1hc A=A-n-cy
            0xDE => (SubtractConstantWithCarry { n: immediate_u8() }, 2),
            //  sbc  A,(HL)      9E         8 z1hc A=A-(HL)-cy
            //  and  r           Ax         4 z010 A=A & r
            0xA0..=0xA5 => (
                AndRegister {
                    r: Register::from(op & 0b0111),
                },
                1,
            ),
            0xA6 => (AndMemoryHL, 1),
            0xA7 => (AndRegister { r: Register::A }, 1),
            //  and  n           E6 nn      8 z010 A=A & n
            0xE6 => (AndConstant { n: immediate_u8() }, 2),
            //  and  (HL)        A6         8 z010 A=A & (HL)
            //  xor  r           Ax         4 z000
            0xA8..=0xAD => (
                XorRegister {
                    r: Register::from(op & 0b0111),
                },
                1,
            ),
            0xAE => (XorMemoryHL, 1),
            0xAF => (XorRegister { r: Register::A }, 1),
            //  xor  n           EE nn      8 z000
            0xEE => (XorConstant { n: immediate_u8() }, 2),
            //  xor  (HL)        AE         8 z000
            //  or   r           Bx         4 z000 A=A | r
            0xB0..=0xB5 => (
                OrRegister {
                    r: Register::from(op & 0b0111),
                },
                1,
            ),
            0xB6 => (OrMemoryHL, 1),
            0xB7 => (OrRegister { r: Register::A }, 1),
            //  or   n           F6 nn      8 z000 A=A | n
            0xF6 => (OrConstant { n: immediate_u8() }, 2),
            //  or   (HL)        B6         8 z000 A=A | (HL)
            //  cp   r           Bx         4 z1hc compare A-r
            0xB8..=0xBD => (
                CompareRegister {
                    r: Register::from(op & 0b0111),
                },
                1,
            ),
            0xBE => (CompareMemoryHL, 1),
            0xBF => (CompareRegister { r: Register::A }, 1),
            //  cp   n           FE nn      8 z1hc compare A-n
            0xFE => (CompareConstant { n: immediate_u8() }, 2),
            //  cp   (HL)        BE         8 z1hc compare A-(HL)
            //  inc  r           xx         4 z0h- r=r+1
            0x04 => (IncrementRegister { r: Register::B }, 1),
            0x0c => (IncrementRegister { r: Register::C }, 1),
            0x14 => (IncrementRegister { r: Register::D }, 1),
            0x1c => (IncrementRegister { r: Register::E }, 1),
            0x24 => (IncrementRegister { r: Register::H }, 1),
            0x2c => (IncrementRegister { r: Register::L }, 1),
            0x3c => (IncrementRegister { r: Register::A }, 1),
            //  inc  (HL)        34        12 z0h- (HL)=(HL)+1
            0x34 => (IncrementMemoryHL, 1),
            //  dec  r           xx         4 z1h- r=r-1
            0x05 => (DecrementRegister { r: Register::B }, 1),
            0x0d => (DecrementRegister { r: Register::C }, 1),
            0x15 => (DecrementRegister { r: Register::D }, 1),
            0x1d => (DecrementRegister { r: Register::E }, 1),
            0x25 => (DecrementRegister { r: Register::H }, 1),
            0x2d => (DecrementRegister { r: Register::L }, 1),
            0x3d => (DecrementRegister { r: Register::A }, 1),
            //  dec  (HL)        35        12 z1h- (HL)=(HL)-1
            0x35 => (DecrementMemoryHL, 1),
            //  daa              27         4 z-0x decimal adjust akku
            0x27 => (DecimalAdjust, 1),
            //  cpl              2F         4 -11- A = A xor FF
            0x2F => (CPL, 1),

            //GMB 16bit-Arithmetic/logical Commands
            //add  HL,rr     x9           8 -0hc HL = HL+rr     ;rr may be BC,DE,HL,SP
            0x09 => (
                AddRegisterHL16bit {
                    r: Register16bit::BC,
                },
                1,
            ),
            0x19 => (
                AddRegisterHL16bit {
                    r: Register16bit::DE,
                },
                1,
            ),
            0x29 => (
                AddRegisterHL16bit {
                    r: Register16bit::HL,
                },
                1,
            ),
            0x39 => (AddRegisterSPtoHL16bit, 1),
            //inc  rr        x3           8 ---- rr = rr+1      ;rr may be BC,DE,HL,SP
            0x03 => (
                IncrementRegister16bit {
                    r: Register16bit::BC,
                },
                1,
            ),
            0x13 => (
                IncrementRegister16bit {
                    r: Register16bit::DE,
                },
                1,
            ),
            0x23 => (
                IncrementRegister16bit {
                    r: Register16bit::HL,
                },
                1,
            ),
            0x33 => (IncrementSP, 1),
            //dec  rr        xB           8 ---- rr = rr-1      ;rr may be BC,DE,HL,SP
            0x0B => (
                DecrementRegister16bit {
                    r: Register16bit::BC,
                },
                1,
            ),
            0x1B => (
                DecrementRegister16bit {
                    r: Register16bit::DE,
                },
                1,
            ),
            0x2B => (
                DecrementRegister16bit {
                    r: Register16bit::HL,
                },
                1,
            ),
            0x3B => (DecrementSP, 1),
            //add  SP,dd     E8          16 00hc SP = SP +/- dd ;dd is 8bit signed number
            0xE8 => (AddSP { d: immediate_i8() }, 2),
            //ld   HL,SP+dd  F8          12 00hc HL = SP +/- dd ;dd is 8bit signed number
            0xF8 => (LoadHLwithSPplus { d: immediate_i8() }, 2),

            //GMB Rotate- und Shift-Commands
            //rlca           07           4 000c rotate akku left
            0x07 => (RotateLeft, 1),
            //rla            17           4 000c rotate akku left through carry
            0x17 => (RotateLeftThroughCarry, 1),
            //rrca           0F           4 000c rotate akku right
            0x0F => (RotateRight, 1),
            //rra            1F           4 000c rotate akku right through carry
            0x1F => (RotateRightThroughCarry, 1),
            0xCB => {
                let op = self.mem.read(self.pc + 1);
                match op {
                    //rlc  r         CB 0x        8 z00c rotate left
                    //rlc  (HL)      CB 06       16 z00c rotate left
                    0x00..=0x05 => (
                        RotateLeftRegisterThroughCarry {
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x06 => (RotateLeftHLThroughCarry, 2),
                    0x07 => (RotateLeftRegisterThroughCarry { r: Register::A }, 2),
                    //rrc  r         CB 0x        8 z00c rotate right
                    //rrc  (HL)      CB 0E       16 z00c rotate right
                    0x08..=0x0D => (
                        RotateRightRegisterThroughCarry {
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x0E => (RotateRightHLThroughCarry, 2),
                    0x0F => (RotateRightRegisterThroughCarry { r: Register::A }, 2),
                    //rl   r         CB 1x        8 z00c rotate left through carry
                    //rl   (HL)      CB 16       16 z00c rotate left through carry
                    0x10..=0x15 => (
                        RotateLeftRegister {
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x16 => (RotateLeftHL, 2),
                    0x17 => (RotateLeftRegister { r: Register::A }, 2),
                    //rr   r         CB 1x        8 z00c rotate right through carry
                    //rr   (HL)      CB 1E       16 z00c rotate right through carry
                    0x18..=0x1D => (
                        RotateRightRegister {
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x1E => (RotateRightHL, 2),
                    0x1F => (RotateRightRegister { r: Register::A }, 2),
                    //sla  r         CB 2x        8 z00c shift left arithmetic (b0=0)
                    //sla  (HL)      CB 26       16 z00c shift left arithmetic (b0=0)
                    0x20..=0x25 => (
                        ShiftLeftRegister {
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x26 => (ShiftLeftHL, 2),
                    0x27 => (ShiftLeftRegister { r: Register::A }, 2),
                    //sra  r         CB 2x        8 z00c shift right arithmetic (b7=b7)
                    //sra  (HL)      CB 2E       16 z00c shift right arithmetic (b7=b7)
                    0x28..=0x2D => (
                        ShiftRightArithmeticRegister {
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x2E => (ShiftRightArithmeticHL, 2),
                    0x2F => (ShiftRightArithmeticRegister { r: Register::A }, 2),
                    //swap r         CB 3x        8 z000 exchange low/hi-nibble
                    //swap (HL)      CB 36       16 z000 exchange low/hi-nibble
                    0x30..=0x35 => (
                        SwapRegister {
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x36 => (SwapHL, 2),
                    0x37 => (SwapRegister { r: Register::A }, 2),
                    //srl  r         CB 3x        8 z00c shift right logical (b7=0)
                    //srl  (HL)      CB 3E       16 z00c shift right logical (b7=0)
                    0x38..=0x3D => (
                        ShiftRightLogicalRegister {
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x3E => (ShiftRightLogicalHL, 2),
                    0x3F => (ShiftRightLogicalRegister { r: Register::A }, 2),

                    //GMB Singlebit Operation Commands
                    //bit  n,r       CB xx        8 z01- test bit n
                    //bit  n,(HL)    CB xx       12 z01- test bit n
                    0x40..=0x45 => (
                        TestBitRegister {
                            bit: 0,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x46 => (TestBitMemoryHL { bit: 0 }, 2),
                    0x47 => (
                        TestBitRegister {
                            bit: 0,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x48..=0x4D => (
                        TestBitRegister {
                            bit: 1,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x4E => (TestBitMemoryHL { bit: 1 }, 2),
                    0x4F => (
                        TestBitRegister {
                            bit: 1,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x50..=0x55 => (
                        TestBitRegister {
                            bit: 2,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x56 => (TestBitMemoryHL { bit: 2 }, 2),
                    0x57 => (
                        TestBitRegister {
                            bit: 2,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x58..=0x5D => (
                        TestBitRegister {
                            bit: 3,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x5E => (TestBitMemoryHL { bit: 3 }, 2),
                    0x5F => (
                        TestBitRegister {
                            bit: 3,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x60..=0x65 => (
                        TestBitRegister {
                            bit: 4,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x66 => (TestBitMemoryHL { bit: 4 }, 2),
                    0x67 => (
                        TestBitRegister {
                            bit: 4,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x68..=0x6D => (
                        TestBitRegister {
                            bit: 5,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x6E => (TestBitMemoryHL { bit: 5 }, 2),
                    0x6F => (
                        TestBitRegister {
                            bit: 5,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x70..=0x75 => (
                        TestBitRegister {
                            bit: 6,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x76 => (TestBitMemoryHL { bit: 6 }, 2),
                    0x77 => (
                        TestBitRegister {
                            bit: 6,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x78..=0x7D => (
                        TestBitRegister {
                            bit: 7,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x7E => (TestBitMemoryHL { bit: 7 }, 2),
                    0x7F => (
                        TestBitRegister {
                            bit: 7,
                            r: Register::A,
                        },
                        2,
                    ),

                    //res  n,r       CB xx        8 ---- reset bit n
                    //res  n,(HL)    CB xx       16 ---- reset bit n
                    0x80..=0x85 => (
                        ResetBitRegister {
                            bit: 0,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x86 => (ResetBitMemoryHL { bit: 0 }, 2),
                    0x87 => (
                        ResetBitRegister {
                            bit: 0,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x88..=0x8D => (
                        ResetBitRegister {
                            bit: 1,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x8E => (ResetBitMemoryHL { bit: 1 }, 2),
                    0x8F => (
                        ResetBitRegister {
                            bit: 1,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x90..=0x95 => (
                        ResetBitRegister {
                            bit: 2,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x96 => (ResetBitMemoryHL { bit: 2 }, 2),
                    0x97 => (
                        ResetBitRegister {
                            bit: 2,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x98..=0x9D => (
                        ResetBitRegister {
                            bit: 3,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x9E => (ResetBitMemoryHL { bit: 3 }, 2),
                    0x9F => (
                        ResetBitRegister {
                            bit: 3,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xA0..=0xA5 => (
                        ResetBitRegister {
                            bit: 4,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xA6 => (ResetBitMemoryHL { bit: 4 }, 2),
                    0xA7 => (
                        ResetBitRegister {
                            bit: 4,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xA8..=0xAD => (
                        ResetBitRegister {
                            bit: 5,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xAE => (ResetBitMemoryHL { bit: 5 }, 2),
                    0xAF => (
                        ResetBitRegister {
                            bit: 5,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xB0..=0xB5 => (
                        ResetBitRegister {
                            bit: 6,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xB6 => (ResetBitMemoryHL { bit: 6 }, 2),
                    0xB7 => (
                        ResetBitRegister {
                            bit: 6,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xB8..=0xBD => (
                        ResetBitRegister {
                            bit: 7,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xBE => (ResetBitMemoryHL { bit: 7 }, 2),
                    0xBF => (
                        ResetBitRegister {
                            bit: 7,
                            r: Register::A,
                        },
                        2,
                    ),

                    //set  n,r       CB xx        8 ---- set bit n
                    //set  n,(HL)    CB xx       16 ---- set bit n
                    0xC0..=0xC5 => (
                        SetBitRegister {
                            bit: 0,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xC6 => (SetBitMemoryHL { bit: 0 }, 2),
                    0xC7 => (
                        SetBitRegister {
                            bit: 0,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xC8..=0xCD => (
                        SetBitRegister {
                            bit: 1,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xCE => (SetBitMemoryHL { bit: 1 }, 2),
                    0xCF => (
                        SetBitRegister {
                            bit: 1,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xD0..=0xD5 => (
                        SetBitRegister {
                            bit: 2,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xD6 => (SetBitMemoryHL { bit: 2 }, 2),
                    0xD7 => (
                        SetBitRegister {
                            bit: 2,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xD8..=0xDD => (
                        SetBitRegister {
                            bit: 3,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xDE => (SetBitMemoryHL { bit: 3 }, 2),
                    0xDF => (
                        SetBitRegister {
                            bit: 3,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xE0..=0xE5 => (
                        SetBitRegister {
                            bit: 4,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xE6 => (SetBitMemoryHL { bit: 4 }, 2),
                    0xE7 => (
                        SetBitRegister {
                            bit: 4,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xE8..=0xED => (
                        SetBitRegister {
                            bit: 5,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xEE => (SetBitMemoryHL { bit: 5 }, 2),
                    0xEF => (
                        SetBitRegister {
                            bit: 5,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xF0..=0xF5 => (
                        SetBitRegister {
                            bit: 6,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xF6 => (SetBitMemoryHL { bit: 6 }, 2),
                    0xF7 => (
                        SetBitRegister {
                            bit: 6,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xF8..=0xFD => (
                        SetBitRegister {
                            bit: 7,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xFE => (SetBitMemoryHL { bit: 7 }, 2),
                    0xFF => (
                        SetBitRegister {
                            bit: 7,
                            r: Register::A,
                        },
                        2,
                    ),
                }
            }
            //GMB CPU-Control Commands
            //ccf            3F           4 -00c cy=cy xor 1
            0x3F => (CCF, 1),
            //scf            37           4 -001 cy=1
            0x37 => (SCF, 1),
            //nop            00           4 ---- no operation
            0x00 => (NOP, 1),
            //halt           76         N*4 ---- halt until interrupt occurs (low power)
            0x76 => (HALT, 1),
            //stop           10 00        ? ---- low power standby mode (VERY low power)
            0x10 => (STOP, 2),
            //di             F3           4 ---- disable interrupts, IME=0
            0xF3 => (DI, 1),
            //ei             FB           4 ---- enable interrupts, IME=1
            0xFB => (EI, 1),

            //GMB Jump Commands
            //jp   nn        C3 nn nn    16 ---- jump to nn, PC=nn
            0xC3 => (
                JP {
                    nn: immediate_u16(),
                },
                3,
            ),
            //jp   HL        E9           4 ---- jump to HL, PC=HL
            0xE9 => (JPtoMemoryHL, 1),
            //jp   f,nn      xx nn nn 16;12 ---- conditional jump if nz,z,nc,c
            0xC2 => (
                CondJP {
                    flag: ConditionalFlag::Zero,
                    check_state: false,
                    nn: immediate_u16(),
                },
                3,
            ),
            0xCA => (
                CondJP {
                    flag: ConditionalFlag::Zero,
                    check_state: true,
                    nn: immediate_u16(),
                },
                3,
            ),
            0xD2 => (
                CondJP {
                    flag: ConditionalFlag::Carry,
                    check_state: false,
                    nn: immediate_u16(),
                },
                3,
            ),
            0xDA => (
                CondJP {
                    flag: ConditionalFlag::Carry,
                    check_state: true,
                    nn: immediate_u16(),
                },
                3,
            ),
            //jr   PC+dd     18 dd       12 ---- relative jump to nn (PC=PC+/-7bit)
            0x18 => (JRtoMemory { dd: immediate_i8() }, 2),
            //jr   f,PC+dd   xx dd     12;8 ---- conditional relative jump if nz,z,nc,c
            0x20 => (
                CondJR {
                    flag: ConditionalFlag::Zero,
                    check_state: false,
                    dd: immediate_i8(),
                },
                2,
            ),
            0x28 => (
                CondJR {
                    flag: ConditionalFlag::Zero,
                    check_state: true,
                    dd: immediate_i8(),
                },
                2,
            ),
            0x30 => (
                CondJR {
                    flag: ConditionalFlag::Carry,
                    check_state: false,
                    dd: immediate_i8(),
                },
                2,
            ),
            0x38 => (
                CondJR {
                    flag: ConditionalFlag::Carry,
                    check_state: true,
                    dd: immediate_i8(),
                },
                2,
            ),
            //call nn        CD nn nn    24 ---- call to nn, SP=SP-2, (SP)=PC, PC=nn
            0xCD => (
                CALL {
                    nn: immediate_u16(),
                },
                3,
            ),
            //call f,nn      xx nn nn 24;12 ---- conditional call if nz,z,nc,c
            0xC4 => (
                CondCALL {
                    flag: ConditionalFlag::Zero,
                    check_state: false,
                    nn: immediate_u16(),
                },
                3,
            ),
            0xCC => (
                CondCALL {
                    flag: ConditionalFlag::Zero,
                    check_state: true,
                    nn: immediate_u16(),
                },
                3,
            ),
            0xD4 => (
                CondCALL {
                    flag: ConditionalFlag::Carry,
                    check_state: false,
                    nn: immediate_u16(),
                },
                3,
            ),
            0xDC => (
                CondCALL {
                    flag: ConditionalFlag::Carry,
                    check_state: true,
                    nn: immediate_u16(),
                },
                3,
            ),
            //ret            C9          16 ---- return, PC=(SP), SP=SP+2
            0xC9 => (RET, 1),
            //ret  f         xx        20;8 ---- conditional return if nz,z,nc,c
            0xC0 => (CondRET { flag: ConditionalFlag::Zero, check_state: false }, 1),
            0xC8 => (CondRET { flag: ConditionalFlag::Zero, check_state: true }, 1),
            0xD0 => (CondRET { flag: ConditionalFlag::Carry, check_state: false }, 1),
            0xD8 => (CondRET { flag: ConditionalFlag::Carry, check_state: true }, 1),
            //reti           D9          16 ---- return and enable interrupts (IME=1)
            0xD9 => (RETI, 1),
            //rst  n         xx          16 ---- call to 00,08,10,18,20,28,30,38
            0xC7 => (RST { n: 0x00 }, 1),
            0xCF => (RST { n: 0x08 }, 1),
            0xD7 => (RST { n: 0x10 }, 1),
            0xDF => (RST { n: 0x18 }, 1),
            0xE7 => (RST { n: 0x20 }, 1),
            0xEF => (RST { n: 0x28 }, 1),
            0xF7 => (RST { n: 0x30 }, 1),
            0xFF => (RST { n: 0x38 }, 1),
            0x08 => (LoadAddressWithSP { n: immediate_u16() }, 3),
            _ => panic!("pc: {:x?}, op: {:x?}", self.pc, op),
        }
    }
}
