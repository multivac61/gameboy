use crate::cpu::MemoryAddress;

pub const ROM_BANK_0_START: u16 = 0x0000;
const ROM_BANK_0_END: u16 = 0x3FFF;
pub const ROM_BANK_N_START: u16 = 0x4000;
const ROM_BANK_N_END: u16 = 0x7FFF;

const ROM_BANK_SIZE: u16 = 0x4000;
const RAM_BANK_SIZE: u16 = 0x2000;

pub const RAM_START: u16 = 0xA000;
pub const RAM_END: u16 = RAM_START + RAM_BANK_SIZE - 1;

enum Controller {
    Local,
    MBC1 { is_4mbit: bool },
    MBC2,
}

pub struct Cartridge {
    controller: Controller,
    rom: Vec<u8>,
    cur_rom_bank: u8,
    ram: Vec<u8>,
    cur_ram_bank: u8,
    should_enable_ram: bool,
}

impl Cartridge {
    pub fn new(cartridge: &[u8]) -> Self {
        let controller = match cartridge[0x147] {
            0 => Controller::Local,
            1..=3 => Controller::MBC1 { is_4mbit: false },
            5..=6 => Controller::MBC2,
            _ => unreachable!(),
        };

        let ram_size = match cartridge[0x149] {
            0 => 0,
            1 => 0x800,
            2 => 0x2000,
            3 => 0x8000,
            4 => 0x20000,
            _ => unreachable!(),
        };

        Cartridge {
            controller,
            rom: cartridge.to_vec(),
            cur_rom_bank: 1,
            ram: vec![0; ram_size],
            cur_ram_bank: 0,
            should_enable_ram: false,
        }
    }

    pub fn read_rom(&self, address: MemoryAddress) -> u8 {
        match address {
            ROM_BANK_0_START..=ROM_BANK_0_END => self.rom[address as usize],
            ROM_BANK_N_START..=ROM_BANK_N_END => {
                let address = address - ROM_BANK_N_START + ROM_BANK_SIZE * self.cur_rom_bank as u16;
                self.rom[address as usize]
            }
            _ => panic!("Tried to write to invalid cartridge address"),
        }
    }

    pub fn read_ram(&self, address: MemoryAddress) -> u8 {
        let address = address - ROM_BANK_N_START + RAM_BANK_SIZE * self.cur_ram_bank as u16;
        self.ram[address as usize]
    }

    pub fn write_rom(&mut self, address: MemoryAddress, data: u8) {
        match address {
            0x0000..=0x1FFF => {
                self.should_enable_ram = match self.controller {
                    Controller::Local => false,
                    Controller::MBC1 { is_4mbit: false } => data & 0x0F == 0x0A,
                    _ => unreachable!(),
                };
            }
            0x2000..=0x3FFF => {
                self.cur_rom_bank = match self.controller {
                    Controller::Local => 1,
                    Controller::MBC1 { is_4mbit: _ } => match data {
                        0..=1 => 1,
                        _ => data & 0b0001_1111,
                    },
                    Controller::MBC2 => {
                        // The least significant bit of the upper address
                        // byte must be zero to enable/disable cart RAM.
                        if (address >> 8) & 0x01 == 0x00 {
                            self.should_enable_ram = true;
                            self.cur_rom_bank
                        } else {
                            // The least significant bit of the upper address
                            // byte must be one to select a ROM bank.
                            data & 0b0000_1111
                        }
                    }
                };
            }
            0x4000..=0x5FFF => {
                match self.controller {
                    Controller::MBC1 { is_4mbit } => {
                        if is_4mbit {
                            // If memory model is set to 4/32:
                            // Writing a value (XXXXXXBB - X = Don't care, B =
                            // bank select bits) into 4000-5FFF area will select
                            // an appropriate RAM bank at A000-C000.
                            self.cur_ram_bank = data & 0b0000_0011;
                        } else {
                            // If memory model is set to 16/8 mode:
                            // Writing a value (XXXXXXBB - X = Don't care, B =
                            // bank select bits) into 4000-5FFF area will set the
                            // two most significant ROM address lines.
                            self.cur_rom_bank &= 0b0001_11111;
                            self.cur_rom_bank |= (data & 0b11) << 6;
                        }
                    }
                    _ => unreachable!(),
                };
            }
            0x6000..=0x7FFF => {
                match self.controller {
                    Controller::MBC1 { is_4mbit: _ } => {
                        // 0: 16MBit ROM / 8KByte RAM
                        // 1: 4Mbit ROM / 32KByte RAM
                        self.controller = Controller::MBC1 {
                            is_4mbit: data & 0x01 == 0x01,
                        };
                    }
                    _ => unreachable!(),
                };
            }
            _ => unreachable!(),
        }
    }

    pub fn write_ram(&mut self, address: MemoryAddress, data: u8) {
        match address {
            0xA000..=0xBFFF => {
                if self.should_enable_ram {
                    let address = address as usize - 0xA000 + 0x2000 * self.cur_ram_bank as usize;
                    self.ram[address] = data;
                }
            }
            _ => unreachable!()
        }
    }
}
