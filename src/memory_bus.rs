use std::fs::File;
use std::io::Read;

use crate::cpu::{BOOT_ROM_ENABLE_REGISTER, DIV, DMA, MemoryAddress, OAM, P1};
use crate::joypad::Joypad;
use crate::util::little_endian;

// 0000-3FFF 16KB ROM Bank 00 (in cartridge, fixed at bank 00)
// 4000-7FFF 16KB ROM Bank 01..NN (in cartridge, switchable bank number)
// 8000-9FFF 8KB Video RAM (VRAM) (switchable bank 0-1 in CGB Mode)
// A000-BFFF 8KB External RAM (in cartridge, switchable bank, if any)
// C000-CFFF 4KB Work RAM Bank 0 (WRAM)
// D000-DFFF 4KB Work RAM Bank 1 (WRAM) (switchable bank 1-7 in CGB Mode)
// E000-FDFF Same as C000-DDFF (ECHO) (typically not used)
// FE00-FE9F Sprite Attribute Table (OAM)
// FEA0-FEFF Not Usable
// FF00-FF7F I/O Ports
// FF80-FFFE High RAM (HRAM)
// FFFF Interrupt Enable Register

enum MemoryBankType {
    Local,
    MBC1 { is_4mbit: bool },
    MBC2,
}

pub struct MemoryBus {
    pub raw_memory: Vec<u8>,
    boot_rom: Vec<u8>,
    cartridge: Vec<u8>,
    ram_banks: Vec<u8>,
    ram_bank: u8,
    rom_type: MemoryBankType,
    rom_bank: u8,
    should_enable_ram: bool,
    is_boot_rom_enabled: bool,
    pub joypad: Joypad,
}

impl MemoryBus {
    pub fn new(cartridge: &[u8], enable_boot_rom: bool) -> Self {
        //        0100-014F contains the cartridge header
        let mut m = vec![0; 0x10000];

        m[0..0x4000].copy_from_slice(&cartridge[0..0x4000]);

        let file_path = std::env::current_dir()
            .unwrap()
            .join(std::path::Path::new("src"))
            .join(std::path::Path::new("DMG_ROM.bin"));

        let mut file = File::open(file_path).expect("There was an issue opening the file");
        let mut buffer = Vec::new();
        let _bytes_read = file.read_to_end(&mut buffer);
        assert!(_bytes_read.unwrap() == 0x100);

        let b = match cartridge[0x147] {
            0 => MemoryBankType::Local,
            1..=3 => MemoryBankType::MBC1 { is_4mbit: false },
            5..=6 => MemoryBankType::MBC2,
            _ => unreachable!(),
        };

        MemoryBus {
            raw_memory: m,
            boot_rom: buffer,
            cartridge: cartridge.to_vec(),
            ram_banks: vec![0; 4 * 0x2000],
            ram_bank: 0,
            rom_type: b,
            rom_bank: 1,
            should_enable_ram: false,
            is_boot_rom_enabled: enable_boot_rom,
            joypad: Joypad::new(),
        }
    }

    pub fn write_word(&mut self, address: MemoryAddress, data: u16) {
        self.write(address, (data & 0xFF) as u8);
        self.write(address + 1, (data >> 8) as u8);
    }

    pub fn write(&mut self, address: MemoryAddress, data: u8) {
        //        0000-3FFF   16KB ROM Bank 00     (in cartridge, fixed at bank 00)
        //        4000-7FFF   16KB ROM Bank 01..NN (in cartridge, switchable bank number)
        //        8000-9FFF   8KB Video RAM (VRAM) (switchable bank 0-1 in CGB Mode)
        //        A000-BFFF   8KB External RAM     (in cartridge, switchable bank, if any)
        //        C000-CFFF   4KB Work RAM Bank 0 (WRAM)
        //        D000-DFFF   4KB Work RAM Bank 1 (WRAM)  (switchable bank 1-7 in CGB Mode)
        //        E000-FDFF   Same as C000-DDFF (ECHO)    (typically not used)
        //        FE00-FE9F   Sprite Attribute Table (OAM)
        //        FEA0-FEFF   Not Usable
        //        FF00-FF7F   I/O Ports
        //        FF80-FFFE   High RAM (HRAM)
        //        FFFF        Interrupt Enable Register0
        match address {
            0x0000..=0x1FFF => {
                self.should_enable_ram = match self.rom_type {
                    MemoryBankType::Local => false,
                    MemoryBankType::MBC1 { is_4mbit: false } => data & 0x0F == 0x0A,
                    _ => unreachable!(),
                };
            }
            0x2000..=0x3FFF => {
                self.rom_bank = match self.rom_type {
                    MemoryBankType::Local => 1,
                    MemoryBankType::MBC1 { is_4mbit: _ } => match data {
                        0..=1 => 1,
                        _ => data & 0b0001_1111,
                    },
                    MemoryBankType::MBC2 => {
                        // The least significant bit of the upper address
                        // byte must be zero to enable/disable cart RAM.
                        if (address >> 8) & 0x01 == 0x00 {
                            self.should_enable_ram = true;
                            self.rom_bank
                        } else {
                            // The least significant bit of the upper address
                            // byte must be one to select a ROM bank.
                            data & 0b0000_1111
                        }
                    }
                };
            }
            0x4000..=0x5FFF => {
                match self.rom_type {
                    MemoryBankType::MBC1 { is_4mbit } => {
                        if is_4mbit {
                            // If memory model is set to 4/32:
                            // Writing a value (XXXXXXBB - X = Don't care, B =
                            // bank select bits) into 4000-5FFF area will select
                            // an appropriate RAM bank at A000-C000.
                            self.ram_bank = data & 0b0000_0011;
                        } else {
                            // If memory model is set to 16/8 mode:
                            // Writing a value (XXXXXXBB - X = Don't care, B =
                            // bank select bits) into 4000-5FFF area will set the
                            // two most significant ROM address lines.
                            self.rom_bank &= 0b0001_11111;
                            self.rom_bank |= (data & 0b11) << 6;
                        }
                    }
                    _ => unreachable!(),
                };
            }
            0x6000..=0x7FFF => {
                match self.rom_type {
                    MemoryBankType::MBC1 { is_4mbit: _ } => {
                        // 0: 16MBit ROM / 8KByte RAM
                        // 1: 4Mbit ROM / 32KByte RAM
                        self.rom_type = MemoryBankType::MBC1 {
                            is_4mbit: data & 0x01 == 0x01,
                        };
                    }
                    _ => unreachable!(),
                };
            }
            0xA000..=0xBFFF => {
                if self.should_enable_ram {
                    let address = address as usize - 0xA000 + 0x2000 * self.ram_bank as usize;
                    self.ram_banks[address] = data;
                }
            }
            0xC000..=0xDDFF => {
                self.raw_memory[address as usize] = data;
                self.raw_memory[address as usize + (0xE000 - 0xC000)] = data;
            }
            0xE000..=0xFDFF => {
                self.raw_memory[address as usize] = data;
                self.raw_memory[address as usize - (0xE000 - 0xC000)] = data;
            }
            0xFEA0..=0xFEFE => self.write(address - 0x2000, data),
            P1 => self.joypad.set_state(data),
            0xFF01 => print!("{}", data as char),
            DIV => self.raw_memory[DIV as usize] = 0,
            DMA => {
                let source = little_endian::u16(0, data) as usize;
                self.raw_memory
                    .copy_within(source..source + 0xA0, OAM as usize);
            }
            BOOT_ROM_ENABLE_REGISTER => self.is_boot_rom_enabled = data == 0,
            _ => self.raw_memory[address as usize] = data,
        }
    }

    pub fn read_word(&self, address: MemoryAddress) -> u16 {
        little_endian::u16(self.read(address), self.read(address + 1))
    }

    pub fn read(&self, address: MemoryAddress) -> u8 {
        match address {
            0x0000..=0x00FF => {
                if self.is_boot_rom_enabled {
                    self.boot_rom[address as usize]
                } else {
                    self.raw_memory[address as usize]
                }
            }
            0x4000..=0x7FFF => {
                self.cartridge[address as usize - 0x4000 + 0x4000 * self.rom_bank as usize]
            }
            0xA000..=0xBFFF => {
                self.ram_banks[address as usize - 0xA000 + 0x2000 * self.ram_bank as usize]
            }
            P1 => self.joypad.get_state(),
            _ => self.raw_memory[address as usize],
        }
    }
}
