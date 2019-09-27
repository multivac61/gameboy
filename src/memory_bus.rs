use std::fs::File;
use std::io::Read;

use crate::cpu::{Register, Register16bit, MemoryAddress, OAM, Rom, Ram, Cpu};
use crate::util::{LittleEndian};

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

pub struct MemoryBus {
    pub raw_memory: Vec<u8>,
    cartridge: Vec<u8>,
    ram_banks: Vec<u8>,
    ram_bank: Ram,
    rom_bank: Rom,
    should_enable_ram: bool,
    is_rom_banking: bool,
}

impl MemoryBus {
    pub fn new(cartridge: &[u8]) -> Self {
        //        0100-014F contains the cartridge header
        let mut m = vec![0; 0x10000];

        m[0..0x4000].copy_from_slice(&cartridge[0..0x4000]);

        let mut file = File::open("/home/horigome/dev/rust/gameboy/src/DMG_ROM.bin")
            .expect("There was an issue opening the file");
        let mut buffer = Vec::new();
        let _bytes_read = file.read_to_end(&mut buffer);
        m[0..0x100].copy_from_slice(&buffer.as_slice());

        let b = match cartridge[0x147] {
            0 => Rom::Local,
            1..=3 => Rom::Bank1,
            5..=6 => Rom::Bank2,
            _ => unreachable!(),
        };

        MemoryBus {
            raw_memory: m,
            cartridge: cartridge.to_vec(),
            ram_banks: vec![0; 4 * 0x2000],
            ram_bank: Ram::Bank0,
            rom_bank: b,
            should_enable_ram: false,
            is_rom_banking: false,
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
                // Ram Enable
                // Do RAM bank Enable
                if let Rom::Bank2 = self.rom_bank {
                    if Cpu::ith_bit(self.read(address), 4) {
                        return;
                    }
                }

                if data & 0x0F == 0x0A {
                    self.should_enable_ram = true;
                } else if data & 0x0F == 0x00 {
                    self.should_enable_ram = false;
                }
            }
            0x2000..=0x3FFF => {
                let new_bank = match self.rom_bank {
                    Rom::Bank1 => {
                        (self.rom_bank as u8 & 244) | (data & 31)
                    }
                    Rom::Bank2 => {
                        (data & 0xF)
                    }
                    _ => 0
                };
                self.rom_bank = Rom::from(new_bank);
            }
            0x4000..=0x5FFF => {
                if let Rom::Bank1 = self.rom_bank {
                    let new_bank = if self.is_rom_banking {
                        (self.rom_bank as u8) & 0b0001_1111 | (data & 0b1110_0000)
                    } else {
                        self.rom_bank as u8 & 0b0000_0011
                    };
                    self.rom_bank = Rom::from(new_bank);
                }
            }
            0x6000..=0x7FFF => {
                if let Rom::Bank1 = self.rom_bank {
                    // Change ROM/RAM mode
                    self.is_rom_banking = data & 0x01 == 0x01;
                    if self.is_rom_banking {
                        self.ram_bank = Ram::Bank0;
                    }
                }
            }
            0xA000..=0xBFFF => {
                if self.should_enable_ram {
                    let address = address as usize - 0xA000 + 0x2000 * self.ram_bank as usize;
                    self.ram_banks[address] = data;
                }
            }
            0xE000..=0xFDFF => unreachable!(),
            0xFEA0..=0xFEFE => self.write(address - 0x2000, data),
            DIV => self.raw_memory[DIV as usize] = 0,
            DMA => {
                let source = LittleEndian::u16(0, data) as usize;
                self.raw_memory.copy_within(source..source + 0xA0, OAM as usize);
            }
            _ => self.raw_memory[address as usize] = data,
        }
    }

    pub fn read_word(&self, address: MemoryAddress) -> u16 {
        LittleEndian::u16(self.read(address), self.read(address + 1))
    }

    pub fn read(&self, address: MemoryAddress) -> u8 {
        match address {
            0x4000..=0x7FFF => {
                self.cartridge[address as usize - 0x4000 + 0x4000 * self.rom_bank as usize]
            }
            0xA000..=0xBFFF => {
                self.ram_banks[address as usize - 0xA000 + 0x2000 * self.ram_bank as usize]
            }
            _ => self.raw_memory[address as usize],
        }
    }
}
