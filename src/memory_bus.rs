use crate::apu::{self, Apu};
use crate::cartridge::{self, Cartridge};
use crate::cpu::{MemoryAddress, BOOT_ROM_ENABLE_REGISTER};
use crate::hram::{self, Hram};
use crate::joypad::{self, Joypad};
use crate::ppu::{self, Ppu};
use crate::ram::{self, Ram};
use crate::serial::{self, Serial};
use crate::timer::{self, Timer};
use crate::util;

pub const BOOT_ROM_SIZE: usize = 0x100;
const BOOT_ROM_START: MemoryAddress = 0x0000;
const BOOT_ROM_END: MemoryAddress = 0x100;

const ROM_BANK_START: MemoryAddress = 0x0000;
const ROM_BANK_END: MemoryAddress = ppu::RAM_START - 1;

const INTERRUPT_FLAG: MemoryAddress = 0xFF0F;
const DMA: MemoryAddress = 0xFF46;
const INTERRUPT_ENABLE: MemoryAddress = 0xFFFF;

pub struct MemoryBus {
    is_boot_rom_enabled: bool,
    boot_rom: Vec<u8>,

    cartridge: Cartridge,
    ram: Ram,
    pub ppu: Ppu,
    hram: Hram,

    pub joypad: Joypad,
    pub serial: Serial,
    pub timer: Timer,
    pub apu: Apu,

    pub interrupt_flag: u8,
    pub interrupt_enable: u8,
}

impl MemoryBus {
    pub fn new(game_rom: &[u8], boot_rom: Option<Vec<u8>>) -> Self {
        MemoryBus {
            is_boot_rom_enabled: match &boot_rom {
                Some(b) if b.len() == BOOT_ROM_SIZE => true,
                _ => false
            },
            boot_rom: match &boot_rom {
                Some(b) if b.len() == BOOT_ROM_SIZE => b.clone(),
                _ => vec!()
            },

            cartridge: Cartridge::new(game_rom),
            ram: Ram::new(),
            ppu: Ppu::new(),
            hram: Hram::new(),

            joypad: Joypad::new(),
            serial: Serial::new(),
            timer: Timer::new(),
            apu: Apu::new(),

            interrupt_flag: 0,
            interrupt_enable: 0,
        }
    }

    pub fn write_word(&mut self, address: MemoryAddress, data: u16) {
        self.write(address, util::little_endian::lsb(data));
        self.write(address + 1, util::little_endian::msb(data));
    }

    pub fn write(&mut self, address: MemoryAddress, data: u8) {
        // TODO: Refactor
        match address {
            ROM_BANK_START..=ROM_BANK_END => self.cartridge.write_rom(address, data),

            ppu::RAM_START..=ppu::RAM_END => self.ppu.write(address, data),

            cartridge::RAM_START..=cartridge::RAM_END => self.cartridge.write_ram(address, data),

            ram::START..=ram::ECHO_END => self.ram.write(address, data),

            ppu::OAM_START..=ppu::OAM_END => self.ppu.write(address, data),

            joypad::P1 => self.joypad.write(address, data),

            serial::START..=serial::END => self.serial.write(address, data),

            timer::START..=timer::END => self.timer.write(address, data),

            INTERRUPT_FLAG => self.interrupt_flag = data,

            apu::START..=apu::END => self.apu.write(address, data),

            ppu::REG_START..=ppu::LYC => self.ppu.write(address, data),
            DMA => {
                let source = u16::from_le_bytes([0, data]);
                let chunk: Vec<u8> = (source..source + 0xA0).map(|addr| self.read(addr)).collect();

                self.ppu.dma(&chunk);
            }
            ppu::BGP..=ppu::REG_END => self.ppu.write(address, data),

            hram::START..=hram::END => self.hram.write(address, data),

            INTERRUPT_ENABLE => self.interrupt_enable = data,

            BOOT_ROM_ENABLE_REGISTER => self.is_boot_rom_enabled = data == 0,

            _ => println!("Tried to write to unavailable address {:#x}: {:#x}", address, data),
        };
    }

    pub fn read_word(&self, address: MemoryAddress) -> u16 {
        u16::from_le_bytes([self.read(address), self.read(address + 1)])
    }

    pub fn read(&self, address: MemoryAddress) -> u8 {
        if address < BOOT_ROM_END && self.is_boot_rom_enabled && self.boot_rom.len() == BOOT_ROM_SIZE {
            return self.boot_rom[address as usize];
        }

        match address {
            ROM_BANK_START..=ROM_BANK_END => self.cartridge.read_rom(address),

            ppu::RAM_START..=ppu::RAM_END => self.ppu.read(address),

            cartridge::RAM_START..=cartridge::RAM_END => self.cartridge.read_ram(address),

            ram::START..=ram::ECHO_END => self.ram.read(address),

            ppu::OAM_START..=ppu::OAM_END => self.ppu.read(address),

            joypad::P1 => self.joypad.read(address),

            serial::START..=serial::END => self.serial.read(address),

            timer::START..=timer::END => self.timer.read(address),

            INTERRUPT_FLAG => self.interrupt_flag,

            apu::START..=apu::END => self.apu.read(address),

            ppu::REG_START..=ppu::REG_END => self.ppu.read(address),

            hram::START..=hram::END => self.hram.read(address),

            INTERRUPT_ENABLE => self.interrupt_enable,

            _ => {
                println!("Tried to read from unavailable address {:#x}", address);
                0x00
            }
        }
    }

    pub fn update_timers(&mut self, cycles: u8) -> u8 {
        self.timer.update(cycles.into())
    }
}
