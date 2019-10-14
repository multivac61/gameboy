use std::fs::File;
use std::io::Read;

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

const ROM_BANK_START: MemoryAddress = 0x0000;
const ROM_BANK_END: MemoryAddress = ppu::RAM_START;

const INTERRUPT_FLAG: MemoryAddress = 0xFF0F;
const DMA: MemoryAddress = 0xFF46;
const INTERRUPT_ENABLE: MemoryAddress = 0xFFFF;

pub struct MemoryBus {
    boot_rom: Vec<u8>,
    is_boot_rom_enabled: bool,

    cartridge: Cartridge,
    ram: Ram,
    pub ppu: Ppu,
    hram: Hram,

    pub joypad: Joypad,
    pub serial: Serial,
    pub timer: Timer,

    pub interrupt_flag: u8,
    pub interrupt_enable: u8,
}

impl MemoryBus {
    pub fn new(game_rom: &[u8], enable_boot_rom: bool) -> Self {
        let file_path = std::env::current_dir()
            .unwrap()
            .join(std::path::Path::new("src"))
            .join(std::path::Path::new("DMG_ROM.bin"));

        let mut file = File::open(file_path).expect("There was an issue opening the file");
        let mut boot_rom = Vec::new();
        let _bytes_read = file.read_to_end(&mut boot_rom);
        assert_eq!(_bytes_read.unwrap(), 0x100);

        MemoryBus {
            boot_rom,
            is_boot_rom_enabled: enable_boot_rom,

            cartridge: Cartridge::new(game_rom),
            ram: Ram::new(),
            ppu: Ppu::new(),
            hram: Hram::new(),

            joypad: Joypad::new(),
            timer: Timer::new(),

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
            ROM_BANK_START..ROM_BANK_END => self.cartridge.write_rom(address, data),

            ppu::RAM_START..ppu::RAM_END => self.ppu.write(address, data),

            cartridge::RAM_START..cartridge::RAM_END => self.cartridge.write_ram(address, data),

            ram::START..ram::ECHO_END => self.ram.write(address, data),

            ppu::OAM_START..ppu::OAM_END => self.ppu.write(address, data),

            JOY_PAD => self.joypad.write(address, data),

            serial::START..serial::END => self.serial.write(address, data),

            timer::START..timer::END => self.timer.write(address, data),

            INTERRUPT_FLAG => self.interrupt_flag = data,

            apu::START..apu::END => self.apu.write(address),

            ppu::REG_START..ppu::REG_END => self.ppu.write(address),

            hram::START..hram::END => self.hram.write(address),

            INTERRUPT_ENABLE => self.interrupt_enable = data,

            DMA => {
                let source = u16::from_le_bytes([0, data]) as usize;
                self.raw_memory.copy_within(source..source + 0xA0, OAM as usize);
                let chunk = self.
                self.ppu.dma(chunk);
            }

            BOOT_ROM_ENABLE_REGISTER => self.is_boot_rom_enabled = data == 0,

            _ => unreachable!(),
        }
    }

    pub fn read_word(&self, address: MemoryAddress) -> u16 {
        u16::from_le_bytes([self.read(address), self.read(address + 1)])
    }

    pub fn read(&self, address: MemoryAddress) -> u8 {
        if address < BOOT_ROM_END && self.is_boot_rom_enabled {
            self.boot_rom[address as usize]
        }

        match address {
            ROM_BANK_START..ROM_BANK_END => self.cartridge.read_rom(address),

            ppu::RAM_START..ppu::RAM_END => self.ppu.read(address),

            cartridge::RAM_START..cartridge::RAM_END => self.cartridge.read_ram(address),

            ram::START..ram::ECHO_END => self.ram.read(address),

            ppu::OAM_START..ppu::OAM_END => self.ppu.read(address),

            JOY_PAD => self.joypad.read(address),

            serial::START..serial::END => self.serial.read(address),

            timer::START..timer::END => self.timer.read(address),

            INTERRUPT_FLAG => self.interrupt_flag,

            apu::START..apu::END => self.apu.read(address),

            ppu::REG_START..ppu::REG_END => self.ppu.read(address),

            hram::START..hram::END => self.hram.read(address),

            INTERRUPT_ENABLE => self.interrupt_enable,

            _ => unreachable!(),
        }
    }

    pub fn update_timers(&mut self, cycles: u8) -> u8 {
        self.timer.update(cycles.into())
    }
}
