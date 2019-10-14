use crate::cpu::MemoryAddress;

pub const HIGH_RAM_START: MemoryAddress = 0xFF80;
pub const HIGH_RAM_END: MemoryAddress = 0xFFFE;

const HIGH_RAM_SIZE: usize = HIGH_RAM_END - HIGH_RAM_START;

pub struct Hram {
    hram: [u8; HIGH_RAM_SIZE],
}

impl Hram {
    pub fn new() -> Self {
        Hram {
            hram: [0; HIGH_RAM_SIZE],
        }
    }

    pub fn read(&self, address: MemoryAddress) -> u8 {
        self.hram[address as usize]
    }

    pub fn write(&mut self, address: MemoryAddress, data: u8) {}
}
