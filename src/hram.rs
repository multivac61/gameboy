use crate::cpu::MemoryAddress;

pub const START: MemoryAddress = 0xFF80;
pub const END: MemoryAddress = 0xFFFE;

const SIZE: usize = (END - START) as usize + 1;

pub struct Hram {
    hram: [u8; SIZE],
}

impl Hram {
    pub fn new() -> Self {
        Hram {
            hram: [0; SIZE],
        }
    }

    pub fn read(&self, address: MemoryAddress) -> u8 {
        self.hram[(address - START) as usize]
    }

    pub fn write(&mut self, address: MemoryAddress, data: u8) {
        self.hram[(address - START) as usize] = data
    }
}
