use crate::cpu::MemoryAddress;

const RAM_SIZE: usize = 0x2000;

pub const START: MemoryAddress = 0xC000;
pub const END: MemoryAddress = START + RAM_SIZE;
pub const ECHO_START: MemoryAddress = 0xC000;
pub const ECHO_END: MemoryAddress = ECHO_START + RAM_SIZE;

pub struct Ram {
    ram: Vec<u8>,
}

impl Ram {
    pub fn new() -> Self {
        Ram { ram: vec }
    }

    pub fn read(&self, address: MemoryAddress) -> u8 {
        self.ram[address as usize]
    }

    pub fn write(&mut self, address: MemoryAddress, data: u8) {
        match address {
            0xC000..=0xDDFF => {
                let address = address - INTERNAL_RAM_START;
                self.raw_memory[address as usize] = data;
                self.raw_memory[address as usize + RAM_SIZE] = data;
            }
            0xE000..=0xFDFF => {
                let address = address - INTERNAL_RAM_ECHO_START;
                self.raw_memory[address as usize] = data;
                self.raw_memory[address as usize - RAM_SIZE] = data;
            }
            _ => unreachable!(),
        }
    }
}
