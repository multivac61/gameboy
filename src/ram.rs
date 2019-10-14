use crate::cpu::MemoryAddress;

const RAM_SIZE: usize = 0x2000;

pub const START: MemoryAddress = 0xC000;
pub const END: MemoryAddress = START + RAM_SIZE as MemoryAddress;
pub const ECHO_START: MemoryAddress = 0xC000;
pub const ECHO_END: MemoryAddress = ECHO_START + RAM_SIZE as MemoryAddress - 1;

pub struct Ram {
    ram: [u8; RAM_SIZE],
}

impl Ram {
    pub fn new() -> Self {
        Ram { ram: [0; RAM_SIZE] }
    }

    pub fn read(&self, address: MemoryAddress) -> u8 {
        self.ram[address as usize]
    }

    pub fn write(&mut self, address: MemoryAddress, data: u8) {
        match address {
            0xC000..=0xDDFF => {
                let address = address - START;
                self.ram[address as usize] = data;
                self.ram[address as usize + RAM_SIZE] = data;
            }
            0xE000..=0xFDFF => {
                let address = address - ECHO_START;
                self.ram[address as usize] = data;
                self.ram[address as usize - RAM_SIZE] = data;
            }
            _ => unreachable!(),
        }
    }
}
