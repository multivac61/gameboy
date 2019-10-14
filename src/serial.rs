use crate::cpu::MemoryAddress;

pub const START: MemoryAddress = 0xFF01;
pub const END: MemoryAddress = 0xFF02;

struct Serial {}

impl Serial {
    pub fn read(&self, address: MemoryAddress) -> u8 {
        unimplemented!()
    }

    pub fn write(&mut self, address: MemoryAddress, data: u8) {
        unimplemented!()
    }
}
