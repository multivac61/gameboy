use crate::cpu::MemoryAddress;

pub const START: MemoryAddress = 0xFF01;
pub const END: MemoryAddress = 0xFF02;

pub struct Serial {}

impl Serial {
    pub fn new() -> Self {
        Serial{}
    }

    pub fn read(&self, _address: MemoryAddress) -> u8 {
        // TODO: Implement
        0
    }

    pub fn write(&mut self, _address: MemoryAddress, _data: u8) {
        // TODO: Implement
    }
}
