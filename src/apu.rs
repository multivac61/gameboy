use crate::cpu::MemoryAddress;

pub const START: MemoryAddress = 0xFF10;
pub const END: MemoryAddress = 0xFF30;

pub struct Apu {
    ram: Vec<u8>,
}

impl Apu {
    pub fn new() -> Self {
        //        m[0xFF10] = 0x80;
        //        m[0xFF11] = 0xBF;
        //        m[0xFF12] = 0xF3;
        //        m[0xFF14] = 0xBF;
        //        m[0xFF16] = 0x3F;
        //        m[0xFF17] = 0x00;
        //        m[0xFF19] = 0xBF;
        //        m[0xFF1A] = 0x7F;
        //        m[0xFF1B] = 0xFF;
        //        m[0xFF1C] = 0x9F;
        //        m[0xFF1E] = 0xBF;
        //        m[0xFF20] = 0xFF;
        //        m[0xFF21] = 0x00;
        //        m[0xFF22] = 0x00;
        //        m[0xFF23] = 0xBF;
        //        m[0xFF24] = 0x77;
        //        m[0xFF25] = 0xF3;
        //        m[0xFF26] = 0xF1;
        Apu { ram: vec![0; 100] }
    }

    pub fn read(&self, address: MemoryAddress) -> u8 {
        unimplemented!()
    }

    pub fn write(&mut self, address: MemoryAddress, data: u8) {
        unimplemented!()
    }
}
