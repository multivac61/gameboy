use crate::cpu::MemoryAddress;

const NR10: MemoryAddress = 0xFF10;
const NR11: MemoryAddress = 0xFF11;
const NR12: MemoryAddress = 0xFF12;
const NR13: MemoryAddress = 0xFF13;
const NR14: MemoryAddress = 0xFF14;
const NR21: MemoryAddress = 0xFF16;
const NR22: MemoryAddress = 0xFF17;
const NR23: MemoryAddress = 0xFF18;
const NR24: MemoryAddress = 0xFF19;
const NR30: MemoryAddress = 0xFF1A;
const NR31: MemoryAddress = 0xFF1B;
const NR32: MemoryAddress = 0xFF1C;
const NR33: MemoryAddress = 0xFF1D;
const NR34: MemoryAddress = 0xFF1E;
const NR41: MemoryAddress = 0xFF20;
const NR42: MemoryAddress = 0xFF21;
const NR43: MemoryAddress = 0xFF22;
const NR44: MemoryAddress = 0xFF23;
const NR50: MemoryAddress = 0xFF24;
const NR51: MemoryAddress = 0xFF25;
const NR52: MemoryAddress = 0xFF26;
// ...
const WAVE_PATTERN_RAM_START: MemoryAddress = 0xFF30;
const WAVE_PATTERN_RAM_SIZE: usize = 16; // 32 * 4 bits
const WAVE_PATTERN_RAM_END: MemoryAddress = WAVE_PATTERN_RAM_START + WAVE_PATTERN_RAM_SIZE as MemoryAddress - 1;

pub const START: MemoryAddress = NR10;
pub const END: MemoryAddress = WAVE_PATTERN_RAM_END;

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

    pub fn read(&self, _address: MemoryAddress) -> u8 {
        // TODO: Implement
        0
    }

    pub fn write(&mut self, _address: MemoryAddress, _data: u8) {
        // TODO: Implement
    }
}
