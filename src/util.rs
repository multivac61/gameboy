pub struct LittleEndian {}

impl LittleEndian {
    pub fn msb(val: u16) -> u8 {
        (val & 0x0F) as u8
    }

    pub fn lsb(val: u16) -> u8 {
        (val >> 8) as u8
    }

    pub fn u8(val: u16) -> (u8, u8) {
        (LittleEndian::lsb(val), LittleEndian::msb(val))
    }

    pub fn u16(lsb: u8, msb: u8) -> u16 {
        (u16::from(msb) << 8) | u16::from(lsb)
    }
}

