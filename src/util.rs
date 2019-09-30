pub(crate) mod little_endian {
    pub fn lsb(val: u16) -> u8 {
        (val & 0xFF) as u8
    }

    pub fn msb(val: u16) -> u8 {
        (val >> 8) as u8
    }

    pub fn u8(val: u16) -> (u8, u8) {
        (lsb(val), msb(val))
    }

    pub fn u16(lsb: u8, msb: u8) -> u16 {
        (u16::from(msb) << 8) | u16::from(lsb)
    }
}

pub fn ith_bit(n: u8, i: u8) -> bool {
    assert!(i <= 7);
    n & (1 << i) == (1 << i)
}

