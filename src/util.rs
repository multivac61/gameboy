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

pub fn set_bit(n: u8, i: u8) -> u8 {
    n | (1 << i)
}

pub fn clear_bit(n: u8, i: u8) -> u8 {
    n & !(1 << i)
}

#[cfg(test)]
mod test
{
    #[test]
    fn clear_bit() {
        assert_eq!(super::clear_bit(0b1111_1111, 5), 0b1101_1111);
        assert_eq!(super::clear_bit(0b1101_1111, 5), 0b1101_1111);
    }

    #[test]
    fn set_bit() {
        assert_eq!(super::set_bit(0b0000_0000, 3), 0b0000_1000);
        assert_eq!(super::set_bit(0b0000_1000, 3), 0b0000_1000);
    }

    #[test]
    fn ith_bit() {
        for i in 0..8 {
            assert!(super::ith_bit(0xff, i) == true);
            assert!(super::ith_bit(0x00, i) == false);
        }

        assert!(super::ith_bit(0b0010_0000, 5) == true);
        assert!(super::ith_bit(0b0010_0000, 6) == false);
    }
}
