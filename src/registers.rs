use crate::util::{ith_bit, little_endian};

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ConditionalFlag {
    Zero = 7,
    Subtract = 6,
    HalfCarry = 5,
    Carry = 4,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Flags {
    pub z: bool,
    pub n: bool,
    pub h: bool,
    pub c: bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Register {
    B,
    C,
    D,
    E,
    H,
    L,
    A,
    F,
}

impl std::convert::From<u8> for Register {
    fn from(n: u8) -> Register {
        match n {
            0 => Register::B,
            1 => Register::C,
            2 => Register::D,
            3 => Register::E,
            4 => Register::H,
            5 => Register::L,
            6 => Register::A,
            7 => Register::F,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Register16bit {
    BC = Register::B as isize,
    DE = Register::D as isize,
    HL = Register::H as isize,
    AF = Register::A as isize,
}

#[derive(Debug, Clone, Copy)]
pub struct Registers {
    reg: [u8; 8],
}

impl Registers {
    pub fn new() -> Self {
        Registers {
            reg: [0x00, 0x13, 0x00, 0xD8, 0x01, 0x4D, 0x01, 0xB0],
        }
    }

    pub fn write_word(&mut self, r: Register16bit, val: u16) {
        let lsb_mask = match r {
            Register16bit::AF => 0xF0,
            _ => 0xFF,
        };

        self.reg[r as usize] = little_endian::msb(val);
        self.reg[r as usize + 1] = little_endian::lsb(val) & lsb_mask;
    }

    pub fn read_word(&self, r: Register16bit) -> u16 {
        u16::from_be_bytes([self.reg[r as usize], self.reg[r as usize + 1]])
    }

    pub fn write(&mut self, r: Register, val: u8) {
        #[cfg(release)]
        assert_ne!(r, Register::F);
        self.reg[r as usize] = val;
    }

    pub fn read(&self, r: Register) -> u8 {
        #[cfg(release)]
        assert_ne!(r, Register::F);
        self.reg[r as usize]
    }

    pub fn set_flag(&mut self, flag: ConditionalFlag, state: bool) {
        self.reg[Register::F as usize] = if state {
            self.reg[Register::F as usize] | (1 << flag as u8)
        } else {
            self.reg[Register::F as usize] & !(1 << flag as u8)
        };
    }

    pub fn get_flag(&self, flag: ConditionalFlag) -> bool {
        ith_bit(self.reg[Register::F as usize], flag as u8)
    }

    pub fn set_flags(&mut self, flags: Flags) {
        self.set_flag(ConditionalFlag::Zero, flags.z);
        self.set_flag(ConditionalFlag::Subtract, flags.n);
        self.set_flag(ConditionalFlag::HalfCarry, flags.h);
        self.set_flag(ConditionalFlag::Carry, flags.c);
    }

    pub fn get_flags(&self) -> Flags {
        Flags {
            z: self.get_flag(ConditionalFlag::Zero),
            n: self.get_flag(ConditionalFlag::Subtract),
            h: self.get_flag(ConditionalFlag::HalfCarry),
            c: self.get_flag(ConditionalFlag::Carry),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::registers::{ConditionalFlag, Register, Register16bit, Registers};

    #[test]
    fn wide_registers() {
        let mut reg = Registers::new();
        reg.write(Register::A, 0x12);
        reg.write(Register::F, 0x23);
        reg.write(Register::B, 0x34);
        reg.write(Register::C, 0x45);
        reg.write(Register::D, 0x56);
        reg.write(Register::E, 0x67);
        reg.write(Register::H, 0x78);
        reg.write(Register::L, 0x89);
        assert_eq!(reg.read_word(Register16bit::AF), 0x1223);
        assert_eq!(reg.read_word(Register16bit::BC), 0x3445);
        assert_eq!(reg.read_word(Register16bit::DE), 0x5667);
        assert_eq!(reg.read_word(Register16bit::HL), 0x7889);

        assert_eq!(reg.read(Register::A), 0x12);
        assert_eq!(reg.read(Register::F), 0x23);
        assert_eq!(reg.read(Register::B), 0x34);
        assert_eq!(reg.read(Register::C), 0x45);
        assert_eq!(reg.read(Register::D), 0x56);
        assert_eq!(reg.read(Register::E), 0x67);
        assert_eq!(reg.read(Register::H), 0x78);
        assert_eq!(reg.read(Register::L), 0x89);

        reg.write_word(Register16bit::AF, 0x1111);
        reg.write_word(Register16bit::BC, 0x1111);
        reg.write_word(Register16bit::DE, 0x1111);
        reg.write_word(Register16bit::HL, 0x1111);
        assert_eq!(reg.read_word(Register16bit::AF), 0x1111);
        assert_eq!(reg.read_word(Register16bit::BC), 0x1111);
        assert_eq!(reg.read_word(Register16bit::DE), 0x1111);
        assert_eq!(reg.read_word(Register16bit::HL), 0x1111);
    }

    #[test]
    fn flags() {
        let mut r = Registers::new();
        let flags = [
            ConditionalFlag::Zero,
            ConditionalFlag::Subtract,
            ConditionalFlag::HalfCarry,
            ConditionalFlag::Carry,
        ];

        // Check if initially the flags are good
        assert_eq!(r.read(Register::F) & 0x0F, 0);

        r.write(Register::F, 0);
        for f in &flags {
            assert_eq!(r.get_flag(*f), false);
            r.set_flag(*f, true);
            assert_eq!(r.get_flag(*f), true);
            r.set_flag(*f, false);
            assert_eq!(r.get_flag(*f), false);
        }
    }
}
