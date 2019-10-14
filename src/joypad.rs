use crate::util;

#[derive(Debug, Copy, Clone)]
pub enum Key {
    Select,
    Start,
    A,
    B,
    Right,
    Left,
    Down,
    Up
}

impl std::convert::From<usize> for Key {
    fn from(n: usize) -> Key {
        match n {
            0 => Key::Select,
            1 => Key::Start,
            2 => Key::A,
            3 => Key::B,
            4 => Key::Right,
            5 => Key::Left,
            6 => Key::Down,
            7 => Key::Up,
            _ => unreachable!(),
        }
    }
}

pub struct Joypad {
    direction_state: u8,
    key_state: u8,
    register_state: u8
}

impl Joypad {
    pub fn new() -> Self {
        Joypad{
            direction_state: 0b0000_1111,
            key_state: 0b0000_1111,
            register_state: 0b0011_0000
        }
    }

    fn get_in_bit_num(key: Key) -> u8 {
        match key {
            Key::A | Key::Right => 0,
            Key::B | Key::Left => 1,
            Key::Select | Key::Up => 2,
            Key::Start | Key::Down => 3,
        }
    }

    fn get_out_bit_num(key: Key) -> u8 {
        match key {
            Key::A | Key::B | Key::Select | Key::Start => 5,
            Key::Right | Key::Left | Key::Up | Key::Down => 4,
        }
    }

    pub fn key_up(&mut self, key: Key) {
        match key {
            Key::A | Key::B | Key::Select | Key::Start =>
                self.key_state = util::set_bit(self.key_state, Joypad::get_in_bit_num(key)),
            Key::Right | Key::Left | Key::Up | Key::Down =>
                self.direction_state = util::set_bit(self.direction_state, Joypad::get_in_bit_num(key)),
        }
    }

    pub fn key_down(&mut self, key: Key) {
        match key {
            Key::A | Key::B | Key::Select | Key::Start =>
                self.key_state = util::clear_bit(self.key_state, Joypad::get_in_bit_num(key)),
            Key::Right | Key::Left | Key::Up | Key::Down =>
                self.direction_state = util::clear_bit(self.direction_state, Joypad::get_in_bit_num(key)),
        }
    }

    pub fn get_state(&self) -> u8 {
        match self.register_state & 0x30 {
            0x10 => self.register_state | self.key_state,
            0x20 => self.register_state | self.direction_state,
            _ => self.register_state | 0b1111,
        }
    }

    pub fn set_state(&mut self, new_state: u8) {
        self.register_state = new_state & 0x30;
    }
}

#[cfg(test)]
mod test
{
    use crate::joypad::{Joypad, Key};

    #[test]
    fn key_down()
    {
        let mut jp = Joypad::new();

        assert_eq!(jp.get_state(), 0b0011_1111);
        jp.set_state(0b0001_0000);
        jp.key_down(Key::A);
        assert_eq!(jp.get_state(), 0b0001_1110);

        jp.key_up(Key::A);
        assert_eq!(jp.get_state(), 0b0001_1111);

        jp.set_state(0b0010_0000);
        jp.key_down(Key::Right);
        assert_eq!(jp.get_state(), 0b0010_1110);

        jp.key_up(Key::Right);
        assert_eq!(jp.get_state(), 0b0010_1111);
    }

}

