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
    state: u8
}

impl Joypad {
    pub fn new() -> Self {
        Joypad{
            state: 0b0011_1111
        }
    }

    fn get_mask(key: Key) -> u8 {
        match key {
            Key::A | Key::Right => 1 << 0,
            Key::B | Key::Left => 1 << 1,
            Key::Select | Key::Up => 1 << 2,
            Key::Start | Key::Down => 1 << 3,
        }
    }

    pub fn key_up(&mut self, key: Key) {
        self.state |= Joypad::get_mask(key);
    }

    pub fn key_down(&mut self, key: Key) {
        self.state &= !Joypad::get_mask(key);
    }

    pub fn get_state(&self) -> u8 {
        self.state
    }

    pub fn set_state(&mut self, new_state: u8) {
        self.state = (new_state & 0b00110000) | (self.state & 0b00001111);
    }
}
