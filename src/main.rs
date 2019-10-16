use std::fs::File;
use std::io::Read;
use std::thread::sleep;
use std::time::*;
use std::path::Path;

use minifb::{Key, Scale, Window, WindowOptions};
use clap::{App, Arg};

mod apu;
mod cartridge;
mod cpu;
mod hram;
mod instructions;
mod joypad;
mod memory_bus;
mod ppu;
mod ram;
mod registers;
mod serial;
mod timer;
mod util;

const ONE_SECOND_IN_MICROS: usize = 1000000000;
const ONE_FRAME_IN_CYCLES: usize = 70224;

const ENLARGEMENT_FACTOR: usize = 1;
const WINDOW_DIMENSIONS: [usize; 2] = [(160 * ENLARGEMENT_FACTOR), (144 * ENLARGEMENT_FACTOR)];

fn load_binary(filename: &str) -> Vec<u8> {
    let fp = Path::new(filename);
    if !fp.is_file() {
        // TODO: Return error?
        panic!("Must be a file: {}", fp.display());
    }

    let fp = if fp.is_relative() {
        std::env::current_dir().unwrap().join(fp)
    } else {
        fp.to_path_buf()
    };

    let mut file = File::open(fp).expect("Couldn't open boot file");
    let mut buf = Vec::new();
    let _bytes_read = file.read_to_end(&mut buf);

    buf
}

fn main() {
    let matches = App::new("GameBoy")
        .version("0.1")
        .author("Olafur Bogason & Daniel Gretarsson")
        .about("Does awesome things")
        .arg(Arg::with_name("GAME_ROM")
            .help("The game ROM to use")
            .required(true)
            .index(1))
        .arg(Arg::with_name("bootrom")
                 .short("b")
                 .long("bootrom")
                 .value_name("FILE")
                 .help("Nintendo bootstrap ROM that displays the scrolling logo and performs system initalization")
                 .takes_value(true))
        .arg(Arg::with_name("scale")
            .short("s")
            .long("scale")
            .value_name("MULTIPLE_OF_TWO")
            .help("Scale value for the display")
            .takes_value(true))
        .get_matches();

    let game_rom = load_binary(matches.value_of("GAME_ROM").unwrap());
    let boot_rom = match matches.value_of("bootrom") {
        Some(b) => Some(load_binary(b)),
        None => None
    };
    let scale = match matches.value_of("scale") {
        Some(val) if val.parse::<usize>().unwrap() == 1 => Scale::X1,
        Some(val) if val.parse::<usize>().unwrap() == 2 => Scale::X2,
        Some(val) if val.parse::<usize>().unwrap() == 4 => Scale::X4,
        Some(val) if val.parse::<usize>().unwrap() == 8 => Scale::X8,
        Some(val) if val.parse::<usize>().unwrap() == 16 => Scale::X16,
        Some(val) if val.parse::<usize>().unwrap() == 32 => Scale::X32,
        _ => Scale::X4
    };

    let mut cpu = cpu::Cpu::new(game_rom.as_slice(), boot_rom);

    let mut prev_keys = [false; 8];

    let mut window = Window::new(
        "DMG-01",
        WINDOW_DIMENSIONS[0],
        WINDOW_DIMENSIONS[1],
        WindowOptions {
            borderless: false,
            title: false,
            resize: false,
            scale,
        },
    )
    .unwrap();

    let mut cycles_elapsed_in_frame = 0usize;
    let mut now = Instant::now();
    while window.is_open() && !window.is_key_down(Key::Escape) {
        let time_delta = now.elapsed().subsec_nanos();
        now = Instant::now();
        let delta = time_delta as f64 / ONE_SECOND_IN_MICROS as f64;

        let cur_keys = [
            window.is_key_down(Key::A),
            window.is_key_down(Key::S),
            window.is_key_down(Key::Z),
            window.is_key_down(Key::X),
            window.is_key_down(Key::Right),
            window.is_key_down(Key::Left),
            window.is_key_down(Key::Down),
            window.is_key_down(Key::Up),
        ];

        prev_keys
            .iter()
            .zip(cur_keys.iter())
            .enumerate()
            .for_each(|(i, (prev, cur))| match (cur, prev) {
                (true, false) => cpu.key_down(joypad::Key::from(i)),
                (false, true) => cpu.key_up(joypad::Key::from(i)),
                _ => {}
            });

        prev_keys = cur_keys;

        let cycles_elapsed = cpu.cycle(delta) as usize;
        cycles_elapsed_in_frame += cycles_elapsed;

        // TODO: Consider updating buffer after every line is rendered.
        if cycles_elapsed_in_frame >= ONE_FRAME_IN_CYCLES {
            window.update_with_buffer(&cpu.mem.ppu.frame_buffer).unwrap();
            cycles_elapsed_in_frame = 0;
        } else {
            sleep(Duration::from_nanos(2))
        }
    }
}
