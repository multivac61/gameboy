use std::fs::File;
use std::io::Read;
use std::thread::sleep;
use std::time::*;

use minifb::{Key, Scale, Window, WindowOptions};

mod cpu;
mod instructions;
mod memory_bus;
mod util;
mod registers;
mod joypad;

const ONE_SECOND_IN_MICROS: usize = 1000000000;
const ONE_SECOND_IN_CYCLES: usize = 4190000;
const ONE_FRAME_IN_CYCLES: usize = 70224;
const NUMBER_OF_PIXELS: usize = 23040;

const ENLARGEMENT_FACTOR: usize = 1;
const WINDOW_DIMENSIONS: [usize; 2] = [(160 * ENLARGEMENT_FACTOR), (144 * ENLARGEMENT_FACTOR)];

fn main() {
    let file_name = std::env::args().nth(1);
    let file_path = std::env::current_dir()
        .unwrap()
        .join(file_name.unwrap());
//        .join(std::path::Path::new("src"))
//        .join(std::path::Path::new("Tetris.gb"));
//        .join(std::path::Path::new("Dr. Mario (World).gb"));
//        .join(std::path::Path::new(".."))
//        .join(std::path::Path::new("gb-test-roms"))
//        .join(std::path::Path::new("cpu_instrs"))
//        .join(std::path::Path::new("cpu_instrs.gb"));
//        .join(std::path::Path::new("06-ld r,r.gb"));
//        .join(std::path::Path::new("individual"))
//        .join(std::path::Path::new("01-special.gb"));
//        .join(std::path::Path::new("02-interrupts.gb"));
//            .join(std::path::Path::new("03-op sp,hl.gb"));
//    .join(std::path::Path::new("04-op r,imm.gb"));
//    .join(std::path::Path::new("05-op rp.gb"));
//    .join(std::path::Path::new("06-ld r,r.gb"));
//    .join(std::path::Path::new("07-jr,jp,call,ret,rst.gb"));
//    .join(std::path::Path::new("08-misc instrs.gb"));
//    .join(std::path::Path::new("09-op r,r.gb"));
//    .join(std::path::Path::new("10-bit ops.gb"));
//    .join(std::path::Path::new("11-op a,(hl).gb"));

//        let file_path = std::path::Path::new("/home/dingari/vblank_stat_intr-C.gb");

    let mut prev_keys = [false; 8];

    println!("{:?}", file_path);
    let mut file = File::open(file_path).expect("There was an issue opening the file");
    let mut buffer = Vec::new();
    let _bytes_read = file.read_to_end(&mut buffer);

    let mut cpu = cpu::Cpu::new(buffer.as_slice(), false);

    let mut window = Window::new(
        "DMG-01",
        WINDOW_DIMENSIONS[0],
        WINDOW_DIMENSIONS[1],
        WindowOptions {
            borderless: false,
            title: false,
            resize: false,
            scale: Scale::X4,
        },
    ).unwrap();

    let mut cycles_elapsed_in_frame = 0usize;
    let mut now = Instant::now();
    while window.is_open() && !window.is_key_down(Key::Escape) {
        let time_delta = now.elapsed().subsec_nanos();
        now = Instant::now();
        let delta = time_delta as f64 / ONE_SECOND_IN_MICROS as f64;

        let cur_keys = [
            window.is_key_down(Key::Key0),
            window.is_key_down(Key::Key1),
            window.is_key_down(Key::Key2),
            window.is_key_down(Key::Key3),
            window.is_key_down(Key::Key4),
            window.is_key_down(Key::Key5),
            window.is_key_down(Key::Key6),
            window.is_key_down(Key::Key7)
        ];

        prev_keys.iter().zip(cur_keys.iter()).enumerate().for_each(|(i, (prev, cur))|
            match (cur, prev) {
                (true, false) => cpu.key_down(joypad::Key::from(i)),
                (false, true) => cpu.key_up(joypad::Key::from(i)),
                _ => {}
            }
        );

        prev_keys = cur_keys;

        let cycles_elapsed = cpu.cycle(delta) as usize;
        cycles_elapsed_in_frame += cycles_elapsed;

        // TODO: Consider updating buffer after every line is rendered.
        if cycles_elapsed_in_frame >= ONE_FRAME_IN_CYCLES {
            window.update_with_buffer(&cpu.display).unwrap();
            cycles_elapsed_in_frame = 0;
        } else {
            sleep(Duration::from_nanos(2))
        }
    }
}

