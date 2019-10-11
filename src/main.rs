use std::fs::File;
use std::io::Read;
use std::thread::sleep;
use std::time::*;

use minifb::{Key, Window, WindowOptions};

mod cpu;
mod instructions;
mod memory_bus;
mod util;
mod registers;
//mod serial;

const ONE_SECOND_IN_MICROS: usize = 1000000000;
const ONE_SECOND_IN_CYCLES: usize = 4190000;
const ONE_FRAME_IN_CYCLES: usize = 70224;
const NUMBER_OF_PIXELS: usize = 23040;

const ENLARGEMENT_FACTOR: usize = 1;
const WINDOW_DIMENSIONS: [usize; 2] = [(160 * ENLARGEMENT_FACTOR), (144 * ENLARGEMENT_FACTOR)];

fn main() {
    let file_path = std::env::current_dir()
        .unwrap()
        .join(std::path::Path::new("src"))
        .join(std::path::Path::new("Tetris.gb"));
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

    println!("{:?}", file_path);
    let mut file = File::open(file_path).expect("There was an issue opening the file");
    let mut buffer = Vec::new();
    let _bytes_read = file.read_to_end(&mut buffer);

    let mut cpu = cpu::Cpu::new(buffer.as_slice());

    let mut window = Window::new(
        "DMG-01",
        WINDOW_DIMENSIONS[0],
        WINDOW_DIMENSIONS[1],
        WindowOptions::default(),
    )
    .unwrap();

    let mut framebuffer = vec![0u32; WINDOW_DIMENSIONS[0] * WINDOW_DIMENSIONS[1]];
    let mut cycles_elapsed_in_frame = 0usize;
    let mut now = Instant::now();
    while window.is_open() && !window.is_key_down(Key::Escape) {
        let time_delta = now.elapsed().subsec_nanos();
        now = Instant::now();
        let delta = time_delta as f64 / ONE_SECOND_IN_MICROS as f64;

        let cycles_elapsed = cpu.cycle(delta) as usize;
        cycles_elapsed_in_frame += cycles_elapsed;

        // TODO: Consider updating buffer after every line is rendered.
        if cycles_elapsed_in_frame >= ONE_FRAME_IN_CYCLES {
            for (i, pixel) in cpu.display.iter().enumerate() {
                framebuffer[i] = *pixel;
            }

            window.update_with_buffer(&framebuffer).unwrap();
            cycles_elapsed_in_frame = 0;
        } else {
            sleep(Duration::from_nanos(2))
        }
    }
}
