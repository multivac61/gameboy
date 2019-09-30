use std::fs::File;
use std::io::Read;

use piston_window::*;

mod instructions;
mod memory_bus;
mod cpu;
mod util;

fn main() {
    let file_path = std::env::current_dir() .unwrap()
        .join(std::path::Path::new("src"))
        .join(std::path::Path::new("Tetris.gb"));

    println!("{:?}", file_path);
    let mut file = File::open(file_path)
        .expect("There was an issue opening the file");
//    let mut file = File::open("/home/horigome/dev/rust/gb-test-roms/cpu_instrs/cpu_instrs.gb")
//        .expect("There was an issue opening the file");
    let mut buffer = Vec::new();
    let _bytes_read = file.read_to_end(&mut buffer);

    let mut cpu = cpu::Cpu::new(buffer.as_slice());

    const GUI_SCALE: f64 = 5.0;
    let window_dimensions = [cpu::VIDEO_WIDTH * GUI_SCALE as u32, cpu::VIDEO_HEIGHT * GUI_SCALE as u32];
    let mut window: PistonWindow = WindowSettings::new("Chip-8 Interpreter", window_dimensions)
        .exit_on_esc(true)
        .build()
        .expect("Failed to create a piston window");

    while let Some(e) = window.next() {
        if e.render_args().is_some() {
            window.draw_2d(&e, |context, graphics, _| {
                piston_window::clear(color::BLACK, graphics);

                for (i, row) in cpu.display.iter().enumerate() {
                    for (j, &val) in row.iter().enumerate() {
                        if val > 0 {
                            let d = [
                                j as f64 * GUI_SCALE,
                                i as f64 * GUI_SCALE,
                                GUI_SCALE,
                                GUI_SCALE,
                            ];
                            Rectangle::new(color::grey(f32::from(val) / 255.0)).draw(
                                d,
                                &context.draw_state,
                                context.transform,
                                graphics,
                            );
                        }
                    }
                }
            });
        }

        if let Some(u) = e.update_args() {
            cpu.cycle(u.dt);
        }

        //        if let Some(Button::Keyboard(key)) = e.release_args() {
        //            cpu.handle_key(key.code(), false);
        //        }
        //
        //        if let Some(Button::Keyboard(key)) = e.press_args() {
        //            cpu.handle_key(key.code(), true);
        //
    }
}
