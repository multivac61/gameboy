# Game Boy 
This is a work-in-progress Game Boy (DMG-01) emulator written as an introduction and an exercise in the [Rust programing language](https://www.rust-lang.org).

### Building and running
This is pretty straight forward, thanks to the awesome toolchain support provided by the Rust team. 

Once you've installed the Rust toolchain, you can issue `cargo build --release` in the project root. When you run the executable, provide a game ROM as the first parameter. You can optionally pass in your copy of the [Nintendo Game Boy bootstrap ROM](https://wowroms.com/en/roms/nintendo-gameboy/bios-nintendo-game-boy-boot-rom-world/10446.html) via the `--bootrom` switch.

The original Game Boy's display has a resolution of a whopping 160x144 pixels. The default scale factor is 4, but this can be changed with the `--scale` switch. 

### Currently implemented
* Full CPU instruction set
* Interrupts
* Graphics emulation (background, sprites, scrolling)
* Memory Bank Controllers 1 & 2
* Keypad input
* Timer

### Yet to implement
* Audio Processing Unit
* Serial IO
* ...

### ROMs
These are the cartridge ROMs we've tested
* [Tetris](https://github.com/AntonioND/giibiiadvance/blob/master/docs/TCAGBD.pdf)
* [PacMan](https://wowroms.com/en/roms/nintendo-gameboy/pac-man-usa/9827.html)
* [Super Mario Land](https://wowroms.com/en/roms/nintendo-gameboy/super-mario-land-world/10202.html)
* [The Legend of Zelda - Link's Awakening](https://wowroms.com/en/roms/nintendo-gameboy/legend-of-zelda-the-links-awakening-germany/9554.html)

### References:
* [Game Boy CPU Manual](http://marc.rawer.de/Gameboy/Docs/GBCPUman.pdf)
* [Game Boy PAN docs](http://problemkaputt.de/pandocs.htm)
* [Game Boy Instruction Set](https://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html)
* [The Cycle-Accurate Game Boy Docs](https://github.com/AntonioND/giibiiadvance/blob/master/docs/TCAGBD.pdf)
* [The Ultimate Game Boy Talk (33c3)](https://www.youtube.com/watch?v=HyzD8pNlpwI)
