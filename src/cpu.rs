use std::fmt;

use crate::instructions::Instruction;
use crate::instructions::Instruction::*;
use crate::joypad::Key;
use crate::memory_bus::{MemoryBus, BOOT_ROM_SIZE};
use crate::registers::{ConditionalFlag::*, Flags, Register, Register16bit, Registers};
use crate::util;

const CLOCK_FREQUENCY: f64 = 4_194_304.0;

const INTERRUPT_REQUEST_REGISTER: u16 = 0xFF0F;
const INTERRUPT_ENABLE_REGISTER: u16 = 0xFFFF;
pub const BOOT_ROM_ENABLE_REGISTER: u16 = 0xFF50;

pub type MemoryAddress = u16;

pub enum Interrupt {
    VBlank,
    LCD,
    Timer,
    SerialTxComplete,
    Joypad,
}

impl std::convert::From<u8> for Interrupt {
    fn from(n: u8) -> Interrupt {
        match n {
            0 => Interrupt::VBlank,
            1 => Interrupt::LCD,
            2 => Interrupt::Timer,
            3 => Interrupt::SerialTxComplete,
            4 => Interrupt::Joypad,
            _ => unreachable!(),
        }
    }
}

pub struct Cpu {
    pc: MemoryAddress,
    sp: MemoryAddress,
    is_halted: bool,
    is_stopped: bool,
    are_interrupts_enabled: bool,
    reg: Registers,
    pub mem: MemoryBus,
}

impl Cpu {
    pub fn new(cartridge: &[u8], boot_rom: Option<Vec<u8>>) -> Self {
        Cpu {
            pc: match &boot_rom {
                Some(b) if b.len() == BOOT_ROM_SIZE => 0x000,
                _ => 0x100,
            },
            sp: 0xFFFE,
            is_halted: false,
            is_stopped: false,
            are_interrupts_enabled: false,
            reg: Registers::new(),
            mem: MemoryBus::new(cartridge, boot_rom),
        }
    }

    pub fn key_up(&mut self, key: Key) {
        self.mem.joypad.key_up(key);
    }

    pub fn key_down(&mut self, key: Key) {
        self.mem.joypad.key_down(key);

        self.mem.interrupt_flag = util::set_bit(self.mem.interrupt_flag, Interrupt::Joypad as u8);
        self.is_halted = false;
    }

    fn do_interrupts(&mut self) -> u8 {
        let is_requested = self.mem.read(INTERRUPT_REQUEST_REGISTER);
        let is_enabled = self.mem.read(INTERRUPT_ENABLE_REGISTER);
        let is_requested_and_enabled = is_requested & is_enabled;

        for i in 0..5 {
            if util::ith_bit(is_requested_and_enabled, i) && self.are_interrupts_enabled {
                self.are_interrupts_enabled = false;

                let not_requested_anymore = util::clear_bit(is_requested, i);
                self.mem
                    .write(INTERRUPT_REQUEST_REGISTER, not_requested_anymore);

                self.push_stack(self.pc);

                self.pc = match Interrupt::from(i) {
                    Interrupt::VBlank => 0x40,
                    Interrupt::LCD => 0x48,
                    Interrupt::Timer => 0x50,
                    Interrupt::SerialTxComplete => 0x58,
                    Interrupt::Joypad => 0x60,
                };

                return 16;
            }
        }
        0
    }

    pub fn cycle(&mut self, s: f64) -> usize {
        let total_cycles = (s * CLOCK_FREQUENCY).round() as u64;
        let mut cur_num_cycles = 0;

        while cur_num_cycles < total_cycles {
            let program_cycles = if !self.is_halted {
                #[cfg(debug_assertions)]
                println!("{:?}", self);

                let (instruction, pc_increments) = self.fetch();
                self.pc += pc_increments;
                self.execute_instruction(instruction)
            } else {
                4
            };

            let interrupt_cycles = self.do_interrupts();
            let cycles = program_cycles + interrupt_cycles;

            self.mem.interrupt_flag |= self.mem.timer.update(cycles);
            self.mem.interrupt_flag |= self.mem.ppu.update(cycles);

            if self.mem.interrupt_flag > 0 {
                self.is_halted = false;
            }

            cur_num_cycles += cycles as u64;
        }

        total_cycles as usize
    }

    fn push_stack(&mut self, val: u16) {
        self.sp -= 2;
        self.mem.write_word(self.sp, val);
    }

    fn pop_stack(&mut self) -> u16 {
        let val = self.mem.read_word(self.sp);
        self.sp += 2;

        val
    }

    fn alu_add16(&mut self, a: u16, b: u16) -> u16 {
        let r = a.wrapping_add(b);

        self.reg.set_flags(Flags {
            z: self.reg.get_flag(Zero),
            n: false,
            h: (a & 0x7FF) + (b & 0x7FF) > 0x7FF,
            c: a > 0xFFFF - b,
        });

        r
    }

    fn alu_add(&mut self, b: u8, use_carry: bool) {
        let a = self.reg.read(Register::A);
        let c = if use_carry {
            self.reg.get_flag(Carry) as u8
        } else {
            0
        };
        let r = a.wrapping_add(b).wrapping_add(c);
        self.reg.write(Register::A, r);
        self.reg.set_flags(Flags {
            z: r == 0,
            n: false,
            h: (a & 0xF) + (b & 0xF) + c > 0xF,
            c: a as u16 + b as u16 + c as u16 > 0xFF,
        });
    }

    fn alu_cmp(&mut self, b: u8) {
        let a = self.reg.read(Register::A);
        self.alu_sub(b, false);
        self.reg.write(Register::A, a);
    }

    fn alu_sub(&mut self, b: u8, use_carry: bool) -> u8 {
        let a = self.reg.read(Register::A);
        let c = if use_carry {
            self.reg.get_flag(Carry) as u8
        } else {
            0
        };
        let r = a.wrapping_sub(b).wrapping_sub(c);
        self.reg.write(Register::A, r);
        self.reg.set_flags(Flags {
            z: r == 0,
            n: true,
            h: (a & 0x0F) < (b & 0x0F) + c,
            c: (a as u16) < (b as u16) + (c as u16),
        });
        r
    }

    fn alu_and(&mut self, b: u8) {
        let a = self.reg.read(Register::A);
        self.reg.write(Register::A, a & b);
        self.reg.set_flags(Flags {
            z: self.reg.read(Register::A) == 0,
            n: false,
            h: true,
            c: false,
        });
    }

    fn alu_or(&mut self, b: u8) {
        let a = self.reg.read(Register::A);
        self.reg.write(Register::A, a | b);
        self.reg.set_flags(Flags {
            z: self.reg.read(Register::A) == 0,
            n: false,
            h: false,
            c: false,
        });
    }

    fn alu_xor(&mut self, b: u8) {
        let a = self.reg.read(Register::A);
        self.reg.write(Register::A, a ^ b);
        self.reg.set_flags(Flags {
            z: self.reg.read(Register::A) == 0,
            n: false,
            h: false,
            c: false,
        });
    }

    fn alu_inc(&mut self, a: u8) -> u8 {
        let r = a.wrapping_add(1);
        self.reg.set_flags(Flags {
            z: r == 0,
            n: false,
            h: (a & 0xF) + 1 > 0xF,
            c: self.reg.get_flag(Carry),
        });
        r
    }

    fn alu_dec(&mut self, a: u8) -> u8 {
        let r = a.wrapping_sub(1);
        self.reg.set_flags(Flags {
            z: r == 0,
            n: true,
            h: a & 0xF == 0,
            c: self.reg.get_flag(Carry),
        });
        r
    }

    fn alu_rotate_left(&mut self, x: u8, use_carry: bool) -> u8 {
        let c = if use_carry {
            self.reg.get_flag(Carry)
        } else {
            util::ith_bit(x, 7)
        };
        let r = (x << 1) | c as u8;
        self.reg.set_flags(Flags {
            z: false,
            n: false,
            h: false,
            c: util::ith_bit(x, 7),
        });
        r
    }

    fn alu_rotate_right(&mut self, x: u8, use_carry: bool) -> u8 {
        let c = if use_carry {
            self.reg.get_flag(Carry)
        } else {
            util::ith_bit(x, 0)
        };
        let r = (x >> 1) | ((c as u8) << 7);
        self.reg.set_flags(Flags {
            z: false,
            n: false,
            h: false,
            c: util::ith_bit(x, 0),
        });
        r
    }

    fn alu_shift_left(&mut self, x: u8) -> u8 {
        let carry = util::ith_bit(x, 7);
        let r = x << 1;
        self.reg.set_flags(Flags {
            z: r == 0,
            n: false,
            h: false,
            c: carry,
        });
        r
    }

    fn alu_shift_right(&mut self, x: u8, is_arithmetic: bool) -> u8 {
        let sign_bit = if is_arithmetic {
            util::ith_bit(x, 7) as u8
        } else {
            0
        };
        let r = (x >> 1) | ((sign_bit as u8) << 7);
        self.reg.set_flags(Flags {
            z: r == 0,
            n: false,
            h: false,
            c: util::ith_bit(x, 0),
        });
        r
    }

    fn alu_swap_nibbles(&mut self, x: u8) -> u8 {
        let r = (x << 4) | (x >> 4);
        self.reg.set_flags(Flags {
            z: r == 0,
            n: false,
            h: false,
            c: false,
        });
        r
    }

    fn alu_test_bit(&mut self, x: u8, bit: u8) {
        assert!(bit <= 7);
        self.reg.set_flags(Flags {
            z: !util::ith_bit(x, bit),
            n: false,
            h: true,
            c: self.reg.get_flag(Carry),
        });
    }

    fn alu_set_bit(x: u8, bit: u8) -> u8 {
        assert!(bit <= 7);
        x | (1 << bit)
    }

    fn alu_reset_bit(x: u8, bit: u8) -> u8 {
        assert!(bit <= 7);
        x & !(1 << bit)
    }

    fn alu_daa(&mut self) {
        let mut a = self.reg.read(Register::A);

        if !self.reg.get_flag(Subtract) {
            if self.reg.get_flag(Carry) || self.reg.read(Register::A) > 0x99 {
                a = a.wrapping_add(0x60);
                self.reg.set_flag(Carry, true);
            }

            if self.reg.get_flag(HalfCarry) || self.reg.read(Register::A) & 0x0f > 0x09 {
                a = a.wrapping_add(0x06);
            }
        } else if self.reg.get_flag(Carry) {
            a = a.wrapping_add(if self.reg.get_flag(HalfCarry) {
                0x9a
            } else {
                0xa0
            });
        } else if self.reg.get_flag(HalfCarry) {
            a = a.wrapping_add(0xfa);
        }

        self.reg.set_flag(Zero, a == 0);
        self.reg.set_flag(HalfCarry, false);
        self.reg.write(Register::A, a);
    }

    fn execute_instruction(&mut self, instr: Instruction) -> u8 {
        match instr {
            // GMB 8bit-Load commands
            // ld   r,r         xx         4 ---- r=r
            LoadReg { to, from } => {
                self.reg.write(to, self.reg.read(from));
                4
            }
            // ld   r,n         xx nn      8 ---- r=n
            LoadRegWithConstant { to, n } => {
                self.reg.write(to, n);
                8
            }
            // ld   r,(HL)      xx         8 ---- r=(HL)
            LoadRegWithMemoryHL { to } => {
                let address = self.reg.read_word(Register16bit::HL);
                self.reg.write(to, self.mem.read(address));
                8
            }
            // ld   (HL),r      7x         8 ---- (HL)=r
            LoadMemoryHLwithRegister { from } => {
                let address = self.reg.read_word(Register16bit::HL);
                self.mem.write(address, self.reg.read(from));
                8
            }
            // ld   (HL),n      36 nn     12 ----
            LoadMemoryHLwithConstant { n } => {
                let address = self.reg.read_word(Register16bit::HL);
                self.mem.write(address, n);
                12
            }
            // ld   A,(BC)      0A         8 ----
            LoadAwithValBC => {
                let address = self.reg.read_word(Register16bit::BC);
                self.reg.write(Register::A, self.mem.read(address));
                8
            }
            // ld   A,(DE)      1A         8 ----
            LoadAwithValDE => {
                let address = self.reg.read_word(Register16bit::DE);
                self.reg.write(Register::A, self.mem.read(address));
                8
            }
            // ld   A,(nn)      FA        16 ----
            LoadAwithMemory { nn } => {
                assert!(nn < 0xFFFF);
                self.reg.write(Register::A, self.mem.read(nn));
                16
            }
            // ld   (BC),A      02         8 ----
            LoadMemoryBCwithA => {
                let address = self.reg.read_word(Register16bit::BC);
                self.mem.write(address, self.reg.read(Register::A));
                8
            }
            // ld   (DE),A      12         8 ----
            LoadMemoryDEwithA => {
                let address = self.reg.read_word(Register16bit::DE);
                self.mem.write(address, self.reg.read(Register::A));
                8
            }
            // ld   (nn),A      EA        16 ----
            LoadMemoryNNwithA { nn } => {
                self.mem.write(nn, self.reg.read(Register::A));
                16
            }
            // ld   A,(FF00+n)  F0 nn     12 ---- read from io-port n (memory FF00+n)
            LoadAwithFF00plusN { n } => {
                self.reg
                    .write(Register::A, self.mem.read(u16::from(n) + 0xFF00));
                12
            }
            // ld   (FF00+n),A  E0 nn     12 ---- write to io-port n (memory FF00+n)
            LoadMemoryFF00plusNwithA { nn } => {
                self.mem
                    .write(u16::from(nn) + 0xFF00, self.reg.read(Register::A));
                12
            }
            // ld   A,(FF00+C)  F2         8 ---- read from io-port C (memory FF00+C)
            LoadAwithFF00plusC => {
                let address = 0xFF00 + u16::from(self.reg.read(Register::C));
                self.reg.write(Register::A, self.mem.read(address));
                8
            }
            // ld   (FF00+C),A  E2         8 ---- write to io-port C (memory FF00+C)
            LoadMemoryFF00plusCwithA => {
                let address = 0xFF00 + u16::from(self.reg.read(Register::C));
                self.mem.write(address, self.reg.read(Register::A));

                8
            }
            // ldi  (HL),A      22         8 ---- (HL)=A, HL=HL+1
            LoadMemoryHLwithAandIncr => {
                let address = self.reg.read_word(Register16bit::HL);
                self.mem.write(address, self.reg.read(Register::A));

                self.reg.write_word(Register16bit::HL, address + 1);
                8
            }
            // ldi  A,(HL)      2A         8 ---- A=(HL), HL=HL+1
            LoadAwithValHLandIncr => {
                let address = self.reg.read_word(Register16bit::HL);
                let val = self.mem.read(address);

                self.reg.write(Register::A, val);

                self.reg.write_word(Register16bit::HL, address + 1);
                8
            }
            // ldd  (HL),A      32         8 ---- (HL)=A, HL=HL-1
            LoadMemoryHLwithAandDecr => {
                let address = self.reg.read_word(Register16bit::HL);
                let val = self.reg.read(Register::A);

                self.mem.write(address, val);

                self.reg.write_word(Register16bit::HL, address - 1);
                8
            }
            // ldd  A,(HL)      3A         8 ---- A=(HL), HL=HL-1
            LoadAwithValHLandDecr => {
                let address = self.reg.read_word(Register16bit::HL);
                let val = self.mem.read(address);

                self.reg.write(Register::A, val);

                self.reg.write_word(Register16bit::HL, address - 1);
                8
            }

            // GMB 16bit-Load Commands
            // ld   rr,nn       x1 nn nn  12 ---- rr=nn (rr may be BC,DE,HL or SP)
            LoadRegWith16BitConstant { rr, nn } => {
                self.reg.write_word(rr, nn);
                12
            }
            LoadSPWith16BitConstant { nn } => {
                self.sp = nn;
                12
            }
            // ld   SP,HL       F9         8 ---- SP=HL
            LoadSPwithHL => {
                self.sp = self.reg.read_word(Register16bit::HL);
                8
            }
            // push rr          x5 (c5, d5, e5, f5)        16 ---- SP=SP-2  (SP)=rr   (rr may be BC,DE,HL,AF)
            Push { rr } => {
                self.push_stack(self.reg.read_word(rr));

                16
            }
            // pop  rr          x1 (c1, d1, e1, f1)      12 (AF) rr=(SP)  SP=SP+2   (rr may be BC,DE,HL,AF)
            Pop { rr } => {
                let val = self.pop_stack();
                self.reg.write_word(rr, val);
                12
            }

            // GMB 8bit-Arithmetic/logical Commands
            //  add  A,r         8x         4 z0hc A=A+r
            AddRegister { r } => {
                self.alu_add(self.reg.read(r), false);
                4
            }
            //  add  A,n         C6 nn      8 z0hc A=A+n
            AddConstant { n } => {
                self.alu_add(n, false);
                8
            }
            //  add  A,(HL)      86         8 z0hc A=A+(HL)
            AddMemoryHL => {
                let address = self.reg.read_word(Register16bit::HL);
                self.alu_add(self.mem.read(address), false);
                8
            }
            //  adc  A,r         8x         4 z0hc A=A+r+cy
            AddRegisterWithCarry { r } => {
                self.alu_add(self.reg.read(r), true);
                4
            }
            //  adc  A,n         CE nn      8 z0hc A=A+n+cy
            AddConstantWithCarry { n } => {
                self.alu_add(n, true);
                8
            }
            //  adc  A,(HL)      8E         8 z0hc A=A+(HL)+cy
            AddMemoryHLWithCarry => {
                let address = self.reg.read_word(Register16bit::HL);
                self.alu_add(self.mem.read(address), true);
                8
            }
            //  sub  r           9x         4 z1hc A=A-r
            SubtractRegister { r } => {
                self.alu_sub(self.reg.read(r), false);
                4
            }
            //  sub  n           D6 nn      8 z1hc A=A-n
            SubtractConstant { n } => {
                self.alu_sub(n, false);
                8
            }
            //  sub  (HL)        96         8 z1hc A=A-(HL)
            SubtractMemoryHL => {
                let address = self.reg.read_word(Register16bit::HL);
                self.alu_sub(self.mem.read(address), false);
                8
            }
            //  sbc  A,r         9x         4 z1hc A=A-r-cy
            SubtractRegisterWithCarry { r } => {
                self.alu_sub(self.reg.read(r), true);
                4
            }
            //  sbc  A,n         DE nn      8 z1hc A=A-n-cy
            SubtractConstantWithCarry { n } => {
                self.alu_sub(n, true);
                8
            }
            //  sbc  A,(HL)      9E         8 z1hc A=A-(HL)-cy
            SubtractMemoryHLWithCarry => {
                let address = self.reg.read_word(Register16bit::HL);
                self.alu_sub(self.mem.read(address), true);
                8
            }
            //  and  r           Ax         4 z010 A=A & r
            AndRegister { r } => {
                self.alu_and(self.reg.read(r));
                4
            }
            //  and  n           E6 nn      8 z010 A=A & n
            AndConstant { n } => {
                self.alu_and(n);
                8
            }
            //  and  (HL)        A6         8 z010 A=A & (HL)
            AndMemoryHL => {
                let address = self.reg.read_word(Register16bit::HL);
                self.alu_and(self.mem.read(address));
                8
            }
            //  xor  r           Ax         4 z000
            XorRegister { r } => {
                self.alu_xor(self.reg.read(r));
                4
            }
            //  xor  n           EE nn      8 z000
            XorConstant { n } => {
                self.alu_xor(n);
                8
            }
            //  xor  (HL)        AE         8 z000
            XorMemoryHL => {
                let address = self.reg.read_word(Register16bit::HL);
                self.alu_xor(self.mem.read(address));
                8
            }
            //  or   r           Bx         4 z000 A=A | r
            OrRegister { r } => {
                self.alu_or(self.reg.read(r));
                4
            }
            //  or   n           F6 nn      8 z000 A=A | n
            OrConstant { n } => {
                self.alu_or(n);
                8
            }
            //  or   (HL)        B6         8 z000 A=A | (HL)
            OrMemoryHL => {
                let address = self.reg.read_word(Register16bit::HL);
                self.alu_or(self.mem.read(address));
                8
            }
            //  cp   r           Bx         4 z1hc compare A-r
            CompareRegister { r } => {
                self.alu_cmp(self.reg.read(r));
                4
            }
            //  cp   n           FE nn      8 z1hc compare A-n
            CompareConstant { n } => {
                self.alu_cmp(n);
                8
            }
            //  cp   (HL)        BE         8 z1hc compare A-(HL)
            CompareMemoryHL => {
                let address = self.reg.read_word(Register16bit::HL);
                self.alu_cmp(self.mem.read(address));
                8
            }
            //  inc  r           xx         4 z0h- r=r+1
            IncrementRegister { r } => {
                let val = self.alu_inc(self.reg.read(r));
                self.reg.write(r, val);
                4
            }
            //  inc  (HL)        34        12 z0h- (HL)=(HL)+1
            IncrementMemoryHL => {
                let address = self.reg.read_word(Register16bit::HL);
                let val = self.alu_inc(self.mem.read(address));
                self.mem.write(address, val);
                12
            }
            //  dec  r           xx         4 z1h- r=r-1
            DecrementRegister { r } => {
                let val = self.alu_dec(self.reg.read(r));
                self.reg.write(r, val);
                4
            }
            //  dec  (HL)        35        12 z1h- (HL)=(HL)-1
            DecrementMemoryHL => {
                let address = self.reg.read_word(Register16bit::HL);
                let val = self.alu_dec(self.mem.read(address));
                self.mem.write(address, val);
                12
            }
            //  daa              27         4 z-0c decimal adjust akku
            DecimalAdjust => {
                // The BCD Flags (N, H)
                // These flags are (rarely) used for the DAA instruction only, N Indicates whether the previous instruction has been an addition or subtraction, and H indicates carry for lower 4bits of the result, also for DAA, the C flag must indicate carry for upper 8bits.
                // After adding/subtracting two BCD numbers, DAA is intended to convert the result into BCD format; BCD numbers are ranged from 00h to 99h rather than 00h to FFh.
                // Because C and H flags must contain carry-outs for each digit, DAA cannot be used for 16bit operations (which have 4 digits), or for INC/DEC operations (which do not affect C-flag).
                self.alu_daa();
                4
            }
            //  cpl              2F         4 -11- A = A xor FF
            CPL => {
                // Complement all bits
                self.reg
                    .write(Register::A, self.reg.read(Register::A) ^ 0xFF);
                self.reg.set_flag(Subtract, true);
                self.reg.set_flag(HalfCarry, true);
                4
            }

            //GMB 16bit-Arithmetic/logical Commands
            //add  HL,rr     x9           8 -0hc HL = HL+rr     ;rr may be BC,DE,HL,SP
            AddRegisterHL16bit { r } => {
                let val =
                    self.alu_add16(self.reg.read_word(Register16bit::HL), self.reg.read_word(r));
                self.reg.write_word(Register16bit::HL, val);
                8
            }
            AddRegisterSPtoHL16bit {} => {
                let val = self.alu_add16(self.reg.read_word(Register16bit::HL), self.sp);
                self.reg.write_word(Register16bit::HL, val);
                8
            }
            //inc  rr        x3           8 ---- rr = rr+1      ;rr may be BC,DE,HL,SP
            IncrementRegister16bit { r } => {
                self.reg
                    .write_word(r, self.reg.read_word(r).wrapping_add(1));
                8
            }
            IncrementSP {} => {
                self.sp = self.sp.wrapping_add(1);
                8
            }
            //dec  rr        xB           8 ---- rr = rr-1      ;rr may be BC,DE,HL,SP
            DecrementRegister16bit { r } => {
                self.reg
                    .write_word(r, self.reg.read_word(r).wrapping_sub(1));
                8
            }
            DecrementSP {} => {
                self.sp = self.sp.wrapping_sub(1);
                8
            }
            //add  SP,dd     E8          16 00hc SP = SP +/- dd ;dd is 8bit signed number
            AddSP { d } => {
                let a = self.sp;
                let b = d as i16 as u16;
                self.sp = a.wrapping_add(b);

                self.reg.set_flags(Flags {
                    z: false,
                    n: false,
                    h: (a & 0xF) + (b & 0xF) > 0xF,
                    c: (a & 0xFF) + (b & 0xFF) > 0xFF,
                });

                16
            }
            //ld   HL,SP+dd  F8          12 00hc HL = SP +/- dd ;dd is 8bit signed number
            LoadHLwithSPplus { d } => {
                let a = self.sp;
                let b = d as i16 as u16;
                self.reg.write_word(Register16bit::HL, a.wrapping_add(b));

                self.reg.set_flags(Flags {
                    z: false,
                    n: false,
                    h: (a & 0xF) + (b & 0xF) > 0xF,
                    c: (a & 0xFF) + (b & 0xFF) > 0xFF,
                });

                12
            }

            //GMB Rotate- und Shift-Commands
            //rlca           07           4 000c rotate akku left through carry
            //rla            17           4 000c rotate akku left
            RotateLeft { use_carry } => {
                let val = self.alu_rotate_left(self.reg.read(Register::A), use_carry);
                self.reg.write(Register::A, val);
                4
            }
            //rrca           0F           4 000c rotate akku right through carry
            //rra            1F           4 000c rotate akku right
            RotateRight { use_carry } => {
                let val = self.alu_rotate_right(self.reg.read(Register::A), use_carry);
                self.reg.write(Register::A, val);
                4
            }
            //rlc  r         CB 0x        8 z00c rotate left through carry
            //rl   r         CB 1x        8 z00c rotate left
            RotateLeftRegister { r, use_carry } => {
                let val = self.alu_rotate_left(self.reg.read(r), use_carry);
                self.reg.write(r, val);
                self.reg.set_flag(Zero, self.reg.read(r) == 0);
                4
            }
            //rlc  (HL)      CB 06       16 z00c rotate left through carry
            //rrc  r         CB 0x        8 z00c rotate right
            RotateRightRegister { r, use_carry } => {
                let val = self.alu_rotate_right(self.reg.read(r), use_carry);
                self.reg.write(r, val);
                self.reg.set_flag(Zero, self.reg.read(r) == 0);
                8
            }
            //rrc  (HL)      CB 0E       16 z00c rotate right through carry
            //rl   (HL)      CB 16       16 z00c rotate left
            RotateLeftHL { use_carry } => {
                let address = self.reg.read_word(Register16bit::HL);
                let val = self.alu_rotate_left(self.mem.read(address), use_carry);
                self.mem.write(address, val);
                self.reg.set_flag(Zero, self.mem.read(address) == 0);
                16
            }
            //rr   r         CB 1x        8 z00c rotate right
            //rr   (HL)      CB 1E       16 z00c rotate right
            RotateRightHL { use_carry } => {
                let address = self.reg.read_word(Register16bit::HL);
                let val = self.alu_rotate_right(self.mem.read(address), use_carry);
                self.mem.write(address, val);
                self.reg.set_flag(Zero, self.mem.read(address) == 0);
                16
            }
            //sla  r         CB 2x        8 z00c shift left arithmetic (b0=0)
            ShiftLeftRegister { r } => {
                let val = self.alu_shift_left(self.reg.read(r));
                self.reg.write(r, val);
                8
            }
            //sla  (HL)      CB 26       16 z00c shift left arithmetic (b0=0)
            ShiftLeftHL => {
                let address = self.reg.read_word(Register16bit::HL);
                let val = self.alu_shift_left(self.mem.read(address));
                self.mem.write(address, val);
                16
            }
            //sra  r         CB 2x        8 z00c shift right arithmetic (b7=b7)
            //srl  r         CB 3x        8 z00c shift right logical (b7=0)
            ShiftRightRegister { r, is_arithmetic } => {
                let val = self.alu_shift_right(self.reg.read(r), is_arithmetic);
                self.reg.write(r, val);
                8
            }
            //sra  (HL)      CB 2E       16 z00c shift right arithmetic (b7=b7)
            //srl  (HL)      CB 3E       16 z00c shift right logical (b7=0)
            ShiftRightHL { is_arithmetic } => {
                let address = self.reg.read_word(Register16bit::HL);
                let val = self.alu_shift_right(self.mem.read(address), is_arithmetic);
                self.mem.write(address, val);
                16
            }
            //swap r         CB 3x        8 z000 exchange low/hi-nibble
            SwapRegister { r } => {
                let val = self.alu_swap_nibbles(self.reg.read(r));
                self.reg.write(r, val);
                8
            }
            //swap (HL)      CB 36       16 z000 exchange low/hi-nibble
            SwapHL => {
                let address = self.reg.read_word(Register16bit::HL);
                let val = self.alu_swap_nibbles(self.mem.read(address));
                self.mem.write(address, val);
                16
            }
            //GMB Singlebit Operation Commands
            //bit  n,r       CB xx        8 z01- test bit n
            TestBitRegister { bit, r } => {
                self.alu_test_bit(self.reg.read(r), bit);
                8
            }
            //bit  n,(HL)    CB xx       12 z01- test bit n
            TestBitMemoryHL { bit } => {
                let address = self.reg.read_word(Register16bit::HL);
                self.alu_test_bit(self.mem.read(address), bit);
                12
            }
            //set  n,r       CB xx        8 ---- set bit n
            SetBitRegister { bit, r } => {
                self.reg.write(r, Cpu::alu_set_bit(self.reg.read(r), bit));
                assert!(util::ith_bit(self.reg.read(r), bit));
                8
            }
            //set  n,(HL)    CB xx       16 ---- set bit n
            SetBitMemoryHL { bit } => {
                let address = self.reg.read_word(Register16bit::HL);
                self.mem
                    .write(address, Cpu::alu_set_bit(self.mem.read(address), bit));
                assert!(util::ith_bit(self.mem.read(address), bit));
                16
            }
            //res  n,r       CB xx        8 ---- reset bit n
            ResetBitRegister { bit, r } => {
                self.reg.write(r, Cpu::alu_reset_bit(self.reg.read(r), bit));
                assert!(!util::ith_bit(self.reg.read(r), bit));
                8
            }
            //res  n,(HL)    CB xx       16 ---- reset bit n
            ResetBitMemoryHL { bit } => {
                let address = self.reg.read_word(Register16bit::HL);
                self.mem
                    .write(address, Cpu::alu_reset_bit(self.mem.read(address), bit));
                assert!(!util::ith_bit(self.mem.read(address), bit));
                16
            }
            //GMB CPU-Control Commands
            //ccf            3F           4 -00c cy=cy xor 1
            CCF => {
                self.reg.set_flag(Subtract, false);
                self.reg.set_flag(HalfCarry, false);
                self.reg.set_flag(Carry, !self.reg.get_flag(Carry));
                4
            }
            //scf            37           4 -001 cy=1
            SCF => {
                self.reg.set_flag(Subtract, false);
                self.reg.set_flag(HalfCarry, false);
                self.reg.set_flag(Carry, true);
                4
            }
            //nop            00           4 ---- no operation
            NOP => 4,
            //halt           76         N*4 ---- halt until interrupt occurs (low power)
            HALT => {
                self.is_halted = true;
                4
            }
            //stop           10 00        ? ---- low power standby mode (VERY low power)
            STOP => {
                self.is_stopped = true;
                4
            }
            //di             F3           4 ---- disable interrupts, IME=0
            DI => {
                self.are_interrupts_enabled = false;
                4
            }
            //ei             FB           4 ---- enable interrupts, IME=1
            EI => {
                self.are_interrupts_enabled = true;
                4
            }
            //GMB Jump Commands
            //jp   nn        C3 nn nn    16 ---- jump to nn, PC=nn
            JP { nn } => {
                self.pc = nn;
                16
            }
            //jp   HL        E9           4 ---- jump to HL, PC=HL
            JPtoMemoryHL => {
                let address = self.reg.read_word(Register16bit::HL);
                self.pc = address;
                4
            }
            //jp   f,nn      xx nn nn 16;12 ---- conditional jump if nz,z,nc,c
            CondJP {
                flag,
                check_state,
                nn,
            } => {
                assert!(flag == Zero || flag == Carry);
                if self.reg.get_flag(flag) == check_state {
                    self.pc = nn;
                    16
                } else {
                    12
                }
            }
            //jr   PC+dd     18 dd       12 ---- relative jump to nn (PC=PC+/-7bit)
            JRtoMemory { dd } => {
                self.pc = (i32::from(self.pc) + i32::from(dd)) as u16;
                12
            }
            //jr   f,PC+dd   xx dd     12;8 ---- conditional relative jump if nz,z,nc,c
            CondJR {
                flag,
                check_state,
                dd,
            } => {
                assert!(flag == Zero || flag == Carry);
                if self.reg.get_flag(flag) == check_state {
                    self.pc = (i32::from(self.pc) + i32::from(dd)) as u16;
                    12
                } else {
                    8
                }
            }
            //call nn        CD nn nn    24 ---- call to nn, SP=SP-2, (SP)=PC, PC=nn
            CALL { nn } => {
                self.push_stack(self.pc);
                self.pc = nn;
                24
            }
            //call f,nn      xx nn nn 24;12 ---- conditional call if nz,z,nc,c
            CondCALL {
                flag,
                check_state,
                nn,
            } => {
                assert!(flag == Zero || flag == Carry);
                if self.reg.get_flag(flag) == check_state {
                    self.push_stack(self.pc);
                    self.pc = nn;
                    24
                } else {
                    12
                }
            }
            //ret            C9          16 ---- return, PC=(SP), SP=SP+2
            RET => {
                self.pc = self.pop_stack();
                16
            }
            //ret  f         xx        20;8 ---- conditional return if nz,z,nc,c
            CondRET { flag, check_state } => {
                assert!(flag == Zero || flag == Carry);
                if self.reg.get_flag(flag) == check_state {
                    self.pc = self.pop_stack();
                    20
                } else {
                    8
                }
            }
            //reti           D9          16 ---- return and enable interrupts (IME=1)
            RETI => {
                self.pc = self.pop_stack();

                self.are_interrupts_enabled = true;
                16
            }
            //rst  n         xx          16 ---- call to 00,08,10,18,20,28,30,38
            RST { n } => {
                self.push_stack(self.pc);
                self.pc = n;
                16
            }
            LoadAddressWithSP { n } => {
                self.mem.write_word(n, self.sp);
                20
            }
        }
    }

    // (instruction, pc increments)
    fn fetch(&self) -> (Instruction, u16) {
        let immediate_u8 = || self.mem.read(self.pc + 1);
        let immediate_i8 = || self.mem.read(self.pc + 1) as i8;
        let immediate_u16 = || self.mem.read_word(self.pc + 1);

        let op = self.mem.read(self.pc);

        match op {
            // GMB 8bit-Load commands
            // ld   r,r         xx         4 ---- r=r
            // ld   r,(HL)      xx         8 ---- r=(HL)
            0x40..=0x45 => (
                LoadReg {
                    to: Register::B,
                    from: Register::from(op & 0b0111),
                },
                1,
            ),
            0x46 => (LoadRegWithMemoryHL { to: Register::B }, 1),
            0x47 => (
                LoadReg {
                    to: Register::B,
                    from: Register::A,
                },
                1,
            ),
            0x48..=0x4D => (
                LoadReg {
                    to: Register::C,
                    from: Register::from((op - 8) & 0b0111),
                },
                1,
            ),
            0x4E => (LoadRegWithMemoryHL { to: Register::C }, 1),
            0x4F => (
                LoadReg {
                    to: Register::C,
                    from: Register::A,
                },
                1,
            ),
            0x50..=0x55 => (
                LoadReg {
                    to: Register::D,
                    from: Register::from(op & 0b0111),
                },
                1,
            ),
            0x56 => (LoadRegWithMemoryHL { to: Register::D }, 1),
            0x57 => (
                LoadReg {
                    to: Register::D,
                    from: Register::A,
                },
                1,
            ),
            0x58..=0x5D => (
                LoadReg {
                    to: Register::E,
                    from: Register::from((op - 8) & 0b0111),
                },
                1,
            ),
            0x5E => (LoadRegWithMemoryHL { to: Register::E }, 1),
            0x5F => (
                LoadReg {
                    to: Register::E,
                    from: Register::A,
                },
                1,
            ),
            0x60..=0x65 => (
                LoadReg {
                    to: Register::H,
                    from: Register::from(op & 0b0111),
                },
                1,
            ),
            0x66 => (LoadRegWithMemoryHL { to: Register::H }, 1),
            0x67 => (
                LoadReg {
                    to: Register::H,
                    from: Register::A,
                },
                1,
            ),
            0x68..=0x6D => (
                LoadReg {
                    to: Register::L,
                    from: Register::from((op - 8) & 0b0111),
                },
                1,
            ),
            0x6E => (LoadRegWithMemoryHL { to: Register::L }, 1),
            0x6F => (
                LoadReg {
                    to: Register::L,
                    from: Register::A,
                },
                1,
            ),
            0x78..=0x7D => (
                LoadReg {
                    to: Register::A,
                    from: Register::from((op - 8) & 0b0111),
                },
                1,
            ),
            0x7E => (LoadRegWithMemoryHL { to: Register::A }, 1),
            0x7F => (
                LoadReg {
                    to: Register::A,
                    from: Register::A,
                },
                1,
            ),

            // ld   r,n         xx nn      8 ---- r=n
            0x06 => (
                LoadRegWithConstant {
                    to: Register::B,
                    n: immediate_u8(),
                },
                2,
            ),
            0x0E => (
                LoadRegWithConstant {
                    to: Register::C,
                    n: immediate_u8(),
                },
                2,
            ),
            0x16 => (
                LoadRegWithConstant {
                    to: Register::D,
                    n: immediate_u8(),
                },
                2,
            ),
            0x1E => (
                LoadRegWithConstant {
                    to: Register::E,
                    n: immediate_u8(),
                },
                2,
            ),
            0x26 => (
                LoadRegWithConstant {
                    to: Register::H,
                    n: immediate_u8(),
                },
                2,
            ),
            0x2E => (
                LoadRegWithConstant {
                    to: Register::L,
                    n: immediate_u8(),
                },
                2,
            ),
            0x3E => (
                LoadRegWithConstant {
                    to: Register::A,
                    n: immediate_u8(),
                },
                2,
            ),

            // ld   (HL),r      7x         8 ---- (HL)=r
            0x70 => (LoadMemoryHLwithRegister { from: Register::B }, 1),
            0x71 => (LoadMemoryHLwithRegister { from: Register::C }, 1),
            0x72 => (LoadMemoryHLwithRegister { from: Register::D }, 1),
            0x73 => (LoadMemoryHLwithRegister { from: Register::E }, 1),
            0x74 => (LoadMemoryHLwithRegister { from: Register::H }, 1),
            0x75 => (LoadMemoryHLwithRegister { from: Register::L }, 1),
            0x77 => (LoadMemoryHLwithRegister { from: Register::A }, 1),

            // ld   (HL),n      36 nn     12 ----
            0x36 => (LoadMemoryHLwithConstant { n: immediate_u8() }, 2),
            // ld   A,(BC)      0A         8 ----
            0x0A => (LoadAwithValBC, 1),
            // ld   A,(DE)      1A         8 ----
            0x1A => (LoadAwithValDE, 1),
            // ld   A,(nn)      FA        16 ----
            0xFA => (
                LoadAwithMemory {
                    nn: immediate_u16(),
                },
                3,
            ),
            // ld   (BC),A      02         8 ----
            0x02 => (LoadMemoryBCwithA, 1),
            // ld   (DE),A      12         8 ----
            0x12 => (LoadMemoryDEwithA, 1),
            // ld   (nn),A      EA        16 ----
            0xEA => (
                LoadMemoryNNwithA {
                    nn: immediate_u16(),
                },
                3,
            ),
            // ld   A,(FF00+n)  F0 nn     12 ---- read from io-port n (memory FF00+n)
            0xF0 => (LoadAwithFF00plusN { n: immediate_u8() }, 2),
            // ld   (FF00+n),A  E0 nn     12 ---- write to io-port n (memory FF00+n)
            0xE0 => (LoadMemoryFF00plusNwithA { nn: immediate_u8() }, 2),
            // ld   A,(FF00+C)  F2         8 ---- read from io-port C (memory FF00+C)
            0xF2 => (LoadAwithFF00plusC, 1),
            // ld   (FF00+C),A  E2         8 ---- write to io-port C (memory FF00+C)
            0xE2 => (LoadMemoryFF00plusCwithA, 1),
            // ldi  (HL),A      22         8 ---- (HL)=A, HL=HL+1
            0x22 => (LoadMemoryHLwithAandIncr, 1),
            // ldi  A,(HL)      2A         8 ---- A=(HL), HL=HL+1
            0x2A => (LoadAwithValHLandIncr, 1),
            // ldd  (HL),A      32         8 ---- (HL)=A, HL=HL-1
            0x32 => (LoadMemoryHLwithAandDecr, 1),
            // ldd  A,(HL)      3A         8 ---- A=(HL), HL=HL-1
            0x3A => (LoadAwithValHLandDecr, 1),

            // GMB 16bit-Load Commands
            // ld   rr,nn       x1 nn nn  12 ---- rr=nn (rr may be BC,DE,HL or SP)
            0x01 => (
                LoadRegWith16BitConstant {
                    rr: Register16bit::BC,
                    nn: immediate_u16(),
                },
                3,
            ),
            0x11 => (
                LoadRegWith16BitConstant {
                    rr: Register16bit::DE,
                    nn: immediate_u16(),
                },
                3,
            ),
            0x21 => (
                LoadRegWith16BitConstant {
                    rr: Register16bit::HL,
                    nn: immediate_u16(),
                },
                3,
            ),
            0x31 => (
                LoadSPWith16BitConstant {
                    nn: immediate_u16(),
                },
                3,
            ),
            // ld   SP,HL       F9         8 ---- SP=HL
            0xF9 => (LoadSPwithHL, 1),
            // push rr          x5 (c5, d5, e5, f5)        16 ---- SP=SP-2  (SP)=rr   (rr may be BC,DE,HL,AF)
            0xc5 => (
                Push {
                    rr: Register16bit::BC,
                },
                1,
            ),
            0xd5 => (
                Push {
                    rr: Register16bit::DE,
                },
                1,
            ),
            0xe5 => (
                Push {
                    rr: Register16bit::HL,
                },
                1,
            ),
            0xf5 => (
                Push {
                    rr: Register16bit::AF,
                },
                1,
            ),
            // pop  rr          x1 (c1, d1, e1, f1)      12 (AF) rr=(SP)  SP=SP+2   (rr may be BC,DE,HL,AF)
            0xc1 => (
                Pop {
                    rr: Register16bit::BC,
                },
                1,
            ),
            0xd1 => (
                Pop {
                    rr: Register16bit::DE,
                },
                1,
            ),
            0xe1 => (
                Pop {
                    rr: Register16bit::HL,
                },
                1,
            ),
            0xf1 => (
                Pop {
                    rr: Register16bit::AF,
                },
                1,
            ),

            // GMB 8bit-Arithmetic/logical Commands
            //  add  A,r         8x         4 z0hc A=A+r
            0x80..=0x85 => (
                AddRegister {
                    r: Register::from(op & 0b0111),
                },
                1,
            ),
            0x86 => (AddMemoryHL, 1),
            0x87 => (AddRegister { r: Register::A }, 1),
            //  add  A,n         C6 nn      8 z0hc A=A+n
            0xC6 => (AddConstant { n: immediate_u8() }, 2),
            //  add  A,(HL)      86         8 z0hc A=A+(HL)
            //  adc  A,r         8x         4 z0hc A=A+r+cy
            0x88..=0x8D => (
                AddRegisterWithCarry {
                    r: Register::from((op - 8) & 0b0111),
                },
                1,
            ),
            0x8E => (AddMemoryHLWithCarry, 1),
            0x8F => (AddRegisterWithCarry { r: Register::A }, 1),
            //  adc  A,n         CE nn      8 z0hc A=A+n+cy
            0xCE => (AddConstantWithCarry { n: immediate_u8() }, 2),
            //  adc  A,(HL)      8E         8 z0hc A=A+(HL)+cy
            //  sub  r           9x         4 z1hc A=A-r
            0x90..=0x95 => (
                SubtractRegister {
                    r: Register::from(op & 0b0111),
                },
                1,
            ),
            0x96 => (SubtractMemoryHL, 1),
            0x97 => (SubtractRegister { r: Register::A }, 1),
            //  sub  n           D6 nn      8 z1hc A=A-n
            0xD6 => (SubtractConstant { n: immediate_u8() }, 2),
            //  sub  (HL)        96         8 z1hc A=A-(HL)
            //  sbc  A,r         9x         4 z1hc A=A-r-cy
            0x98..=0x9D => (
                SubtractRegisterWithCarry {
                    r: Register::from((op - 8) & 0b0111),
                },
                1,
            ),
            0x9E => (SubtractMemoryHLWithCarry, 1),
            0x9F => (SubtractRegisterWithCarry { r: Register::A }, 1),
            //  sbc  A,n         DE nn      8 z1hc A=A-n-cy
            0xDE => (SubtractConstantWithCarry { n: immediate_u8() }, 2),
            //  sbc  A,(HL)      9E         8 z1hc A=A-(HL)-cy
            //  and  r           Ax         4 z010 A=A & r
            0xA0..=0xA5 => (
                AndRegister {
                    r: Register::from(op & 0b0111),
                },
                1,
            ),
            0xA6 => (AndMemoryHL, 1),
            0xA7 => (AndRegister { r: Register::A }, 1),
            //  and  n           E6 nn      8 z010 A=A & n
            0xE6 => (AndConstant { n: immediate_u8() }, 2),
            //  and  (HL)        A6         8 z010 A=A & (HL)
            //  xor  r           Ax         4 z000
            0xA8..=0xAD => (
                XorRegister {
                    r: Register::from((op - 8) & 0b0111),
                },
                1,
            ),
            0xAE => (XorMemoryHL, 1),
            0xAF => (XorRegister { r: Register::A }, 1),
            //  xor  n           EE nn      8 z000
            0xEE => (XorConstant { n: immediate_u8() }, 2),
            //  xor  (HL)        AE         8 z000
            //  or   r           Bx         4 z000 A=A | r
            0xB0..=0xB5 => (
                OrRegister {
                    r: Register::from(op & 0b0111),
                },
                1,
            ),
            0xB6 => (OrMemoryHL, 1),
            0xB7 => (OrRegister { r: Register::A }, 1),
            //  or   n           F6 nn      8 z000 A=A | n
            0xF6 => (OrConstant { n: immediate_u8() }, 2),
            //  or   (HL)        B6         8 z000 A=A | (HL)
            //  cp   r           Bx         4 z1hc compare A-r
            0xB8..=0xBD => (
                CompareRegister {
                    r: Register::from((op - 8) & 0b0111),
                },
                1,
            ),
            0xBE => (CompareMemoryHL, 1),
            0xBF => (CompareRegister { r: Register::A }, 1),
            //  cp   n           FE nn      8 z1hc compare A-n
            0xFE => (CompareConstant { n: immediate_u8() }, 2),
            //  cp   (HL)        BE         8 z1hc compare A-(HL)
            //  inc  r           xx         4 z0h- r=r+1
            0x04 => (IncrementRegister { r: Register::B }, 1),
            0x0c => (IncrementRegister { r: Register::C }, 1),
            0x14 => (IncrementRegister { r: Register::D }, 1),
            0x1c => (IncrementRegister { r: Register::E }, 1),
            0x24 => (IncrementRegister { r: Register::H }, 1),
            0x2c => (IncrementRegister { r: Register::L }, 1),
            0x3c => (IncrementRegister { r: Register::A }, 1),
            //  inc  (HL)        34        12 z0h- (HL)=(HL)+1
            0x34 => (IncrementMemoryHL, 1),
            //  dec  r           xx         4 z1h- r=r-1
            0x05 => (DecrementRegister { r: Register::B }, 1),
            0x0d => (DecrementRegister { r: Register::C }, 1),
            0x15 => (DecrementRegister { r: Register::D }, 1),
            0x1d => (DecrementRegister { r: Register::E }, 1),
            0x25 => (DecrementRegister { r: Register::H }, 1),
            0x2d => (DecrementRegister { r: Register::L }, 1),
            0x3d => (DecrementRegister { r: Register::A }, 1),
            //  dec  (HL)        35        12 z1h- (HL)=(HL)-1
            0x35 => (DecrementMemoryHL, 1),
            //  daa              27         4 z-0x decimal adjust akku
            0x27 => (DecimalAdjust, 1),
            //  cpl              2F         4 -11- A = A xor FF
            0x2F => (CPL, 1),

            //GMB 16bit-Arithmetic/logical Commands
            //add  HL,rr     x9           8 -0hc HL = HL+rr     ;rr may be BC,DE,HL,SP
            0x09 => (
                AddRegisterHL16bit {
                    r: Register16bit::BC,
                },
                1,
            ),
            0x19 => (
                AddRegisterHL16bit {
                    r: Register16bit::DE,
                },
                1,
            ),
            0x29 => (
                AddRegisterHL16bit {
                    r: Register16bit::HL,
                },
                1,
            ),
            0x39 => (AddRegisterSPtoHL16bit, 1),
            //inc  rr        x3           8 ---- rr = rr+1      ;rr may be BC,DE,HL,SP
            0x03 => (
                IncrementRegister16bit {
                    r: Register16bit::BC,
                },
                1,
            ),
            0x13 => (
                IncrementRegister16bit {
                    r: Register16bit::DE,
                },
                1,
            ),
            0x23 => (
                IncrementRegister16bit {
                    r: Register16bit::HL,
                },
                1,
            ),
            0x33 => (IncrementSP, 1),
            //dec  rr        xB           8 ---- rr = rr-1      ;rr may be BC,DE,HL,SP
            0x0B => (
                DecrementRegister16bit {
                    r: Register16bit::BC,
                },
                1,
            ),
            0x1B => (
                DecrementRegister16bit {
                    r: Register16bit::DE,
                },
                1,
            ),
            0x2B => (
                DecrementRegister16bit {
                    r: Register16bit::HL,
                },
                1,
            ),
            0x3B => (DecrementSP, 1),
            //add  SP,dd     E8          16 00hc SP = SP +/- dd ;dd is 8bit signed number
            0xE8 => (AddSP { d: immediate_i8() }, 2),
            //ld   HL,SP+dd  F8          12 00hc HL = SP +/- dd ;dd is 8bit signed number
            0xF8 => (LoadHLwithSPplus { d: immediate_i8() }, 2),

            //GMB Rotate- und Shift-Commands
            //rlca           07           4 000c rotate akku left
            0x07 => (RotateLeft { use_carry: false }, 1),
            //rla            17           4 000c rotate akku left through carry
            0x17 => (RotateLeft { use_carry: true }, 1),
            //rrca           0F           4 000c rotate akku right
            0x0F => (RotateRight { use_carry: false }, 1),
            //rra            1F           4 000c rotate akku right through carry
            0x1F => (RotateRight { use_carry: true }, 1),
            0xCB => {
                let op = self.mem.read(self.pc + 1);
                match op {
                    //rlc  r         CB 0x        8 z00c rotate left
                    //rlc  (HL)      CB 06       16 z00c rotate left
                    0x00..=0x05 => (
                        RotateLeftRegister {
                            r: Register::from(op & 0b0111),
                            use_carry: false,
                        },
                        2,
                    ),
                    0x06 => (RotateLeftHL { use_carry: false }, 2),
                    0x07 => (
                        RotateLeftRegister {
                            r: Register::A,
                            use_carry: false,
                        },
                        2,
                    ),
                    //rrc  r         CB 0x        8 z00c rotate right
                    //rrc  (HL)      CB 0E       16 z00c rotate right
                    0x08..=0x0D => (
                        RotateRightRegister {
                            r: Register::from((op - 8) & 0b0111),
                            use_carry: false,
                        },
                        2,
                    ),
                    0x0E => (RotateRightHL { use_carry: false }, 2),
                    0x0F => (
                        RotateRightRegister {
                            r: Register::A,
                            use_carry: false,
                        },
                        2,
                    ),
                    //rl   r         CB 1x        8 z00c rotate left through carry
                    //rl   (HL)      CB 16       16 z00c rotate left through carry
                    0x10..=0x15 => (
                        RotateLeftRegister {
                            r: Register::from(op & 0b0111),
                            use_carry: true,
                        },
                        2,
                    ),
                    0x16 => (RotateLeftHL { use_carry: true }, 2),
                    0x17 => (
                        RotateLeftRegister {
                            r: Register::A,
                            use_carry: true,
                        },
                        2,
                    ),
                    //rr   r         CB 1x        8 z00c rotate right through carry
                    //rr   (HL)      CB 1E       16 z00c rotate right through carry
                    0x18..=0x1D => (
                        RotateRightRegister {
                            r: Register::from((op - 8) & 0b0111),
                            use_carry: true,
                        },
                        2,
                    ),
                    0x1E => (RotateRightHL { use_carry: true }, 2),
                    0x1F => (
                        RotateRightRegister {
                            r: Register::A,
                            use_carry: true,
                        },
                        2,
                    ),
                    //sla  r         CB 2x        8 z00c shift left arithmetic (b0=0)
                    //sla  (HL)      CB 26       16 z00c shift left arithmetic (b0=0)
                    0x20..=0x25 => (
                        ShiftLeftRegister {
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x26 => (ShiftLeftHL, 2),
                    0x27 => (ShiftLeftRegister { r: Register::A }, 2),
                    //sra  r         CB 2x        8 z00c shift right arithmetic (b7=b7)
                    //sra  (HL)      CB 2E       16 z00c shift right arithmetic (b7=b7)
                    0x28..=0x2D => (
                        ShiftRightRegister {
                            r: Register::from((op - 8) & 0b0111),
                            is_arithmetic: true,
                        },
                        2,
                    ),
                    0x2E => (
                        ShiftRightHL {
                            is_arithmetic: true,
                        },
                        2,
                    ),
                    0x2F => (
                        ShiftRightRegister {
                            r: Register::A,
                            is_arithmetic: true,
                        },
                        2,
                    ),
                    //swap r         CB 3x        8 z000 exchange low/hi-nibble
                    //swap (HL)      CB 36       16 z000 exchange low/hi-nibble
                    0x30..=0x35 => (
                        SwapRegister {
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x36 => (SwapHL, 2),
                    0x37 => (SwapRegister { r: Register::A }, 2),
                    //srl  r         CB 3x        8 z00c shift right logical (b7=0)
                    //srl  (HL)      CB 3E       16 z00c shift right logical (b7=0)
                    0x38..=0x3D => (
                        ShiftRightRegister {
                            r: Register::from((op - 8) & 0b0111),
                            is_arithmetic: false,
                        },
                        2,
                    ),
                    0x3E => (
                        ShiftRightHL {
                            is_arithmetic: false,
                        },
                        2,
                    ),
                    0x3F => (
                        ShiftRightRegister {
                            r: Register::A,
                            is_arithmetic: false,
                        },
                        2,
                    ),

                    //GMB Singlebit Operation Commands
                    //bit  n,r       CB xx        8 z01- test bit n
                    //bit  n,(HL)    CB xx       12 z01- test bit n
                    0x40..=0x45 => (
                        TestBitRegister {
                            bit: 0,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x46 => (TestBitMemoryHL { bit: 0 }, 2),
                    0x47 => (
                        TestBitRegister {
                            bit: 0,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x48..=0x4D => (
                        TestBitRegister {
                            bit: 1,
                            r: Register::from((op - 8) & 0b0111),
                        },
                        2,
                    ),
                    0x4E => (TestBitMemoryHL { bit: 1 }, 2),
                    0x4F => (
                        TestBitRegister {
                            bit: 1,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x50..=0x55 => (
                        TestBitRegister {
                            bit: 2,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x56 => (TestBitMemoryHL { bit: 2 }, 2),
                    0x57 => (
                        TestBitRegister {
                            bit: 2,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x58..=0x5D => (
                        TestBitRegister {
                            bit: 3,
                            r: Register::from((op - 8) & 0b0111),
                        },
                        2,
                    ),
                    0x5E => (TestBitMemoryHL { bit: 3 }, 2),
                    0x5F => (
                        TestBitRegister {
                            bit: 3,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x60..=0x65 => (
                        TestBitRegister {
                            bit: 4,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x66 => (TestBitMemoryHL { bit: 4 }, 2),
                    0x67 => (
                        TestBitRegister {
                            bit: 4,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x68..=0x6D => (
                        TestBitRegister {
                            bit: 5,
                            r: Register::from((op - 8) & 0b0111),
                        },
                        2,
                    ),
                    0x6E => (TestBitMemoryHL { bit: 5 }, 2),
                    0x6F => (
                        TestBitRegister {
                            bit: 5,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x70..=0x75 => (
                        TestBitRegister {
                            bit: 6,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x76 => (TestBitMemoryHL { bit: 6 }, 2),
                    0x77 => (
                        TestBitRegister {
                            bit: 6,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x78..=0x7D => (
                        TestBitRegister {
                            bit: 7,
                            r: Register::from((op - 8) & 0b0111),
                        },
                        2,
                    ),
                    0x7E => (TestBitMemoryHL { bit: 7 }, 2),
                    0x7F => (
                        TestBitRegister {
                            bit: 7,
                            r: Register::A,
                        },
                        2,
                    ),

                    //res  n,r       CB xx        8 ---- reset bit n
                    //res  n,(HL)    CB xx       16 ---- reset bit n
                    0x80..=0x85 => (
                        ResetBitRegister {
                            bit: 0,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x86 => (ResetBitMemoryHL { bit: 0 }, 2),
                    0x87 => (
                        ResetBitRegister {
                            bit: 0,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x88..=0x8D => (
                        ResetBitRegister {
                            bit: 1,
                            r: Register::from((op - 8) & 0b0111),
                        },
                        2,
                    ),
                    0x8E => (ResetBitMemoryHL { bit: 1 }, 2),
                    0x8F => (
                        ResetBitRegister {
                            bit: 1,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x90..=0x95 => (
                        ResetBitRegister {
                            bit: 2,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0x96 => (ResetBitMemoryHL { bit: 2 }, 2),
                    0x97 => (
                        ResetBitRegister {
                            bit: 2,
                            r: Register::A,
                        },
                        2,
                    ),
                    0x98..=0x9D => (
                        ResetBitRegister {
                            bit: 3,
                            r: Register::from((op - 8) & 0b0111),
                        },
                        2,
                    ),
                    0x9E => (ResetBitMemoryHL { bit: 3 }, 2),
                    0x9F => (
                        ResetBitRegister {
                            bit: 3,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xA0..=0xA5 => (
                        ResetBitRegister {
                            bit: 4,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xA6 => (ResetBitMemoryHL { bit: 4 }, 2),
                    0xA7 => (
                        ResetBitRegister {
                            bit: 4,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xA8..=0xAD => (
                        ResetBitRegister {
                            bit: 5,
                            r: Register::from((op - 8) & 0b0111),
                        },
                        2,
                    ),
                    0xAE => (ResetBitMemoryHL { bit: 5 }, 2),
                    0xAF => (
                        ResetBitRegister {
                            bit: 5,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xB0..=0xB5 => (
                        ResetBitRegister {
                            bit: 6,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xB6 => (ResetBitMemoryHL { bit: 6 }, 2),
                    0xB7 => (
                        ResetBitRegister {
                            bit: 6,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xB8..=0xBD => (
                        ResetBitRegister {
                            bit: 7,
                            r: Register::from((op - 8) & 0b0111),
                        },
                        2,
                    ),
                    0xBE => (ResetBitMemoryHL { bit: 7 }, 2),
                    0xBF => (
                        ResetBitRegister {
                            bit: 7,
                            r: Register::A,
                        },
                        2,
                    ),

                    //set  n,r       CB xx        8 ---- set bit n
                    //set  n,(HL)    CB xx       16 ---- set bit n
                    0xC0..=0xC5 => (
                        SetBitRegister {
                            bit: 0,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xC6 => (SetBitMemoryHL { bit: 0 }, 2),
                    0xC7 => (
                        SetBitRegister {
                            bit: 0,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xC8..=0xCD => (
                        SetBitRegister {
                            bit: 1,
                            r: Register::from((op - 8) & 0b0111),
                        },
                        2,
                    ),
                    0xCE => (SetBitMemoryHL { bit: 1 }, 2),
                    0xCF => (
                        SetBitRegister {
                            bit: 1,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xD0..=0xD5 => (
                        SetBitRegister {
                            bit: 2,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xD6 => (SetBitMemoryHL { bit: 2 }, 2),
                    0xD7 => (
                        SetBitRegister {
                            bit: 2,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xD8..=0xDD => (
                        SetBitRegister {
                            bit: 3,
                            r: Register::from((op - 8) & 0b0111),
                        },
                        2,
                    ),
                    0xDE => (SetBitMemoryHL { bit: 3 }, 2),
                    0xDF => (
                        SetBitRegister {
                            bit: 3,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xE0..=0xE5 => (
                        SetBitRegister {
                            bit: 4,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xE6 => (SetBitMemoryHL { bit: 4 }, 2),
                    0xE7 => (
                        SetBitRegister {
                            bit: 4,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xE8..=0xED => (
                        SetBitRegister {
                            bit: 5,
                            r: Register::from((op - 8) & 0b0111),
                        },
                        2,
                    ),
                    0xEE => (SetBitMemoryHL { bit: 5 }, 2),
                    0xEF => (
                        SetBitRegister {
                            bit: 5,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xF0..=0xF5 => (
                        SetBitRegister {
                            bit: 6,
                            r: Register::from(op & 0b0111),
                        },
                        2,
                    ),
                    0xF6 => (SetBitMemoryHL { bit: 6 }, 2),
                    0xF7 => (
                        SetBitRegister {
                            bit: 6,
                            r: Register::A,
                        },
                        2,
                    ),
                    0xF8..=0xFD => (
                        SetBitRegister {
                            bit: 7,
                            r: Register::from((op - 8) & 0b0111),
                        },
                        2,
                    ),
                    0xFE => (SetBitMemoryHL { bit: 7 }, 2),
                    0xFF => (
                        SetBitRegister {
                            bit: 7,
                            r: Register::A,
                        },
                        2,
                    ),
                }
            }
            //GMB CPU-Control Commands
            //ccf            3F           4 -00c cy=cy xor 1
            0x3F => (CCF, 1),
            //scf            37           4 -001 cy=1
            0x37 => (SCF, 1),
            //nop            00           4 ---- no operation
            0x00 => (NOP, 1),
            //halt           76         N*4 ---- halt until interrupt occurs (low power)
            0x76 => (HALT, 1),
            //stop           10 00        ? ---- low power standby mode (VERY low power)
            0x10 => (STOP, 2),
            //di             F3           4 ---- disable interrupts, IME=0
            0xF3 => (DI, 1),
            //ei             FB           4 ---- enable interrupts, IME=1
            0xFB => (EI, 1),

            //GMB Jump Commands
            //jp   nn        C3 nn nn    16 ---- jump to nn, PC=nn
            0xC3 => (
                JP {
                    nn: immediate_u16(),
                },
                3,
            ),
            //jp   HL        E9           4 ---- jump to HL, PC=HL
            0xE9 => (JPtoMemoryHL, 1),
            //jp   f,nn      xx nn nn 16;12 ---- conditional jump if nz,z,nc,c
            0xC2 => (
                CondJP {
                    flag: Zero,
                    check_state: false,
                    nn: immediate_u16(),
                },
                3,
            ),
            0xCA => (
                CondJP {
                    flag: Zero,
                    check_state: true,
                    nn: immediate_u16(),
                },
                3,
            ),
            0xD2 => (
                CondJP {
                    flag: Carry,
                    check_state: false,
                    nn: immediate_u16(),
                },
                3,
            ),
            0xDA => (
                CondJP {
                    flag: Carry,
                    check_state: true,
                    nn: immediate_u16(),
                },
                3,
            ),
            //jr   PC+dd     18 dd       12 ---- relative jump to nn (PC=PC+/-7bit)
            0x18 => (JRtoMemory { dd: immediate_i8() }, 2),
            //jr   f,PC+dd   xx dd     12;8 ---- conditional relative jump if nz,z,nc,c
            0x20 => (
                CondJR {
                    flag: Zero,
                    check_state: false,
                    dd: immediate_i8(),
                },
                2,
            ),
            0x28 => (
                CondJR {
                    flag: Zero,
                    check_state: true,
                    dd: immediate_i8(),
                },
                2,
            ),
            0x30 => (
                CondJR {
                    flag: Carry,
                    check_state: false,
                    dd: immediate_i8(),
                },
                2,
            ),
            0x38 => (
                CondJR {
                    flag: Carry,
                    check_state: true,
                    dd: immediate_i8(),
                },
                2,
            ),
            //call nn        CD nn nn    24 ---- call to nn, SP=SP-2, (SP)=PC, PC=nn
            0xCD => (
                CALL {
                    nn: immediate_u16(),
                },
                3,
            ),
            //call f,nn      xx nn nn 24;12 ---- conditional call if nz,z,nc,c
            0xC4 => (
                CondCALL {
                    flag: Zero,
                    check_state: false,
                    nn: immediate_u16(),
                },
                3,
            ),
            0xCC => (
                CondCALL {
                    flag: Zero,
                    check_state: true,
                    nn: immediate_u16(),
                },
                3,
            ),
            0xD4 => (
                CondCALL {
                    flag: Carry,
                    check_state: false,
                    nn: immediate_u16(),
                },
                3,
            ),
            0xDC => (
                CondCALL {
                    flag: Carry,
                    check_state: true,
                    nn: immediate_u16(),
                },
                3,
            ),
            //ret            C9          16 ---- return, PC=(SP), SP=SP+2
            0xC9 => (RET, 1),
            //ret  f         xx        20;8 ---- conditional return if nz,z,nc,c
            0xC0 => (
                CondRET {
                    flag: Zero,
                    check_state: false,
                },
                1,
            ),
            0xC8 => (
                CondRET {
                    flag: Zero,
                    check_state: true,
                },
                1,
            ),
            0xD0 => (
                CondRET {
                    flag: Carry,
                    check_state: false,
                },
                1,
            ),
            0xD8 => (
                CondRET {
                    flag: Carry,
                    check_state: true,
                },
                1,
            ),
            //reti           D9          16 ---- return and enable interrupts (IME=1)
            0xD9 => (RETI, 1),
            //rst  n         xx          16 ---- call to 00,08,10,18,20,28,30,38
            0xC7 => (RST { n: 0x00 }, 1),
            0xCF => (RST { n: 0x08 }, 1),
            0xD7 => (RST { n: 0x10 }, 1),
            0xDF => (RST { n: 0x18 }, 1),
            0xE7 => (RST { n: 0x20 }, 1),
            0xEF => (RST { n: 0x28 }, 1),
            0xF7 => (RST { n: 0x30 }, 1),
            0xFF => (RST { n: 0x38 }, 1),
            0x08 => (LoadAddressWithSP { n: immediate_u16() }, 3),
            _ => panic!("pc: {:x?}, op: {:x?}", self.pc, op),
        }
    }
}

impl fmt::Debug for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (instr, incr) = self.fetch();
        let v: Vec<u8> = (self.pc..self.pc+incr).map(|addr| self.mem.read(addr)).collect();
        let tabs = match v.len() {
            0 => "",
            1 => "\t\t\t",
            2 => "\t\t",
            3 => "\t",
            _ => unreachable!(),
        };

        let stack_data: u16 = if self.sp >= 0xffff {
            0
        } else {
            self.mem.read_word(self.sp)
        };

        write!(f, "PC: {:4x?}, {:2x?}, SP: {:4x?} ({:4x?}), bytes: {:2x?},{} {:2x?}  \t LY: {:2x} \t {:x?}",
               self.pc, self.mem.read(INTERRUPT_REQUEST_REGISTER), self.sp, stack_data, v, tabs, self.reg, self.mem.read(0xFF44), instr)
    }
}

#[cfg(test)]
mod test {
    use crate::cpu::Cpu;
    use crate::registers::ConditionalFlag::*;
    use crate::registers::{Flags, Register};

    fn make_test_cpu() -> Cpu {
        let cartridge = [0u8; 0x8000];
        Cpu::new(&cartridge, None)
    }

    #[test]
    fn alu_add() {
        let mut cpu = make_test_cpu();

        cpu.reg.write(Register::A, 0x3A);
        cpu.alu_add(0xC6, false);
        assert_eq!(0x3Au8.wrapping_add(0xC6), cpu.reg.read(Register::A));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: true,
                n: false,
                h: true,
                c: true,
            }
        );

        cpu.reg.write(Register::A, 0x3A);
        cpu.alu_add(0xC6, true);
        assert_eq!(1, cpu.reg.read(Register::A));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: true,
                c: true,
            }
        );

        cpu.reg.write(Register::A, 0);
        cpu.alu_add(1, false);
        assert_eq!(1, cpu.reg.read(Register::A));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: false,
                c: false,
            }
        );

        cpu.reg.write(Register::A, 0x0f);
        cpu.alu_add(1, false);
        assert_eq!(0x10, cpu.reg.read(Register::A));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: true,
                c: false,
            }
        );
    }

    #[test]
    fn alu_sub() {
        let mut cpu = make_test_cpu();

        cpu.reg.write(Register::A, 0x3A);
        cpu.alu_sub(0x3a, false);
        assert_eq!(0, cpu.reg.read(Register::A));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: true,
                n: true,
                h: false,
                c: false,
            }
        );

        cpu.reg.write(Register::A, 0x00);
        cpu.alu_sub(0x3a, false);
        assert_eq!(0x00u8.wrapping_sub(0x3a), cpu.reg.read(Register::A));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: true,
                h: true,
                c: true,
            }
        );

        cpu.reg.write(Register::A, 0x3a);
        cpu.alu_sub(0x30, true);
        assert_eq!(
            0x3au8.wrapping_sub(0x30).wrapping_sub(1),
            cpu.reg.read(Register::A)
        );
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: true,
                h: false,
                c: false,
            }
        );

        cpu.reg.write(Register::A, 0x20);
        cpu.alu_sub(0x11, false);
        assert_eq!(0x20u8.wrapping_sub(0x11), cpu.reg.read(Register::A));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: true,
                h: true,
                c: false,
            }
        );
    }

    #[test]
    fn alu_logical() {
        let mut cpu = make_test_cpu();

        // AND
        cpu.reg.write(Register::A, 0x3A);
        cpu.alu_and(0x0F);
        assert_eq!(0x3A & 0x0F, cpu.reg.read(Register::A));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: true,
                c: false,
            }
        );

        cpu.reg.write(Register::A, 0x3A);
        cpu.alu_and(0x00);
        assert_eq!(0x00, cpu.reg.read(Register::A));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: true,
                n: false,
                h: true,
                c: false,
            }
        );

        // OR
        cpu.reg.write(Register::A, 0x3A);
        cpu.alu_or(0x0F);
        assert_eq!(0x3A | 0x0F, cpu.reg.read(Register::A));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: false,
                c: false,
            }
        );

        cpu.reg.write(Register::A, 0x00);
        cpu.alu_or(0x00);
        assert_eq!(0x00, cpu.reg.read(Register::A));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: true,
                n: false,
                h: false,
                c: false,
            }
        );

        // XOR
        cpu.reg.write(Register::A, 0x3A);
        cpu.alu_xor(0x1B);
        assert_eq!(0x3A ^ 0x1B, cpu.reg.read(Register::A));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: false,
                c: false,
            }
        );

        cpu.reg.write(Register::A, 0x4C);
        cpu.alu_xor(0x4C);
        assert_eq!(0x00, cpu.reg.read(Register::A));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: true,
                n: false,
                h: false,
                c: false,
            }
        );

        // INC
        assert_eq!(0x4D, cpu.alu_inc(0x4C));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: false,
                c: false,
            }
        );

        assert_eq!(0x00, cpu.alu_inc(0xFF));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: true,
                n: false,
                h: true,
                c: cpu.reg.get_flag(Carry),
            }
        );

        // DEC
        assert_eq!(0x00, cpu.alu_dec(0x01));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: true,
                n: true,
                h: false,
                c: false,
            }
        );

        assert_eq!(0xFF, cpu.alu_dec(0x00));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: true,
                h: true,
                c: cpu.reg.get_flag(Carry),
            }
        );

        // Swap nibbles
        assert_eq!(0xF1, cpu.alu_swap_nibbles(0x1F));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: false,
                c: false,
            }
        );

        // Test bit
        cpu.alu_test_bit(0x02, 1);
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: true,
                c: cpu.reg.get_flag(Carry),
            }
        );

        cpu.alu_test_bit(0x02, 2);
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: true,
                n: false,
                h: true,
                c: cpu.reg.get_flag(Carry),
            }
        );

        // Set bit
        assert_eq!(0x02, Cpu::alu_set_bit(0x00, 1));

        assert_eq!(0x00, Cpu::alu_reset_bit(0x02, 1));

        // Compare
        cpu.reg.write(Register::A, 0x10);
        cpu.alu_cmp(0x10);
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: true,
                n: true,
                h: false,
                c: false,
            }
        );

        cpu.alu_cmp(0x05);
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: true,
                h: true,
                c: false,
            }
        );
    }

    #[test]
    fn alu_rotate() {
        let mut cpu = make_test_cpu();

        // Left
        assert_eq!(0b01010101, cpu.alu_rotate_left(0b10101010, false));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: false,
                c: true,
            }
        );

        assert_eq!(0b01010101, cpu.alu_rotate_left(0b10101010, true));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: false,
                c: true,
            }
        );

        assert_eq!(0x00, cpu.alu_rotate_left(0x00, false));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: true,
                n: false,
                h: false,
                c: false,
            }
        );

        // Right
        assert_eq!(0b01010101, cpu.alu_rotate_right(0b10101010, false));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: false,
                c: false,
            }
        );

        cpu.reg.set_flag(Carry, true);
        assert_eq!(0b11010101, cpu.alu_rotate_right(0b10101010, true));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: false,
                c: false,
            }
        );

        assert_eq!(0x00, cpu.alu_rotate_right(0x00, false));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: true,
                n: false,
                h: false,
                c: false,
            }
        );

        assert_eq!(0x00, cpu.alu_rotate_right(0x00, true));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: true,
                n: false,
                h: false,
                c: false,
            }
        );
    }

    #[test]
    fn alu_shift() {
        let mut cpu = make_test_cpu();

        // Left
        assert_eq!(0b01010100, cpu.alu_shift_left(0b10101010));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: false,
                c: true,
            }
        );

        assert_eq!(0x00, cpu.alu_shift_left(0x80));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: true,
                n: false,
                h: false,
                c: true,
            }
        );

        // Right, logical
        assert_eq!(0b01010101, cpu.alu_shift_right(0b10101010, false));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: false,
                c: false,
            }
        );

        assert_eq!(0x00, cpu.alu_shift_right(0x01, false));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: true,
                n: false,
                h: false,
                c: true,
            }
        );

        // Right, arithmetic
        assert_eq!(0b11010101, cpu.alu_shift_right(0b10101010, true));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: false,
                c: false,
            }
        );

        assert_eq!(0x00, cpu.alu_shift_right(0x01, true));
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: true,
                n: false,
                h: false,
                c: false,
            }
        );
    }

    #[test]
    fn alu_daa() {
        let mut cpu = make_test_cpu();

        // Add
        cpu.reg.write(Register::A, 0x45);
        cpu.alu_add(0x38, false);

        assert_eq!(cpu.reg.read(Register::A), 0x7D);
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: false,
                c: false,
            }
        );

        cpu.alu_daa();
        assert_eq!(cpu.reg.read(Register::A), 0x83);
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: false,
                h: false,
                c: false,
            }
        );

        // Sub
        cpu.alu_sub(0x38, false);
        assert_eq!(cpu.reg.read(Register::A), 0x4B);
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: true,
                h: true,
                c: false,
            }
        );

        cpu.alu_daa();
        assert_eq!(cpu.reg.read(Register::A), 0x45);
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: false,
                n: true,
                h: false,
                c: false,
            }
        );
    }

    #[test]
    fn stack() {
        let mut cpu = make_test_cpu();

        cpu.sp = 0xFFFE;
        cpu.push_stack(10);
        assert_eq!(cpu.sp, 0xFFFE - 2);
        assert_eq!(cpu.mem.read_word(cpu.sp), 10);

        cpu.push_stack(20);
        assert_eq!(cpu.sp, 0xFFFE - 4);
        assert_eq!(cpu.mem.read_word(cpu.sp), 20);

        assert_eq!(cpu.pop_stack(), 20);
        assert_eq!(cpu.sp, 0xFFFE - 2);
        assert_eq!(cpu.pop_stack(), 10);
        assert_eq!(cpu.sp, 0xFFFE);
    }

    #[test]
    fn alu_16() {
        let mut cpu = make_test_cpu();

        assert_eq!(cpu.alu_add16(10, 20), 30);
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: cpu.reg.get_flag(Zero),
                n: false,
                h: false,
                c: false,
            }
        );

        assert_eq!(cpu.alu_add16(0xFFFF, 1), 0);
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: cpu.reg.get_flag(Zero),
                n: false,
                h: true,
                c: true,
            }
        );

        assert_eq!(cpu.alu_add16(0x07FF, 1), 0x800);
        assert_eq!(
            cpu.reg.get_flags(),
            Flags {
                z: cpu.reg.get_flag(Zero),
                n: false,
                h: true,
                c: false,
            }
        );
    }
}
