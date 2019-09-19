use std::collection::BTreeMap;

use crate::cpu::Instruction::*;

// @formatter:off
type RegisterIndex = usize;
type MemoryAddress = usize;

enum ConditionalFlag {
    NotCarry{},
    Carry{},
    NotZero{},
    Zero{},
}

enum Instruction {
    // n: unsigned 8-bit immediate data
    // nn: unsigned 16-bit immediate data
    // e: signed 8-bit immediate data
    // r: signed 8-bit immediate data, relative to PC

    // GMB 8bit-Load commands
    // ld   r,r         xx         4 ---- r=r
    LoadReg{to: RegisterIndex, from: RegisterIndex},
    // ld   r,n         xx nn      8 ---- r=n
    LoadRegWithConstant{to: RegisterIndex, n: u8},
    // ld   r,(HL)      xx         8 ---- r=(HL)
    LoadRegWithMemoryHL{to: RegisterIndex},
    // ld   (HL),r      7x         8 ---- (HL)=r
    LoadMemoryHLwithRegister{from: RegisterIndex},
    // ld   (HL),n      36 nn     12 ----
    LoadMemoryHLwithConstant {n: u8},
    // ld   A,(BC)      0A         8 ----
    LoadAwithValBC {},
    // ld   A,(DE)      1A         8 ----
    LoadAwithValDE {},
    // ld   A,(nn)      FA        16 ----
    LoadAwithMemory { nn: usize},
    // ld   (BC),A      02         8 ----
    LoadMemoryBCwithA {},
    // ld   (DE),A      12         8 ----
    LoadMemoryDEwithA {},
    // ld   (nn),A      EA        16 ----
    LoadMemoryNNwithA {nn: usize},
    // ld   A,(FF00+n)  F0 nn     12 ---- read from io-port n (memory FF00+n)
    LoadAwithFF00plusN {nn: usize},
    // ld   (FF00+n),A  E0 nn     12 ---- write to io-port n (memory FF00+n)
    LoadMemoryFF00plusNwithA {n: usize},
    // ld   A,(FF00+C)  F2         8 ---- read from io-port C (memory FF00+C)
    LoadAwithFF00plusC {},
    // ld   (FF00+C),A  E2         8 ---- write to io-port C (memory FF00+C)
    LoadMemoryFF00plusCwithA {},
    // ldi  (HL),A      22         8 ---- (HL)=A, HL=HL+1
    LoadMemoryHLwithAandIncr {},
    // ldi  A,(HL)      2A         8 ---- A=(HL), HL=HL+1
    LoadAwithValHLandIncr {},

    // ldd  (HL),A      32         8 ---- (HL)=A, HL=HL-1
    LoadMemoryHLwithAandDecr {},
    // ldd  A,(HL)      3A         8 ---- A=(HL), HL=HL-1
    LoadAwithValHLandDecr {},

    // GMB 16bit-Load Commands
    // ld   rr,nn       x1 nn nn  12 ---- rr=nn (rr may be BC,DE,HL or SP)
    LoadRegWith16BitConstant {rr: RegisterIndex, nn: u16},
    LoadSPWith16BitConstant {nn: u16},
    // ld   SP,HL       F9         8 ---- SP=HL
    LoadSPwithHL {},
    // push rr          x5 (c5, d5, e5, f5)        16 ---- SP=SP-2  (SP)=rr   (rr may be BC,DE,HL,AF)
    Push {rr: RegisterIndex},
    // pop  rr          x1 (c1, d1, e1, f1)      12 (AF) rr=(SP)  SP=SP+2   (rr may be BC,DE,HL,AF)
    Pop {rr: RegisterIndex},

    // GMB 8bit-Arithmetic/logical Commands
    //  add  A,r         8x         4 z0hc A=A+r
    AddRegister {r: RegisterIndex},
    //  add  A,n         C6 nn      8 z0hc A=A+n
    AddConstant {n: u8},
    //  add  A,(HL)      86         8 z0hc A=A+(HL)
    AddMemoryHL {},
    //  adc  A,r         8x         4 z0hc A=A+r+cy
    AddRegisterWithCarry {r: RegisterIndex},
    //  adc  A,n         CE nn      8 z0hc A=A+n+cy
    AddConstantWithCarry {n: u8},
    //  adc  A,(HL)      8E         8 z0hc A=A+(HL)+cy
    AddMemoryHLWithCarry {},
    //  sub  r           9x         4 z1hc A=A-r
    SubtractRegister {r: RegisterIndex},
    //  sub  n           D6 nn      8 z1hc A=A-n
    SubtractConstant {n: u8},
    //  sub  (HL)        96         8 z1hc A=A-(HL)
    SubtractMemoryHL {},
    //  sbc  A,r         9x         4 z1hc A=A-r-cy
    SubtractRegisterWithCarry {r: RegisterIndex},
    //  sbc  A,n         DE nn      8 z1hc A=A-n-cy
    SubtractConstantWithCarry {n: u8},
    //  sbc  A,(HL)      9E         8 z1hc A=A-(HL)-cy
    SubtractMemoryHLWithCarry {},
    //  and  r           Ax         4 z010 A=A & r
    AndRegister {r: RegisterIndex},
    //  and  n           E6 nn      8 z010 A=A & n
    AndConstant {n: u8},
    //  and  (HL)        A6         8 z010 A=A & (HL)
    AndMemoryHL {},
    //  xor  r           Ax         4 z000
    XorRegister {r: RegisterIndex},
    //  xor  n           EE nn      8 z000
    XorConstant {n: u8},
    //  xor  (HL)        AE         8 z000
    XorMemoryHL {},
    //  or   r           Bx         4 z000 A=A | r
    OrRegister {r: RegisterIndex},
    //  or   n           F6 nn      8 z000 A=A | n
    OrConstant {n: u8},
    //  or   (HL)        B6         8 z000 A=A | (HL)
    OrMemoryHL {},
    //  cp   r           Bx         4 z1hc compare A-r
    CompareRegister {r: RegisterIndex},
    //  cp   n           FE nn      8 z1hc compare A-n
    CompareConstant {n: u8},
    //  cp   (HL)        BE         8 z1hc compare A-(HL)
    CompareMemoryHL {},
    //  inc  r           xx         4 z0h- r=r+1
    IncrementRegister {r: RegisterIndex},
    //  inc  (HL)        34        12 z0h- (HL)=(HL)+1
    IncrementMemoryHL {},
    //  dec  r           xx         4 z1h- r=r-1
    DecrementRegister {r: RegisterIndex},
    //  dec  (HL)        35        12 z1h- (HL)=(HL)-1
    DecrementMemoryHL {},
    //  daa              27         4 z-0x decimal adjust akku
    DecimalAdjust{},
    //  cpl              2F         4 -11- A = A xor FF
    CPL{},

    //GMB 16bit-Arithmetic/logical Commands
    //add  HL,rr     x9           8 -0hc HL = HL+rr     ;rr may be BC,DE,HL,SP
    AddRegisterHL16bit{r: RegisterIndex},
    AddRegisterSPtoHL16bit{},
    //inc  rr        x3           8 ---- rr = rr+1      ;rr may be BC,DE,HL,SP
    IncrementRegister16bit{r: RegisterIndex},
    //dec  rr        xB           8 ---- rr = rr-1      ;rr may be BC,DE,HL,SP
    DecrementRegister16bit{r: RegisterIndex},
    //add  SP,dd     E8          16 00hc SP = SP +/- dd ;dd is 8bit signed number
    AddSP{d: i8},
    //ld   HL,SP+dd  F8          12 00hc HL = SP +/- dd ;dd is 8bit signed number
    LoadHLwithSPplus{d: i8},


    //GMB Rotate- und Shift-Commands
    //rlca           07           4 000c rotate akku left
    RotateLeft{},
    //rla            17           4 000c rotate akku left through carry
    RotateLeftThroughCarry{},
    //rrca           0F           4 000c rotate akku right
    RotateRight{},
    //rra            1F           4 000c rotate akku right through carry
    RotateRightThroughCarry{},
    //rlc  r         CB 0x        8 z00c rotate left
    RotateLeftRegisterThroughCarry{r: RegisterIndex},
    //rlc  (HL)      CB 06       16 z00c rotate left
    RotateLeftHLThroughCarry{},
    //rrc  r         CB 0x        8 z00c rotate right
    RotateRightRegisterThroughCarry{r: RegisterIndex},
    //rrc  (HL)      CB 0E       16 z00c rotate right
    RotateRightHLThroughCarry{},
    //rl   r         CB 1x        8 z00c rotate left through carry
    RotateLeftRegister{r: RegisterIndex},
    //rl   (HL)      CB 16       16 z00c rotate left through carry
    RotateLeftHL{},
    //rr   r         CB 1x        8 z00c rotate right through carry
    RotateRightRegister{r: RegisterIndex},
    //rr   (HL)      CB 1E       16 z00c rotate right through carry
    RotateRightHL{},
    //sla  r         CB 2x        8 z00c shift left arithmetic (b0=0)
    ShiftLeftRegister{r: RegisterIndex},
    //sla  (HL)      CB 26       16 z00c shift left arithmetic (b0=0)
    ShiftLeftHL{},
    //sra  r         CB 2x        8 z00c shift right arithmetic (b7=b7)
    ShiftRightArithmeticRegister{r: RegisterIndex},
    //sra  (HL)      CB 2E       16 z00c shift right arithmetic (b7=b7)
    ShiftRightArithmeticHL{},
    //swap r         CB 3x        8 z000 exchange low/hi-nibble
    SwapRegister{r: RegisterIndex},
    //swap (HL)      CB 36       16 z000 exchange low/hi-nibble
    SwapHL{},
    //srl  r         CB 3x        8 z00c shift right logical (b7=0)
    ShiftRightLogicalRegister{r: RegisterIndex},
    //srl  (HL)      CB 3E       16 z00c shift right logical (b7=0)
    ShiftRightLogicalHL{},


    //GMB Singlebit Operation Commands
    //bit  n,r       CB xx        8 z01- test bit n
    TestBitRegister{bit: usize, r: RegisterIndex},
    //bit  n,(HL)    CB xx       12 z01- test bit n
    TestBitMemoryHL{bit: usize},
    //set  n,r       CB xx        8 ---- set bit n
    SetBitRegister{bit: usize, r: RegisterIndex},
    //set  n,(HL)    CB xx       16 ---- set bit n
    SetBitMemoryHL{bit: usize},
    //res  n,r       CB xx        8 ---- reset bit n
    ResetBitRegister{bit: usize, r: RegisterIndex},
    //res  n,(HL)    CB xx       16 ---- reset bit n
    ResetBitMemoryHL{bit: usize},

    //GMB CPU-Control Commands
    //ccf            3F           4 -00c cy=cy xor 1
    CCF{},
    //scf            37           4 -001 cy=1
    SCF{},
    //nop            00           4 ---- no operation
    NOP{},
    //halt           76         N*4 ---- halt until interrupt occurs (low power)
    HALT{},
    //stop           10 00        ? ---- low power standby mode (VERY low power)
    STOP{},
    //di             F3           4 ---- disable interrupts, IME=0
    DI{},
    //ei             FB           4 ---- enable interrupts, IME=1
    EI{},

    //GMB Jump Commands
    //jp   nn        C3 nn nn    16 ---- jump to nn, PC=nn
    JP{nn: usize},
    //jp   HL        E9           4 ---- jump to HL, PC=HL
    JPtoMemoryHL{},
    //jp   f,nn      xx nn nn 16;12 ---- conditional jump if nz,z,nc,c
    CondJP{flag: ConditionalFlag, nn: uszie},
    //jr   PC+dd     18 dd       12 ---- relative jump to nn (PC=PC+/-7bit)
    JRtoMemory{dd: i8},
    //jr   f,PC+dd   xx dd     12;8 ---- conditional relative jump if nz,z,nc,c
    CondJR{flag: ConditionalFlag, dd: i8},
    //call nn        CD nn nn    24 ---- call to nn, SP=SP-2, (SP)=PC, PC=nn
    CALL{nn: usize},
    //call f,nn      xx nn nn 24;12 ---- conditional call if nz,z,nc,c
    CondCALL{flag: ConditionalFlag, nn: usize},
    //ret            C9          16 ---- return, PC=(SP), SP=SP+2
    RET{},
    //ret  f         xx        20;8 ---- conditional return if nz,z,nc,c
    CondRET{flag: ConditionalFlag},
    //reti           D9          16 ---- return and enable interrupts (IME=1)
    RETI{},
    //rst  n         xx          16 ---- call to 00,08,10,18,20,28,30,38
    RST{n: usize}
}
// @formatter:on

struct Cpu {
    reg: [u8; 8],
    pc: usize,
    mem: [u8; 0xFFFF],
}

// A: Arithmetic, F: Flags
const A: usize = 0;
const F: usize = 1;
const B: usize = 2;
const C: usize = 3;
const D: usize = 4;
const E: usize = 5;
const H: usize = 6;
const L: usize = 7;

impl Cpu {
    fn read16bit(&self, i: usize) -> u16 {
        assert!(i < 7);
        ((self.reg[i] as u16) << 8) | self.reg[i + 1] as u16
    }

    //    fn write16bit(&mut self, i: usize, val: u16) {
//        assert!(i < 7);
//        self.reg[i] = (val >> 8) as u8;
//        self.reg[i + 1] = (val & 0xFF) as u8;
//    }
//
    // (instruction, num cycles, pc increments)
    fn fetch_instruction(&self) -> (Instruction, usize, usize) {
        let mask_xo = |byte: u8| (byte & 0b11110000) as usize;
        let mask_ox = |byte: u8| (byte & 0b00001111) as usize;
        let shift_right_xo = |byte: u8| (byte >> 4) as usize;

        let immediate_u16 = || (self.mem[self.pc + 1] << 8) | self.mem[self.pc + 2] as usize;
        let immediate_u8 = || self.mem[self.pc + 1];
        let immediate_i8 = || self.mem[self.pc + 1] as i8;
        let reg_offset = |op| [B, C, D, E, H, L][mask_ox(op)];

        let op = self.mem[self.pc];

        match op {
            // GMB 8bit-Load commands
            // ld   r,r         xx         4 ---- r=r
            // ld   r,(HL)      xx         8 ---- r=(HL)
            0x40..0x46 => (LoadReg { to: B, from: reg_offset(op) }, 4, 1),
            0x46 => (LoadRegWithMemoryHL { to: B }, 8, 1),
            0x47 => (LoadReg { to: B, from: A }, 4, 1),

            0x48..0x4E => (LoadReg { to: C, from: load_offsets(op) }, 4, 1),
            0x4E => (LoadRegWithMemoryHL { to: C }, 8, 1),
            0x4F => (LoadReg { to: C, from: A }, 4, 1),

            0x50..0x56 => (LoadReg { to: D, from: load_offsets(op) }, 4, 1),
            0x56 => (LoadRegWithMemoryHL { to: D }, 8, 1),
            0x57 => (LoadReg { to: D, from: A }, 4, 1),

            0x58..0x5E => (LoadReg { to: E, from: load_offsets(op) }, 4, 1),
            0x5E => (LoadRegWithMemoryHL { to: E }, 8, 1),
            0x5F => (LoadReg { to: E, from: A }, 4, 1),

            0x60..0x66 => (LoadReg { to: H, from: load_offsets(op) }, 4, 1),
            0x66 => (LoadRegWithMemoryHL { to: H }, 8, 1),
            0x67 => (LoadReg { to: H, from: A }, 4, 1),

            0x68..0x6E => (LoadReg { to: L, from: load_offsets(op) }, 4, 1),
            0x6E => (LoadRegWithMemoryHL { to: L }, 8, 1),
            0x6F => (LoadReg { to: L, from: A }, 4, 1),

            0x78..0x7E => (LoadReg { to: A, from: load_offsets(op) }, 4, 1),
            0x7E => (LoadRegWithMemoryHL { to: A }, 8, 1),
            0x7F => (LoadReg { to: A, from: A }, 4, 1),

            // ld   r,n         xx nn      8 ---- r=n
            0x06 => (LoadRegWithConstant { to: B, n: immediate_u8 }, 8, 2),
            0x0E => (LoadRegWithConstant { to: C, n: immediate_u8 }, 8, 2),
            0x16 => (LoadRegWithConstant { to: D, n: immediate_u8 }, 8, 2),
            0x1E => (LoadRegWithConstant { to: E, n: immediate_u8 }, 8, 2),
            0x26 => (LoadRegWithConstant { to: H, n: immediate_u8 }, 8, 2),
            0x2E => (LoadRegWithConstant { to: L, n: immediate_u8 }, 8, 2),
            0x3E => (LoadRegWithConstant { to: A, n: immediate_u8 }, 8, 2),

            // ld   (HL),r      7x         8 ---- (HL)=r
            0x70 => (LoadMemoryHLwithRegister { from: B }, 8, 1),
            0x71 => (LoadMemoryHLwithRegister { from: C }, 8, 1),
            0x72 => (LoadMemoryHLwithRegister { from: D }, 8, 1),
            0x73 => (LoadMemoryHLwithRegister { from: E }, 8, 1),
            0x74 => (LoadMemoryHLwithRegister { from: H }, 8, 1),
            0x75 => (LoadMemoryHLwithRegister { from: L }, 8, 1),
            0x77 => (LoadMemoryHLwithRegister { from: A }, 8, 1),

            // ld   (HL),n      36 nn     12 ----
            0x36 => (LoadMemoryHLwithConstant { n: immediate_u8() }, 12, 2),
            // ld   A,(BC)      0A         8 ----
            0x0A => (LoadAwithValBC {}, 8, 1),
            // ld   A,(DE)      1A         8 ----
            0x1A => (LoadAwithValDE {}, 8, 1),
            // ld   A,(nn)      FA        16 ----
            0xFA => (LoadAwithMemory { nn: immediate_u16() }, 16, 3),
            // ld   (BC),A      02         8 ----
            0x02 => (LoadMemoryBCwithA {}, 8, 1),
            // ld   (DE),A      12         8 ----
            0x12 => (LoadMemoryDEwithA {}, 8, 1),
            // ld   (nn),A      EA        16 ----
            0xEA => (LoadMemoryNNwithA { nn: immediate_u16() }, 16, 3),
            // ld   A,(FF00+n)  F0 nn     12 ---- read from io-port n (memory FF00+n)
            0xF0 => (LoadAwithFF00plusN { nn: immediate_u16() }, 12, 3),
            // ld   (FF00+n),A  E0 nn     12 ---- write to io-port n (memory FF00+n)
            0xE0 => LoadMemoryFF00plusNwithA { n: immediate_u8(), 12, 2 },
            // ld   A,(FF00+C)  F2         8 ---- read from io-port C (memory FF00+C)
            0xF2 => (LoadAwithFF00plusC {}, 8, 1),
            // ld   (FF00+C),A  E2         8 ---- write to io-port C (memory FF00+C)
            0xE2 => (LoadMemoryFF00plusCwithA {}, 8, 1),
            // ldi  (HL),A      22         8 ---- (HL)=A, HL=HL+1
            0x22 => (LoadMemoryHLwithAandIncr {}, 8, 1),
            // ldi  A,(HL)      2A         8 ---- A=(HL), HL=HL+1
            0x2A => (LoadAwithValHLandIncr {}, 8, 1),
            // ldd  (HL),A      32         8 ---- (HL)=A, HL=HL-1
            0x32 => (LoadMemoryHLwithAandDecr {}, 8, 1),
            // ldd  A,(HL)      3A         8 ---- A=(HL), HL=HL-1
            0x3A => (LoadAwithValHLandDecr {}, 8, 1),

            // GMB 16bit-Load Commands
            // ld   rr,nn       x1 nn nn  12 ---- rr=nn (rr may be BC,DE,HL or SP)
            0x01 => (LoadRegWith16BitConstant { rr: B, nn: immediate_u16() }, 12, 3),
            0x11 => (LoadRegWith16BitConstant { rr: D, nn: immediate_u16() }, 12, 3),
            0x21 => (LoadRegWith16BitConstant { rr: H, nn: immediate_u16() }, 12, 3),
            0x31 => (LoadSPWith16BitConstant { nn: u16 }, 12, 1),
            // ld   SP,HL       F9         8 ---- SP=HL
            0xF9 => (LoadSPwithHL {}, 8, 1),
            // push rr          x5 (c5, d5, e5, f5)        16 ---- SP=SP-2  (SP)=rr   (rr may be BC,DE,HL,AF)
            0xc5 => (Push { rr: B }, 16, 1),
            0xd5 => (Push { rr: D }, 16, 1),
            0xe5 => (Push { rr: H }, 16, 1),
            0xf5 => (Push { rr: A }, 16, 1),
            // pop  rr          x1 (c1, d1, e1, f1)      12 (AF) rr=(SP)  SP=SP+2   (rr may be BC,DE,HL,AF)
            0xc1 => (Pop { rr: B }, 12, 1),
            0xd1 => (Pop { rr: D }, 12, 1),
            0xe1 => (Pop { rr: H }, 12, 1),
            0xf1 => (Pop { rr: A }, 12, 1),

            // GMB 8bit-Arithmetic/logical Commands
            //  add  A,r         8x         4 z0hc A=A+r
            0x80..0x86 => (AddRegister { r: reg_offset(op) }, 4, 1),
            0x87 => (AddRegister { r: A }, 4, 1),
            //  add  A,n         C6 nn      8 z0hc A=A+n
            0xC6 => (AddConstant { n: immediate_u8() }, 8, 2),
            //  add  A,(HL)      86         8 z0hc A=A+(HL)
            0x86 => (AddMemoryHL {}, 8, 1),
            //  adc  A,r         8x         4 z0hc A=A+r+cy
            0x88..0x8E => (AddRegisterWithCarry { r: reg_offset(op) }, 4, 1),
            0x8F => (AddRegisterWithCarry { r: A }, 4, 1),
            //  adc  A,n         CE nn      8 z0hc A=A+n+cy
            0xCE => (AddConstantWithCarry { n: immediate_u8() }, 8, 2),
            //  adc  A,(HL)      8E         8 z0hc A=A+(HL)+cy
            0x8E => (AddMemoryHLWithCarry {}, 8, 1),
            //  sub  r           9x         4 z1hc A=A-r
            0x90..0x96 => (SubtrackRegister { r: reg_offset(op) }, 4, 1),
            0x97 => (SubtrackRegister { r: A }, 4, 1),
            //  sub  n           D6 nn      8 z1hc A=A-n
            0xD6 => (SubtractConstant { n: immediate_u8() }, 8, 2),
            //  sub  (HL)        96         8 z1hc A=A-(HL)
            0x96 => (SubtractMemoryHL {}, 8, 1),
            //  sbc  A,r         9x         4 z1hc A=A-r-cy
            0x98..0x9E => (SubtractRegisterWithCarry { r: reg_offset(op) }, 4, 1),
            0x9F => (SubtractRegisterWithCarry { r: A }, 4, 1),
            //  sbc  A,n         DE nn      8 z1hc A=A-n-cy
            0xDE => (SubtractConstantWithCarry { n: immediate_u8() }, 8, 1),
            //  sbc  A,(HL)      9E         8 z1hc A=A-(HL)-cy
            0x9E => (SubtractMemoryHLWithCarry {}, 8, 1),
            //  and  r           Ax         4 z010 A=A & r
            0xA0..0xA6 => (AndRegister { r: reg_offset(op) }, 4, 1),
            0xA7 => (AndRegister { r: A }, 4, 1),
            //  and  n           E6 nn      8 z010 A=A & n
            0xE6 => (AndConstant { n: immediate_u8() }, 8, 2),
            //  and  (HL)        A6         8 z010 A=A & (HL)
            0xA6 => (AndMemoryHL {}, 8, 1),
            //  xor  r           Ax         4 z000
            0xA8..0xAE => (XorRegister { r: reg_offset(op) }, 4, 1),
            0xAF => (XorRegister { r: A }, 4, 1),
            //  xor  n           EE nn      8 z000
            0xEE => (XorConstant { n: immediate_u8() }, 8, 2),
            //  xor  (HL)        AE         8 z000
            0xAE => (XorMemoryHL {}, 8, 1),
            //  or   r           Bx         4 z000 A=A | r
            0xB0..0xB6 => (OrRegister { r: reg_offset(op) }, 4, 1),
            0xB7 => (OrRegister { r: A }, 4, 1),
            //  or   n           F6 nn      8 z000 A=A | n
            0xF6 => (OrConstant { n: immediate_u8() }, 8, 2),
            //  or   (HL)        B6         8 z000 A=A | (HL)
            0xB6 => (OrMemoryHL {}, 8, 1),
            //  cp   r           Bx         4 z1hc compare A-r
            0xB8..0xBE => (CompareRegister { r: reg_offset(op) }, 4, 1),
            0xBF => (CompareRegister { r: A }, 4, 1),
            //  cp   n           FE nn      8 z1hc compare A-n
            0xFE => (CompareConstant { n: immediate_u8() }, 8, 2),
            //  cp   (HL)        BE         8 z1hc compare A-(HL)
            0xBE => (CompareMemoryHL {}, 8, 1),
            //  inc  r           xx         4 z0h- r=r+1
            0x04 => (IncrementRegister { r: B }, 4, 1),
            0x0c => (IncrementRegister { r: C }, 4, 1),
            0x14 => (IncrementRegister { r: D }, 4, 1),
            0x1c => (IncrementRegister { r: E }, 4, 1),
            0x24 => (IncrementRegister { r: H }, 4, 1),
            0x2c => (IncrementRegister { r: L }, 4, 1),
            0x3c => (IncrementRegister { r: A }, 4, 1),
            //  inc  (HL)        34        12 z0h- (HL)=(HL)+1
            0x34 => (IncrementMemoryHL {}, 12, 1),
            //  dec  r           xx         4 z1h- r=r-1
            0x05 => (DecrementRegister { r: B }, 4, 1),
            0x0d => (DecrementRegister { r: C }, 4, 1),
            0x15 => (DecrementRegister { r: D }, 4, 1),
            0x1d => (DecrementRegister { r: E }, 4, 1),
            0x25 => (DecrementRegister { r: H }, 4, 1),
            0x2d => (DecrementRegister { r: L }, 4, 1),
            0x3d => (DecrementRegister { r: A }, 4, 1),
            //  dec  (HL)        35        12 z1h- (HL)=(HL)-1
            0x35 => (DecrementMemoryHL {}, 12, 1),
            //  daa              27         4 z-0x decimal adjust akku
            0x27 => (DecimalAdjust {}, 4, 1),
            //  cpl              2F         4 -11- A = A xor FF
            0x2F => (CPL {}, 4, 1),

            //GMB 16bit-Arithmetic/logical Commands
            //add  HL,rr     x9           8 -0hc HL = HL+rr     ;rr may be BC,DE,HL,SP
            0x09 => (AddRegisterHL16bit { r: B }, 8, 1),
            0x19 => (AddRegisterHL16bit { r: D }, 8, 1),
            0x29 => (AddRegisterHL16bit { r: H }, 8, 1),
            0x39 => (AddRegisterSPtoHL16bit {}, 8, 1),
            //inc  rr        x3           8 ---- rr = rr+1      ;rr may be BC,DE,HL,SP
            0x03 => (IncrementRegisterHL16bit { r: B }, 8, 1),
            0x13 => (IncrementRegisterHL16bit { r: D }, 8, 1),
            0x23 => (IncrementRegisterHL16bit { r: H }, 8, 1),
            0x33 => (IncrementRegisterSPtoHL16bit {}, 8, 1),
            //dec  rr        xB           8 ---- rr = rr-1      ;rr may be BC,DE,HL,SP
            0x0B => (DecrementRegisterHL16bit { r: B }, 8, 1),
            0x1B => (DecrementRegisterHL16bit { r: D }, 8, 1),
            0x2B => (DecrementRegisterHL16bit { r: H }, 8, 1),
            0x3B => (DecrementRegisterSPtoHL16bit {}, 8, 1),
            //add  SP,dd     E8          16 00hc SP = SP +/- dd ;dd is 8bit signed number
            0xE8 => (AddSP { d: immediate_i8() }, 16, 2),
            //ld   HL,SP+dd  F8          12 00hc HL = SP +/- dd ;dd is 8bit signed number
            0xF8 => (LoadHLwithSPplus { d: immediate_i8() }, 12, 2),

            //GMB Rotate- und Shift-Commands
            //rlca           07           4 000c rotate akku left
            0x07 => (RotateLeft {}, 4, 1),
            //rla            17           4 000c rotate akku left through carry
            0x17 => (RotateLeftThroughCarry {}, 4, 1),
            //rrca           0F           4 000c rotate akku right
            0x0F => (RotateRight {}, 4, 1),
            //rra            1F           4 000c rotate akku right through carry
            0x1F => (RotateRightThroughCarry {}, 4, 1),
            0xCB => {
                let op = self.mem[self.pc + 1];
                match op {
                    //rlc  r         CB 0x        8 z00c rotate left
                    0x00..0x06 => (RotateLeftRegisterThroughCarry { r: reg_offset(op) }, 8, 2),
                    0x07 => (RotateLeftRegisterThroughCarry { r: A }, 8, 2),
                    //rlc  (HL)      CB 06       16 z00c rotate left
                    0x06 => (RotateLeftHLThroughCarry {}, 16, 2),
                    //rrc  r         CB 0x        8 z00c rotate right
                    0x08..0x0E => (RotateRightRegisterThroughCarry { r: reg_offset(op) }, 8, 2),
                    0x0F => (RotateRightRegisterThroughCarry { r: A }, 8, 2),
                    //rrc  (HL)      CB 0E       16 z00c rotate right
                    0x0E => (RotateRightHLThroughCarry {}, 16, 2),
                    //rl   r         CB 1x        8 z00c rotate left through carry
                    0x10..0x16 => (RotateLeftRegister { r: reg_offset(op) }, 8, 2),
                    0x17 => (RotateLeftRegister { r: A }, 8, 2),
                    //rl   (HL)      CB 16       16 z00c rotate left through carry
                    0x16 => (RotateLeftHL {}, 16, 2),
                    //rr   r         CB 1x        8 z00c rotate right through carry
                    0x18..0x1E => (RotateRightRegister { r: reg_offset(op) }, 8, 2),
                    0x1F => (RotateRightRegister { r: A }, 8, 2),
                    //rr   (HL)      CB 1E       16 z00c rotate right through carry
                    0x1E => (RotateRightHL {}, 16, 2),
                    //sla  r         CB 2x        8 z00c shift left arithmetic (b0=0)
                    0x20..0x26 => (ShiftLeftRegister { r: reg_offset(op) }, 8, 2),
                    0x27 => (ShiftLeftRegister { r: A }, 8, 2),
                    //sla  (HL)      CB 26       16 z00c shift left arithmetic (b0=0)
                    0x26 => (ShiftLeftHL {}, 16, 2),
                    //sra  r         CB 2x        8 z00c shift right arithmetic (b7=b7)
                    0x28..0x2E => (ShiftRightArithmeticRegister { r: reg_offset(op) }, 8, 2),
                    0x2F => (ShiftRightArithmeticRegister { r: A }, 8, 2),
                    //sra  (HL)      CB 2E       16 z00c shift right arithmetic (b7=b7)
                    0x2E => (ShiftRightArithmeticHL {}, 16, 2),
                    //swap r         CB 3x        8 z000 exchange low/hi-nibble
                    0x30..0x36 => (SwapRegister { r: reg_offset(op) }, 8, 2),
                    0x37 => (SwapRegister { r: A }, 8, 2),
                    //swap (HL)      CB 36       16 z000 exchange low/hi-nibble
                    0x36 => (SwapHL {}, 16, 2),
                    //srl  r         CB 3x        8 z00c shift right logical (b7=0)
                    0x38..0x3E => (ShiftRightLogicalRegister { r: reg_offset(op) }, 8, 2),
                    0x3F => (ShiftRightLogicalRegister { r: A }, 8, 2),
                    //srl  (HL)      CB 3E       16 z00c shift right logical (b7=0)
                    0x3E => (ShiftRightLogicalHL {}, 16, 2),

                    //GMB Singlebit Operation Commands
                    //bit  n,r       CB xx        8 z01- test bit n
                    //bit  n,(HL)    CB xx       12 z01- test bit n
                    // TODO: The clock cycles for bit  n,(HL) may be incorrect! Inconsistencies in our data..
                    0x40..0x46 => (TestBitRegister { bit: 0, r: reg_offset(op) }, 8, 2),
                    0x46 => (TestBitMemoryHL { bit: 0 }, 16, 2),
                    0x47 => (TestBitRegister { bit: 0, r: A }, 8, 2),

                    0x48..0x4E => (TestBitRegister { bit: 1, r: reg_offset(op) }, 8, 2),
                    0x4E => (TestBitMemoryHL { bit: 1 }, 16, 2),
                    0x4F => (TestBitRegister { bit: 1, r: A }, 8, 2),

                    0x50..0x56 => (TestBitRegister { bit: 2, r: reg_offset(op) }, 8, 2),
                    0x56 => (TestBitMemoryHL { bit: 2 }, 16, 2),
                    0x57 => (TestBitRegister { bit: 2, r: A }, 8, 2),

                    0x58..0x5E => (TestBitRegister { bit: 3, r: reg_offset(op) }, 8, 2),
                    0x5E => (TestBitMemoryHL { bit: 3 }, 16, 2),
                    0x5F => (TestBitRegister { bit: 3, r: A }, 8, 2),

                    0x60..0x66 => (TestBitRegister { bit: 4, r: reg_offset(op) }, 8, 2),
                    0x66 => (TestBitMemoryHL { bit: 4 }, 16, 2),
                    0x67 => (TestBitRegister { bit: 4, r: A }, 8, 2),

                    0x68..0x6E => (TestBitRegister { bit: 5, r: reg_offset(op) }, 8, 2),
                    0x6E => (TestBitMemoryHL { bit: 5 }, 16, 2),
                    0x6F => (TestBitRegister { bit: 5, r: A }, 8, 2),

                    0x70..0x76 => (TestBitRegister { bit: 6, r: reg_offset(op) }, 8, 2),
                    0x76 => (TestBitMemoryHL { bit: 6 }, 16, 2),
                    0x77 => (TestBitRegister { bit: 6, r: A }, 8, 2),

                    0x78..0x7E => (TestBitRegister { bit: 7, r: reg_offset(op) }, 8, 2),
                    0x7E => (TestBitMemoryHL { bit: 7 }, 16, 2),
                    0x7F => (TestBitRegister { bit: 7, r: A }, 8, 2),

                    //res  n,r       CB xx        8 ---- reset bit n
                    //res  n,(HL)    CB xx       16 ---- reset bit n
                    0x80..0x86 => (ResetBitRegister { bit: 0, r: reg_offset(op) }, 8, 2),
                    0x86 => (ResetBitMemoryHL { bit: 0 }, 16, 2),
                    0x87 => (ResetBitRegister { bit: 0, r: A }, 8, 2),

                    0x88..0x8E => (ResetBitRegister { bit: 1, r: reg_offset(op) }, 8, 2),
                    0x8E => (ResetBitMemoryHL { bit: 1 }, 16, 2),
                    0x8F => (ResetBitRegister { bit: 1, r: A }, 8, 2),

                    0x90..0x96 => (ResetBitRegister { bit: 2, r: reg_offset(op) }, 8, 2),
                    0x96 => (ResetBitMemoryHL { bit: 2 }, 16, 2),
                    0x97 => (ResetBitRegister { bit: 2, r: A }, 8, 2),

                    0x98..0x9E => (ResetBitRegister { bit: 3, r: reg_offset(op) }, 8, 2),
                    0x9E => (ResetBitMemoryHL { bit: 3 }, 16, 2),
                    0x9F => (ResetBitRegister { bit: 3, r: A }, 8, 2),

                    0xA0..0xA6 => (ResetBitRegister { bit: 4, r: reg_offset(op) }, 8, 2),
                    0xA6 => (ResetBitMemoryHL { bit: 4 }, 16, 2),
                    0xA7 => (ResetBitRegister { bit: 4, r: A }, 8, 2),

                    0xA8..0xAE => (ResetBitRegister { bit: 5, r: reg_offset(op) }, 8, 2),
                    0xAE => (ResetBitMemoryHL { bit: 5 }, 16, 2),
                    0xAF => (ResetBitRegister { bit: 5, r: A }, 8, 2),

                    0xB0..0xB6 => (ResetBitRegister { bit: 6, r: reg_offset(op) }, 8, 2),
                    0xB6 => (ResetBitMemoryHL { bit: 6 }, 16, 2),
                    0xB7 => (ResetBitRegister { bit: 6, r: A }, 8, 2),

                    0xB8..0xBE => (ResetBitRegister { bit: 7, r: reg_offset(op) }, 8, 2),
                    0xBE => (ResetBitMemoryHL { bit: 7 }, 16, 2),
                    0xBF => (ResetBitRegister { bit: 7, r: A }, 8, 2),

                    //set  n,r       CB xx        8 ---- set bit n
                    //set  n,(HL)    CB xx       16 ---- set bit n
                    0xC0..0xC6 => (SetBitRegister { bit: 0, r: reg_offset(op) }, 8, 2),
                    0xC6 => (SetBitMemoryHL { bit: 0 }, 16, 2),
                    0xC7 => (SetBitRegister { bit: 0, r: A }, 8, 2),

                    0xC8..0xCE => (SetBitRegister { bit: 1, r: reg_offset(op) }, 8, 2),
                    0xCE => (SetBitMemoryHL { bit: 1 }, 16, 2),
                    0xCF => (SetBitRegister { bit: 1, r: A }, 8, 2),

                    0xD0..0xD6 => (SetBitRegister { bit: 2, r: reg_offset(op) }, 8, 2),
                    0xD6 => (SetBitMemoryHL { bit: 2 }, 16, 2),
                    0xD7 => (SetBitRegister { bit: 2, r: A }, 8, 2),

                    0xD8..0xDE => (SetBitRegister { bit: 3, r: reg_offset(op) }, 8, 2),
                    0xDE => (SetBitMemoryHL { bit: 3 }, 16, 2),
                    0xDF => (SetBitRegister { bit: 3, r: A }, 8, 2),

                    0xE0..0xE6 => (SetBitRegister { bit: 4, r: reg_offset(op) }, 8, 2),
                    0xE6 => (SetBitMemoryHL { bit: 4 }, 16, 2),
                    0xE7 => (SetBitRegister { bit: 4, r: A }, 8, 2),

                    0xE8..0xEE => (SetBitRegister { bit: 5, r: reg_offset(op) }, 8, 2),
                    0xEE => (SetBitMemoryHL { bit: 5 }, 16, 2),
                    0xEF => (SetBitRegister { bit: 5, r: A }, 8, 2),

                    0xF0..0xF6 => (SetBitRegister { bit: 6, r: reg_offset(op) }, 8, 2),
                    0xF6 => (SetBitMemoryHL { bit: 6 }, 16, 2),
                    0xF7 => (SetBitRegister { bit: 6, r: A }, 8, 2),

                    0xF8..0xFE => (SetBitRegister { bit: 7, r: reg_offset(op) }, 8, 2),
                    0xFE => (SetBitMemoryHL { bit: 7 }, 16, 2),
                    0xFF => (SetBitRegister { bit: 7, r: A }, 8, 2),
                    _ => unimplemented!()
                }
            },
            //GMB CPU-Control Commands
            //ccf            3F           4 -00c cy=cy xor 1
            0x3F => (CCF {}, 4, 1),
            //scf            37           4 -001 cy=1
            0x37 => (SCF {}, 4, 1),
            //nop            00           4 ---- no operation
            0x00 => (NOP {}, 4, 1),
            //halt           76         N*4 ---- halt until interrupt occurs (low power)
            // TODO: N*4 = ?
            0x76 => (HALT {}, 4, 1),
            //stop           10 00        ? ---- low power standby mode (VERY low power)
            // TODO: Num cycles unknown?
            0x10 => (STOP {}, 1, 1),
            //di             F3           4 ---- disable interrupts, IME=0
            0xF3 => (DI {}, 4, 1),
            //ei             FB           4 ---- enable interrupts, IME=1
            0xFB => (EI {}, 4, 1),

            //GMB Jump Commands
            //jp   nn        C3 nn nn    16 ---- jump to nn, PC=nn
            0xC3 => (JP { nn: immediate_u16() }, 16, 3),
            //jp   HL        E9           4 ---- jump to HL, PC=HL
            0xE9 => (JPtoMemoryHL {}, 4, 1),
            //jp   f,nn      xx nn nn 16;12 ---- conditional jump if nz,z,nc,c
            0xC2 => (CondJP { flag: ConditionalFlag::NotZero {}, dd: immediate_u16() }, 1, 3),
            0xCA => (CondJP { flag: ConditionalFlag::Zero {}, dd: immediate_u16() }, 1, 3),
            0xD2 => (CondJP { flag: ConditionalFlag::NotCarry {}, dd: immediate_u16() }, 1, 3),
            0xDA => (CondJP { flag: ConditionalFlag::Carry {}, dd: immediate_u16() }, 1, 3),
            //jr   PC+dd     18 dd       12 ---- relative jump to nn (PC=PC+/-7bit)
            0x18 => (JRtoMemory { dd: usize }, 12, 1),
            //jr   f,PC+dd   xx dd     12;8 ---- conditional relative jump if nz,z,nc,c
            // TODO: Duty cycles depend on conditional too!
            0x20 => (CondJR { flag: ConditionalFlag::NotZero {}, dd: immediate_i8() }, 1, 2),
            0x28 => (CondJR { flag: ConditionalFlag::Zero {}, dd: immediate_i8() }, 1, 2),
            0x30 => (CondJR { flag: ConditionalFlag::NotCarry {}, dd: immediate_i8() }, 1, 2),
            0x38 => (CondJR { flag: ConditionalFlag::Carry {}, dd: immediate_i8() }, 1, 2),
            //call nn        CD nn nn    24 ---- call to nn, SP=SP-2, (SP)=PC, PC=nn
            0xCD => (CALL { nn: immediate_u16() }, 24, 1),
            //call f,nn      xx nn nn 24;12 ---- conditional call if nz,z,nc,c
            0xC4 => (CondCALL { flag: ConditionalFlag::NotZero {}, dd: immediate_u16() }, 1, 3),
            0xCC => (CondCALL { flag: ConditionalFlag::Zero {}, dd: immediate_u16() }, 1, 3),
            0xD4 => (CondCALL { flag: ConditionalFlag::NotCarry {}, dd: immediate_u16() }, 1, 3),
            0xDC => (CondCALL { flag: ConditionalFlag::Carry {}, dd: immediate_u16() }, 1, 3),
            //ret            C9          16 ---- return, PC=(SP), SP=SP+2
            0x16 => (RET {}, 16, 1),
            //ret  f         xx        20;8 ---- conditional return if nz,z,nc,c
            0xC0 => (CondRET { flag: ConditionalFlag::NotZero {} }, 1, 3),
            0xC8 => (CondRET { flag: ConditionalFlag::Zero {} }, 1, 3),
            0xD0 => (CondRET { flag: ConditionalFlag::NotCarry {} }, 1, 3),
            0xD8 => (CondRET { flag: ConditionalFlag::Carry {} }, 1, 3),
            //reti           D9          16 ---- return and enable interrupts (IME=1)
            0xD9 => (RETI {}, 16, 1),
            //rst  n         xx          16 ---- call to 00,08,10,18,20,28,30,38
            0xC7 => (RST { n: 0x00 }, 16, 1),
            0xCF => (RST { n: 0x08 }, 16, 1),
            0xD7 => (RST { n: 0x10 }, 16, 1),
            0xDF => (RST { n: 0x18 }, 16, 1),
            0xE7 => (RST { n: 0x20 }, 16, 1),
            0xEF => (RST { n: 0x28 }, 16, 1),
            0xF7 => (RST { n: 0x30 }, 16, 1),
            0xFF => (RST { n: 0x38 }, 16, 1),
            _ => unreachable!();
        }
    }
}