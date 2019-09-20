use crate::ConditionalFlag::*;
use crate::Instruction::*;

// @formatter:off
type RegisterIndex = usize;
type MemoryAddress = usize;

enum ConditionalFlag {
    Zero{},
    Subtract{},
    HalfCarry{},
    Carry{},
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
    LoadMemoryFF00plusNwithA {nn: usize},
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
    IncrementSP{},
    //dec  rr        xB           8 ---- rr = rr-1      ;rr may be BC,DE,HL,SP
    DecrementRegister16bit{r: RegisterIndex},
    DecrementSP{},
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
    CondJP{flag: ConditionalFlag, nn: usize},
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
    sp: usize,
    is_halted: bool,
    is_stopped: bool,
    are_interrupts_enabled: bool,
}

// A: Arithmetic, F: Flags
const A: usize = 0;
const F: usize = 1;
const AF: usize = A;
const B: usize = 2;
const C: usize = 3;
const BC: usize = B;
const D: usize = 4;
const E: usize = 5;
const DE: usize = E;
const H: usize = 6;
const L: usize = 7;
const HL: usize = H;

struct Flags {
    z: bool,
    n: bool,
    h: bool,
    c: bool,
}

impl Cpu {
    fn write_reg_16(&mut self, i: usize, val: u16) {
        assert!(i < self.reg.len() - 1);
        self.reg[i] = (val >> 8) as u8;
        self.reg[i + 1] = (val & 0xFF) as u8;
    }

    fn read_reg_16(&self, i: usize) -> usize {
        assert!(i < self.reg.len() - 1);
        ((self.reg[i] as usize) << 8) | self.reg[i + 1] as usize
    }

    fn write_mem_16(&mut self, i: usize, val: usize) {
        assert!(i < self.mem.len() - 1);
        self.mem[i] = (val >> 8) as u8;
        self.mem[i + 1] = (val & 0xFF) as u8;
    }

    fn read_mem_16(&self, i: usize) -> usize {
        assert!(i < self.mem.len() - 1);
        ((self.mem[i] as usize) << 8) | self.mem[i + 1] as usize
    }

    fn set_flag(&mut self, flag: ConditionalFlag, state: bool) {
        // 7      	    6            	5            	4 	        3 	2 	1 	0
        // Z (Zero) 	N (Subtract) 	H (Half Carry) 	C (Carry) 	0 	0 	0 	0
        match flag {
            Zero {} => { self.reg[F] &= (state as u8) << 7; },
            Subtract {} => { self.reg[F] &= (state as u8) << 6; },
            HalfCarry {} => { self.reg[F] &= (state as u8) << 5; },
            Carry {} => { self.reg[F] &= (state as u8) << 4; },
        }
    }
    fn ith_bit(n: u8, i: usize) -> bool {
        n & (1 << i as u8) == (1 << i as u8)
    }

    fn get_flag(&self, flag: ConditionalFlag) -> bool {
        // 7      	    6            	5            	4 	        3 	2 	1 	0
        // Z (Zero) 	N (Subtract) 	H (Half Carry) 	C (Carry) 	0 	0 	0 	0
        match flag {
            Zero {} => Cpu::ith_bit(self.reg[F], 7),
            Subtract {} => Cpu::ith_bit(self.reg[F], 6),
            HalfCarry {} => Cpu::ith_bit(self.reg[F], 5),
            Carry {} => Cpu::ith_bit(self.reg[F], 4),
        }
    }

    fn set_flags(&mut self, flags: Flags) {
        self.set_flag(Zero {}, flags.z);
        self.set_flag(Subtract {}, flags.n);
        self.set_flag(HalfCarry {}, flags.h);
        self.set_flag(Carry {}, flags.c);
    }

    fn add_16bit(a: u32, b: u32) -> (u16, Flags) {
        let res = a + b;

        let half_carry = ((a ^ b ^ res) & 0x1000) == 0x1000;

        (res as u16, Flags { z: res == 0, n: false, h: half_carry, c: res > 0xFFFF })
    }

    fn sub_16bit(a: i32, b: i32) -> (u16, Flags) {
        let res = a - b;

        let half_carry = ((a ^ (-b) ^ res) & 0x1000) == 0x1000;

        (res as u16, Flags { z: res == 0, n: true, h: half_carry, c: res < 0 })
    }

    fn add_8bit(a: u16, b: u16) -> (u8, Flags) {
        let res = a + b;

        let half_carry = ((a ^ b ^ res) & 0x0010) == 0x0010;

        (res as u8, Flags { z: res == 0, n: false, h: half_carry, c: res > 0xFF })
    }

    fn sub_8bit(a: i16, b: i16) -> (u8, Flags) {
        let res = a - b;

        let half_carry = ((a ^ (-b) ^ res) & 0x0010) == 0x0010;

        (res as u8, Flags { z: res == 0, n: true, h: half_carry, c: res < 0 })
    }

    fn execute_instruction(&mut self, instr: Instruction) -> u8 {
        let half_carry_add_16 = |a, b, res| ((a ^ b ^ res) & 0x1000) == 0x1000;

        let half_carry_sub = |a, b, res| ((a ^ (-b) ^ res) & 0x0010) == 0x0010;
        let half_carry_sub_16 = |a, b, res| ((a ^ (-b) ^ res) & 0x1000) == 0x1000;

        let rotate_left = |x| ((x << 1) | Cpu::ith_bit(x, 7), Cpu::ith_bit(x, 7));
        let rotate_left_carry = |x| ((x << 1) | self.get_flag(Carry {}), Cpu::ith_bit(x, 7));

        let rotate_right = |x| ((x >> 1) | ((Cpu::ith_bit(x, 0) as u8) << 7), Cpu::ith_bit(x, 0));
        let rotate_right_carry = |x| ((x >> 1) | ((self.get_flag(Carry {}) as u8) << 7), Cpu::ith_bit(x, 0));

        match instr {
            // GMB 8bit-Load commands
            // ld   r,r         xx         4 ---- r=r
            LoadReg { to, from } => {
                assert!(to < self.reg.len(), from < self.reg.len());
                self.reg[to] = self.reg[from];
                4
            },
            // ld   r,n         xx nn      8 ---- r=n
            LoadRegWithConstant { to, n } => {
                assert!(to < self.reg.len());
                self.reg[to] = n;
                8
            },
            // ld   r,(HL)      xx         8 ---- r=(HL)
            LoadRegWithMemoryHL { to } => {
                assert!(to < self.reg.len());
                self.reg[to] = self.mem[self.read_reg_16(HL)];
                8
            },
            // ld   (HL),r      7x         8 ---- (HL)=r
            LoadMemoryHLwithRegister { from } => {
                assert!(from < self.reg.len());
                self.mem[self.read_reg_16(HL)] = self.reg[from];
                8
            },
            // ld   (HL),n      36 nn     12 ----
            LoadMemoryHLwithConstant { n } => {
                self.mem[self.read_reg_16(HL)] = n;
                12
            },
            // ld   A,(BC)      0A         8 ----
            LoadAwithValBC {} => {
                self.reg[A] = self.mem[self.reg[BC] as usize];
                8
            },
            // ld   A,(DE)      1A         8 ----
            LoadAwithValDE {} => {
                self.reg[A] = self.mem[self.reg[DE] as usize];
                8
            },
            // ld   A,(nn)      FA        16 ----
            LoadAwithMemory { nn } => {
                assert!(nn < 0xFFFF);
                self.reg[A] = self.mem[nn];
                16
            },
            // ld   (BC),A      02         8 ----
            LoadMemoryBCwithA {} => {
                self.mem[BC] = self.reg[A];
                8
            },
            // ld   (DE),A      12         8 ----
            LoadMemoryDEwithA {} => {
                self.mem[DE] = self.reg[A];
                8
            },
            // ld   (nn),A      EA        16 ----
            LoadMemoryNNwithA { nn } => {
                assert!(nn < 0xFFFF);
                self.mem[nn] = self.reg[A];
                16
            },
            // ld   A,(FF00+n)  F0 nn     12 ---- read from io-port n (memory FF00+n)
            LoadAwithFF00plusN { nn } => {
                assert!(nn + 0xFF00 < 0xFFFF);
                self.reg[A] = self.mem[nn + 0xFF00];
                12
            },
            // ld   (FF00+n),A  E0 nn     12 ---- write to io-port n (memory FF00+n)
            LoadMemoryFF00plusNwithA { nn } => {
                assert!(nn + 0xFF00 < 0xFFFF);
                self.mem[nn + 0xFF00] = self.reg[A];
                12
            },
            // ld   A,(FF00+C)  F2         8 ---- read from io-port C (memory FF00+C)
            LoadAwithFF00plusC {} => {
                let idx = 0xFF00 + self.reg[C] as usize;
                assert!(idx < 0xFFFF);
                self.reg[A] = self.mem[idx];
                8
            },
            // ld   (FF00+C),A  E2         8 ---- write to io-port C (memory FF00+C)
            LoadMemoryFF00plusCwithA {} => {
                let idx = 0xFF00 + self.reg[C] as usize;
                assert!(idx < 0xFFFF);
                self.mem[idx] = self.reg[A];
                8
            },
            // ldi  (HL),A      22         8 ---- (HL)=A, HL=HL+1
            LoadMemoryHLwithAandIncr {} => {
                let val_hl = self.read_reg_16(HL);
                self.mem[val_hl] = self.reg[A];

                self.write_reg_16(HL, val_hl + 1);
                8
            },
            // ldi  A,(HL)      2A         8 ---- A=(HL), HL=HL+1
            LoadAwithValHLandIncr {} => {
                let val_hl = self.read_reg_16(HL);
                self.reg[A] = self.mem[val_hl];

                self.write_reg_16(HL, val_hl + 1);
                8
            },
            // ldd  (HL),A      32         8 ---- (HL)=A, HL=HL-1
            LoadMemoryHLwithAandDecr {} => {
                let val_hl = self.read_reg_16(HL);
                self.mem[val_hl] = self.reg[A];

                self.write_reg_16(HL, val_hl - 1);
                8
            },
            // ldd  A,(HL)      3A         8 ---- A=(HL), HL=HL-1
            LoadAwithValHLandDecr {} => {
                let val_hl = self.read_reg_16(HL);
                self.reg[A] = self.mem[val_hl];

                self.write_reg_16(HL, val_hl - 1);
                8
            },

            // GMB 16bit-Load Commands
            // ld   rr,nn       x1 nn nn  12 ---- rr=nn (rr may be BC,DE,HL or SP)
            LoadRegWith16BitConstant { rr, nn } => {
                assert!(nn < 0xFFFF);
                self.write_reg_16(rr, nn as usize);
                12
            },
            LoadSPWith16BitConstant { nn } => {
                assert!(nn < 0xFFFF);
                self.sp = nn as usize;
                12
            },
            // ld   SP,HL       F9         8 ---- SP=HL
            LoadSPwithHL {} => {
                self.sp = self.read_reg_16(HL);
                8
            },
            // push rr          x5 (c5, d5, e5, f5)        16 ---- SP=SP-2  (SP)=rr   (rr may be BC,DE,HL,AF)
            Push { rr } => {
                self.sp -= 2;
                self.write_mem_16(self.sp, self.read_reg_16(rr));
                16
            },
            // pop  rr          x1 (c1, d1, e1, f1)      12 (AF) rr=(SP)  SP=SP+2   (rr may be BC,DE,HL,AF)
            Pop { rr } => {
                self.write_reg_16(rr, self.read_mem_16(self.sp));
                self.sp += 2;
                12
            },

            // GMB 8bit-Arithmetic/logical Commands
            //  add  A,r         8x         4 z0hc A=A+r
            AddRegister { r } => {
                let (val, f) = Cpu::add_8bit(self.reg[A] as u16, self.reg[r] as u16);
                self.reg[A] = val;
                self.set_flags(f);
                4
            },
            //  add  A,n         C6 nn      8 z0hc A=A+n
            AddConstant { n } => {
                let (val, f) = Cpu::add_8bit(self.reg[A] as u16, n as u16);
                self.reg[A] = val;
                self.set_flags(f);
                8
            },
            //  add  A,(HL)      86         8 z0hc A=A+(HL)
            AddMemoryHL {} => {
                let (val, f) = Cpu::add_8bit(self.reg[A] as u16, self.mem[self.read_reg_16(HL)] as u16);
                self.reg[A] = val;
                self.set_flags(f);
                8
            },
            //  adc  A,r         8x         4 z0hc A=A+r+cy
            AddRegisterWithCarry { r } => {
                assert!(r < self.reg.len());
                let (val, f) = Cpu::add_8bit(self.reg[A] as u16, self.reg[r] as u16 + self.get_flag(Carry {}) as u16);
                self.reg[A] = val;
                self.set_flags(f);
                4
            },
            //  adc  A,n         CE nn      8 z0hc A=A+n+cy
            AddConstantWithCarry { n } => {
                let (val, f) = Cpu::add_8bit(self.reg[A] as u16, n as u16 + self.get_flag(Carry {}) as u16);
                self.reg[A] = val;
                self.set_flags(f);
                8
            },
            //  adc  A,(HL)      8E         8 z0hc A=A+(HL)+cy
            AddMemoryHLWithCarry {} => {
                let (val, f) = Cpu::add_8bit(self.reg[A] as u16, self.mem[self.read_reg_16(HL)] as u16 + self.get_flag(Carry {}) as u16);
                self.reg[A] = val;
                self.set_flags(f);
                8
            },
            //  sub  r           9x         4 z1hc A=A-r
            SubtractRegister { r } => {
                assert!(r < self.reg.len());
                let (val, f) = Cpu::sub_8bit(self.reg[A] as i16, self.reg[r] as i16);
                self.reg[A] = val;
                self.set_flags(f);
                4
            },
            //  sub  n           D6 nn      8 z1hc A=A-n
            SubtractConstant { n } => {
                let (val, f) = Cpu::sub_8bit(self.reg[A] as i16, n as i16);
                self.reg[A] = val;
                self.set_flags(f);
                8
            },
            //  sub  (HL)        96         8 z1hc A=A-(HL)
            SubtractMemoryHL {} => {
                let (val, f) = Cpu::sub_8bit(self.reg[A] as i16, self.mem[self.read_reg_16(HL)] as i16);
                self.reg[A] = val;
                self.set_flags(f);
                8
            },
            //  sbc  A,r         9x         4 z1hc A=A-r-cy
            SubtractRegisterWithCarry { r } => {
                assert!(r < self.reg.len());
                let (val, f) = Cpu::sub_8bit(self.reg[A] as i16, self.reg[r] as i16 - self.get_flag(Carry {}) as i16);
                self.reg[A] = val;
                self.set_flags(f);
                4
            },
            //  sbc  A,n         DE nn      8 z1hc A=A-n-cy
            SubtractConstantWithCarry { n } => {
                let (val, f) = Cpu::sub_8bit(self.reg[A] as i16, n as i16 - self.get_flag(Carry {}) as i16);
                self.reg[A] = val;
                self.set_flags(f);
                4
            },
            //  sbc  A,(HL)      9E         8 z1hc A=A-(HL)-cy
            SubtractMemoryHLWithCarry {} => {
                let (val, f) = Cpu::sub_8bit(self.reg[A] as i16, self.mem[self.read_reg_16(HL)] as i16 - self.get_flag(Carry {}) as i16);
                self.reg[A] = val;
                self.set_flags(f);
                8
            },
            //  and  r           Ax         4 z010 A=A & r
            AndRegister { r } => {
                self.reg[A] &= self.reg[r];
                self.set_flags(Flags { z: self.reg[A] == 0, n: false, h: true, c: false });
                4
            },
            //  and  n           E6 nn      8 z010 A=A & n
            AndConstant { n } => {
                self.reg[A] &= n;
                self.set_flags(Flags { z: self.reg[A] == 0, n: false, h: true, c: false });
                8
            },
            //  and  (HL)        A6         8 z010 A=A & (HL)
            AndMemoryHL {} => {
                self.reg[A] &= self.mem[self.read_reg_16(HL)];
                self.set_flags(Flags { z: self.reg[A] == 0, n: false, h: true, c: false });
                8
            },
            //  xor  r           Ax         4 z000
            XorRegister { r } => {
                self.reg[A] ^= self.reg[r];
                self.set_flags(Flags { z: self.reg[A] == 0, n: false, h: false, c: false });
                4
            },
            //  xor  n           EE nn      8 z000
            XorConstant { n } => {
                self.reg[A] ^= n;
                self.set_flags(Flags { z: self.reg[A] == 0, n: false, h: false, c: false });
                8
            },
            //  xor  (HL)        AE         8 z000
            XorMemoryHL {} => {
                self.reg[A] ^= self.mem[self.read_reg_16(HL)];
                self.set_flags(Flags { z: self.reg[A] == 0, n: false, h: false, c: false });
                8
            },
            //  or   r           Bx         4 z000 A=A | r
            OrRegister { r } => {
                self.reg[A] |= self.reg[r];
                self.set_flags(Flags { z: self.reg[A] == 0, n: false, h: false, c: false });
                8
            },
            //  or   n           F6 nn      8 z000 A=A | n
            OrConstant { n } => {
                self.reg[A] |= n;
                self.set_flags(Flags { z: self.reg[A] == 0, n: false, h: false, c: false });
                8
            },
            //  or   (HL)        B6         8 z000 A=A | (HL)
            OrMemoryHL {} => {
                self.reg[A] |= self.mem[self.read_reg_16(HL)];
                self.set_flags(Flags { z: self.reg[A] == 0, n: false, h: false, c: false });
                8
            },
            //  cp   r           Bx         4 z1hc compare A-r
            CompareRegister { r } => {
                let (_, f) = Cpu::sub_8bit(self.reg[A] as i16, self.reg[r] as i16);
                self.set_flags(f);
                4
            },
            //  cp   n           FE nn      8 z1hc compare A-n
            CompareConstant { n } => {
                let (_, f) = Cpu::sub_8bit(self.reg[A] as i16, n as i16);
                self.set_flags(f);
                8
            },
            //  cp   (HL)        BE         8 z1hc compare A-(HL)
            CompareMemoryHL {} => {
                let (_, f) = Cpu::sub_8bit(self.reg[A] as i16, self.mem[self.read_reg_16(HL)] as i16);
                self.set_flags(f);
                8
            },
            //  inc  r           xx         4 z0h- r=r+1
            IncrementRegister { r } => {
                assert!(r < self.reg.len());
                let (val, f) = Cpu::add_8bit(self.reg[r] as u16, 1 as u16);
                self.set_flags(Flags { z: f.z, n: false, h: f.h, c: self.get_flag(Carry {}) });
                self.reg[r] = val;
                4
            },
            //  inc  (HL)        34        12 z0h- (HL)=(HL)+1
            IncrementMemoryHL {} => {
                self.mem[self.read_reg_16(HL)] += 1;
                12
            },
            //  dec  r           xx         4 z1h- r=r-1
            DecrementRegister { r } => {
                self.reg[A] -= 1;
                4
            },
            //  dec  (HL)        35        12 z1h- (HL)=(HL)-1
            DecrementMemoryHL {} => {
                self.mem[self.read_reg_16(HL)] -= 1;
                12
            },
            //  daa              27         4 z-0x decimal adjust akku
            DecimalAdjust {} => {
                // The BCD Flags (N, H)
                // These flags are (rarely) used for the DAA instruction only, N Indicates whether the previous instruction has been an addition or subtraction, and H indicates carry for lower 4bits of the result, also for DAA, the C flag must indicate carry for upper 8bits.
                // After adding/subtracting two BCD numbers, DAA is intended to convert the result into BCD format; BCD numbers are ranged from 00h to 99h rather than 00h to FFh.
                // Because C and H flags must contain carry-outs for each digit, DAA cannot be used for 16bit operations (which have 4 digits), or for INC/DEC operations (which do not affect C-flag).

                // TODO: Do we need to set all flags?
                // note: assumes a is a uint8_t and wraps from 0xff to 0
                if !self.get_flag(Subtract {}) {
                    // after an addition, adjust if (half-)carry occurred or if result is out of bounds
                    if self.get_flag(Carry {}) || self.reg[A] > 0x99 {
                        let (val, f) = Cpu::add_8bit(self.reg[A] as u16, 0x60 as u16);
                        self.reg[A] = val;
                        self.set_flags(f);
                    }
                    if self.get_flag(HalfCarry {}) || (self.reg[A] & 0x0f) > 0x09 {
                        let (val, f) = Cpu::add_8bit(self.reg[A] as u16, 0x6 as u16);
                        self.reg[A] = val;
                        self.set_flags(f);
                    }
                } else {  // after a subtraction, only adjust if (half-)carry occurred
                    if self.get_flag(Carry {}) {
                        let (val, f) = Cpu::sub_8bit(self.reg[A] as i16, 0x60 as i16);
                        self.reg[A] = val;
                    }
                    if self.get_flag(HalfCarry {}) {
                        let (val, f) = Cpu::sub_8bit(self.reg[A] as i16, 0x6 as i16);
                        self.reg[A] = val;
                    }
                }

                self.set_flag(HalfCarry {}, false);
                4
            },
            //  cpl              2F         4 -11- A = A xor FF
            CPL {} => {
                // Complement all bits
                self.reg[A] ^= 0xFF;
                4
            },

            //GMB 16bit-Arithmetic/logical Commands
            //add  HL,rr     x9           8 -0hc HL = HL+rr     ;rr may be BC,DE,HL,SP
            AddRegisterHL16bit { r } => {
                let (val, f) = Cpu::add_16bit(self.read_reg_16(HL) as u32, self.read_reg_16(r) as u32);
                self.write_reg_16(HL, val);
                self.set_flags(Flags { z: self.get_flag(Zero {}), n: false, h: f.h, c: f.c });
                8
            },
            AddRegisterSPtoHL16bit {} => {
                let (val, f) = Cpu::add_16bit(self.read_reg_16(HL) as u32, self.sp as u32);
                self.write_reg_16(HL, val);
                self.set_flags(Flags { z: self.get_flag(Zero {}), n: false, h: f.h, c: f.c });
                8
            },
            //inc  rr        x3           8 ---- rr = rr+1      ;rr may be BC,DE,HL,SP
            IncrementRegister16bit { r } => {
                let (val, _) = Cpu::add_16bit(self.read_reg_16(r) as u32, 1 as u32);
                self.write_reg_16(r, val);
                8
            },
            IncrementSP {} => {
                self.sp += 1;
                8
            },
            //dec  rr        xB           8 ---- rr = rr-1      ;rr may be BC,DE,HL,SP
            DecrementRegister16bit { r } => {
                let (val, f) = Cpu::sub_16bit(self.read_reg_16(r) as i32, 1 as i32);
                self.write_reg_16(HL, val);
                8
            },
            DecrementSP {} => {
                self.sp -= 1;
                8
            },
            //add  SP,dd     E8          16 00hc SP = SP +/- dd ;dd is 8bit signed number
            AddSP { d } => {
                let (val, f) = if d > 0 {
                    Cpu::add_16bit(self.sp as u32, d as u32)
                } else {
                    Cpu::sub_16bit(self.sp as i32, d.abs() as i32)
                };
                self.sp = val as usize;
                self.set_flags(Flags { z: false, n: false, h: f.h, c: f.c });
                16
            },
            //ld   HL,SP+dd  F8          12 00hc HL = SP +/- dd ;dd is 8bit signed number
            LoadHLwithSPplus { d } => {
                let (val, f) = if d > 0 {
                    Cpu::add_16bit(self.sp as u32, d as u32)
                } else {
                    Cpu::sub_16bit(self.sp as i32, d.abs() as i32)
                };
                self.write_reg_16(HL, val);
                self.set_flags(Flags { z: false, n: false, h: f.h, c: f.c });
                12
            },

            //GMB Rotate- und Shift-Commands
            //rlca           07           4 000c rotate akku left
            RotateLeft {} => {
                let (val, carry) = rotate_left(self.reg[A]);
                self.reg[A] = val;
                self.set_flags(Flags { z: false, n: false, h: false, c: carry });
                4
            },
            //rla            17           4 000c rotate akku left through carry
            RotateLeftThroughCarry {} => {
                let (val, carry) = rotate_left_carry(self.reg[A]);
                self.reg[A] = val;
                self.set_flags(Flags { z: false, n: false, h: false, c: carry });
                4
            },
            //rrca           0F           4 000c rotate akku right
            RotateRight {} => {
                let (val, carry) = rotate_right(self.reg[A]);
                self.reg[A] = val;
                self.set_flags(Flags { z: false, n: false, h: false, c: carry });
                4
            },
            //rra            1F           4 000c rotate akku right through carry
            RotateRightThroughCarry {} => {
                let (val, carry) = rotate_right_carry(self.reg[A]);
                self.reg[A] = val;
                self.set_flags(Flags { z: false, n: false, h: false, c: carry });
                4
            },
            //rlc  r         CB 0x        8 z00c rotate left
            RotateLeftRegisterThroughCarry { r } => {
                let (val, carry) = rotate_left(self.reg[r]);
                self.reg[r] = val;
                self.set_flags(Flags { z: val == 0, n: false, h: false, c: carry });
                4
            },
            //rlc  (HL)      CB 06       16 z00c rotate left
            RotateLeftHLThroughCarry {} => {
                let (val, carry) = rotate_left_carry(self.mem[self.read_reg_16(HL)]);
                self.mem[self.read_reg_16(HL)] = val;
                self.set_flags(Flags { z: val == 0, n: false, h: false, c: carry });
                4
            },
            //rrc  r         CB 0x        8 z00c rotate right
            RotateRightRegisterThroughCarry { r } => {
                let (val, carry) = rotate_right(self.reg[r]);
                self.reg[r] = val;
                self.set_flags(Flags { z: val == 0, n: false, h: false, c: carry });
                8
            },
            //rrc  (HL)      CB 0E       16 z00c rotate right through carry
            RotateRightHLThroughCarry {} => {
                let (val, carry) = rotate_right_carry(self.mem[self.read_reg_16(HL)]);
                self.mem[self.read_reg_16(HL)] = val;
                self.set_flags(Flags { z: val == 0, n: false, h: false, c: carry });
                8
            },
            //rl   r         CB 1x        8 z00c rotate left
            RotateLeftRegister { r } => {
                let (val, carry) = rotate_left(self.reg[r]);
                self.reg[r] = val;
                self.set_flags(Flags { z: val == 0, n: false, h: false, c: carry });
                4
            },
            //rl   (HL)      CB 16       16 z00c rotate left
            RotateLeftHL {} => {
                let (val, carry) = rotate_left(self.mem[self.read_reg_16(HL)]);
                self.mem[self.read_reg_16(HL)] = val;
                self.set_flags(Flags { z: val == 0, n: false, h: false, c: carry });
                16
            },
            //rr   r         CB 1x        8 z00c rotate right
            RotateRightRegister { r } => {
                let (val, carry) = rotate_right(self.reg[r]);
                self.reg[r] = val;
                self.set_flags(Flags { z: val == 0, n: false, h: false, c: carry });
                4
            },
            //rr   (HL)      CB 1E       16 z00c rotate right
            RotateRightHL {} => {
                let (val, carry) = rotate_right(self.mem[self.read_reg_16(HL)]);
                self.mem[self.read_reg_16(HL)] = val;
                self.set_flags(Flags { z: val == 0, n: false, h: false, c: carry });
                16
            },
            //sla  r         CB 2x        8 z00c shift left arithmetic (b0=0)
            ShiftLeftRegister { r } => {
                let carry = Cpu::ith_bit(self.reg[r], 7);
                self.reg[r] <<= 1;
                self.set_flags(Flags { z: self.reg[r] == 0, n: false, h: false, c: carry });
                8
            },
            //sla  (HL)      CB 26       16 z00c shift left arithmetic (b0=0)
            ShiftLeftHL {} => {
                let carry = Cpu::ith_bit(self.mem[self.read_reg_16(HL)], 7);
                self.mem[self.read_reg_16(HL)] <<= 1;
                self.set_flags(Flags { z: self.mem[self.read_reg_16(HL)] == 0, n: false, h: false, c: carry });
                8
            },
            //sra  r         CB 2x        8 z00c shift right arithmetic (b7=b7)
            ShiftRightArithmeticRegister { r } => {
                let carry = Cpu::ith_bit(self.reg[r], 0);
                self.reg[r] = (self.reg[r] >> 1) | ((carry as u8) << 7);
                self.set_flags(Flags { z: self.reg[r] == 0, n: false, h: false, c: carry });
                8
            },
            //sra  (HL)      CB 2E       16 z00c shift right arithmetic (b7=b7)
            ShiftRightArithmeticHL {} => {
                let a = self.read_reg_16(HL);
                let carry = Cpu::ith_bit(self.mem[a], 0);
                self.mem[a] = (self.mem[a] >> 1) | ((carry as u8) << 7);
                self.set_flags(Flags { z: self.mem[a] == 0, n: false, h: false, c: carry });
                16
            },
            //swap r         CB 3x        8 z000 exchange low/hi-nibble
            SwapRegister { r } => {
                self.reg[r] = (self.reg[r] << 4) | (self.reg[r] >> 4);
                self.set_flags(Flags { z: self.reg[r] == 0, n: false, h: false, c: false });
                8
            },
            //swap (HL)      CB 36       16 z000 exchange low/hi-nibble
            SwapHL {} => {
                let a = self.read_reg_16(HL);
                self.mem[a] = (self.mem[a] << 4) | (self.mem[a] >> 4);
                self.set_flags(Flags { z: self.mem[a] == 0, n: false, h: false, c: false });
                16
            },
            //srl  r         CB 3x        8 z00c shift right logical (b7=0)
            ShiftRightLogicalRegister { r } => {
                let carry = Cpu::ith_bit(self.reg[r], 0);
                self.reg[r] >>= 1;
                self.set_flags(Flags { z: self.reg[r] == 0, n: false, h: false, c: carry });
                8
            },
            //srl  (HL)      CB 3E       16 z00c shift right logical (b7=0)
            ShiftRightLogicalHL {} => {
                let a = self.read_reg_16(HL);
                let carry = Cpu::ith_bit(self.mem[a], 0);
                self.mem[a] >>= 1;
                self.set_flags(Flags { z: self.mem[a] == 0, n: false, h: false, c: carry });
                8
            },
            //GMB Singlebit Operation Commands
            //bit  n,r       CB xx        8 z01- test bit n
            TestBitRegister { bit, r } => {
                assert!(bit < 8, r < self.reg.len());
                self.set_flags(Flags { z: !Cpu::ith_bit(bit as u8, r), n: false, h: true, c: self.get_flag(Carry {}) });
                8
            },
            //bit  n,(HL)    CB xx       12 z01- test bit n
            TestBitMemoryHL { bit } => {
                assert!(bit < 8);
                self.set_flags(Flags { z: !Cpu::ith_bit(bit as u8, self.mem[self.read_reg_16(HL)] as usize), n: false, h: true, c: self.get_flag(Carry {}) });
                12
            },
            //set  n,r       CB xx        8 ---- set bit n
            SetBitRegister { bit, r } => {
                self.reg[r] |= 1 << bit as u8;
                8
            },
            //set  n,(HL)    CB xx       16 ---- set bit n
            SetBitMemoryHL { bit } => {
                self.mem[self.read_mem_16(HL)] |= 1 << bit as u8;
                16
            },
            //res  n,r       CB xx        8 ---- reset bit n
            ResetBitRegister { bit, r } => {
                self.reg[r] &= !(1 << bit as u8);
                8
            },
            //res  n,(HL)    CB xx       16 ---- reset bit n
            ResetBitMemoryHL { bit } => {
                self.mem[self.read_mem_16(HL)] &= !(1 << bit as u8);
                16
            },
            //GMB CPU-Control Commands
            //ccf            3F           4 -00c cy=cy xor 1
            CCF {} => {
                self.set_flags(Flags { z: self.get_flag(Zero {}), n: false, h: false, c: !self.get_flag(Carry {}) });
                4
            },
            //scf            37           4 -001 cy=1
            SCF {} => {
                self.set_flags(Flags { z: self.get_flag(Zero {}), n: false, h: false, c: true });
                4
            },
            //nop            00           4 ---- no operation
            NOP {} => { 4 },
            //halt           76         N*4 ---- halt until interrupt occurs (low power)
            HALT {} => {
                self.is_halted = true;
                4
            },
            //stop           10 00        ? ---- low power standby mode (VERY low power)
            STOP {} => {
                self.is_stopped = true;
                4
            },
            //di             F3           4 ---- disable interrupts, IME=0
            DI {} => {
                self.are_interrupts_enabled = false;
                4
            },
            //ei             FB           4 ---- enable interrupts, IME=1
            EI {} => {
                self.are_interrupts_enabled = true;
                4
            },
            //GMB Jump Commands
            //jp   nn        C3 nn nn    16 ---- jump to nn, PC=nn
            JP { nn } => {
                self.pc = nn;
                16
            },
            //jp   HL        E9           4 ---- jump to HL, PC=HL
            JPtoMemoryHL {} => {
                self.pc = self.read_reg_16(HL);
                4
            },
            //jp   f,nn      xx nn nn 16;12 ---- conditional jump if nz,z,nc,c
            CondJP { flag, nn } => {
                if (self.get_flag(flag)) {
                    self.pc = nn;
                    16
                } else {
                    12
                }
            },
            //jr   PC+dd     18 dd       12 ---- relative jump to nn (PC=PC+/-7bit)
            JRtoMemory { dd } => {
                self.pc = (self.pc as i64 + dd as i64) as usize;
                12
            },
            //jr   f,PC+dd   xx dd     12;8 ---- conditional relative jump if nz,z,nc,c
            CondJR { flag, dd } => {
                if self.get_flag(flag) {
                    self.pc = (self.pc as i64 + dd as i64) as usize;
                    12
                } else {
                    8
                }
            },
            //call nn        CD nn nn    24 ---- call to nn, SP=SP-2, (SP)=PC, PC=nn
            CALL { nn } => {
                self.sp -= 2;
                self.write_mem_16(self.sp, self.pc);
                self.pc = nn;
                24
            },
            //call f,nn      xx nn nn 24;12 ---- conditional call if nz,z,nc,c
            CondCALL { flag, nn } => {
                if self.get_flag(flag) {
                    self.sp -= 2;
                    self.write_mem_16(self.sp, self.pc);
                    self.pc = nn;
                    24
                } else {
                    12
                }
            },
            //ret            C9          16 ---- return, PC=(SP), SP=SP+2
            RET {} => {
                self.pc = self.read_mem_16(self.sp);
                self.sp += 2;
                16
            },
            //ret  f         xx        20;8 ---- conditional return if nz,z,nc,c
            CondRET { flag } => {
                if self.get_flag(flag) {
                    self.pc = self.read_mem_16(self.sp);
                    self.sp += 2;
                    20
                } else {
                    8
                }
            },
            //reti           D9          16 ---- return and enable interrupts (IME=1)
            RETI {} => {
                self.pc = self.read_mem_16(self.sp);
                self.sp += 2;
                self.are_interrupts_enabled = true;
                16
            },
            //rst  n         xx          16 ---- call to 00,08,10,18,20,28,30,38
            RST { n } => {
                self.sp -= 2;
                self.write_mem_16(self.sp, self.pc);
                self.pc = n;
                24
            },
        }
    }

    // (instruction, num cycles, pc increments)
    fn fetch_instruction(&self) -> (Instruction, usize) {
        let immediate_u8 = || self.mem[self.pc + 1];
        let immediate_i8 = || self.mem[self.pc + 1] as i8;
        let immediate_u16 = || ((self.mem[self.pc + 1] as u16) << 8) | self.mem[self.pc + 2] as u16;

        let reg_offset = |op| [B, C, D, E, H, L][(op & 0b00001111) as usize];

        let op = self.mem[self.pc];

        match op {
            // GMB 8bit-Load commands
            // ld   r,r         xx         4 ---- r=r
            // ld   r,(HL)      xx         8 ---- r=(HL)
            0x40..=0x45 => (LoadReg { to: B, from: reg_offset(op) }, 1),
            0x46 => (LoadRegWithMemoryHL { to: B }, 1),
            0x47 => (LoadReg { to: B, from: A }, 1),

            0x48..=0x4D => (LoadReg { to: C, from: reg_offset(op) }, 1),
            0x4E => (LoadRegWithMemoryHL { to: C }, 1),
            0x4F => (LoadReg { to: C, from: A }, 1),

            0x50..=0x55 => (LoadReg { to: D, from: reg_offset(op) }, 1),
            0x56 => (LoadRegWithMemoryHL { to: D }, 1),
            0x57 => (LoadReg { to: D, from: A }, 1),

            0x58..=0x5D => (LoadReg { to: E, from: reg_offset(op) }, 1),
            0x5E => (LoadRegWithMemoryHL { to: E }, 1),
            0x5F => (LoadReg { to: E, from: A }, 1),

            0x60..=0x65 => (LoadReg { to: H, from: reg_offset(op) }, 1),
            0x66 => (LoadRegWithMemoryHL { to: H }, 1),
            0x67 => (LoadReg { to: H, from: A }, 1),

            0x68..=0x6D => (LoadReg { to: L, from: reg_offset(op) }, 1),
            0x6E => (LoadRegWithMemoryHL { to: L }, 1),
            0x6F => (LoadReg { to: L, from: A }, 1),

            0x78..=0x7D => (LoadReg { to: A, from: reg_offset(op) }, 1),
            0x7E => (LoadRegWithMemoryHL { to: A }, 1),
            0x7F => (LoadReg { to: A, from: A }, 1),

            // ld   r,n         xx nn      8 ---- r=n
            0x06 => (LoadRegWithConstant { to: B, n: immediate_u8() }, 2),
            0x0E => (LoadRegWithConstant { to: C, n: immediate_u8() }, 2),
            0x16 => (LoadRegWithConstant { to: D, n: immediate_u8() }, 2),
            0x1E => (LoadRegWithConstant { to: E, n: immediate_u8() }, 2),
            0x26 => (LoadRegWithConstant { to: H, n: immediate_u8() }, 2),
            0x2E => (LoadRegWithConstant { to: L, n: immediate_u8() }, 2),
            0x3E => (LoadRegWithConstant { to: A, n: immediate_u8() }, 2),

            // ld   (HL),r      7x         8 ---- (HL)=r
            0x70 => (LoadMemoryHLwithRegister { from: B }, 1),
            0x71 => (LoadMemoryHLwithRegister { from: C }, 1),
            0x72 => (LoadMemoryHLwithRegister { from: D }, 1),
            0x73 => (LoadMemoryHLwithRegister { from: E }, 1),
            0x74 => (LoadMemoryHLwithRegister { from: H }, 1),
            0x75 => (LoadMemoryHLwithRegister { from: L }, 1),
            0x77 => (LoadMemoryHLwithRegister { from: A }, 1),

            // ld   (HL),n      36 nn     12 ----
            0x36 => (LoadMemoryHLwithConstant { n: immediate_u8() }, 2),
            // ld   A,(BC)      0A         8 ----
            0x0A => (LoadAwithValBC {}, 1),
            // ld   A,(DE)      1A         8 ----
            0x1A => (LoadAwithValDE {}, 1),
            // ld   A,(nn)      FA        16 ----
            0xFA => (LoadAwithMemory { nn: immediate_u16() as usize }, 3),
            // ld   (BC),A      02         8 ----
            0x02 => (LoadMemoryBCwithA {}, 1),
            // ld   (DE),A      12         8 ----
            0x12 => (LoadMemoryDEwithA {}, 1),
            // ld   (nn),A      EA        16 ----
            0xEA => (LoadMemoryNNwithA { nn: immediate_u16() as usize }, 3),
            // ld   A,(FF00+n)  F0 nn     12 ---- read from io-port n (memory FF00+n)
            0xF0 => (LoadAwithFF00plusN { nn: immediate_u16() as usize }, 3),
            // ld   (FF00+n),A  E0 nn     12 ---- write to io-port n (memory FF00+n)
            0xE0 => (LoadMemoryFF00plusNwithA { nn: immediate_u8() as usize }, 2),
            // ld   A,(FF00+C)  F2         8 ---- read from io-port C (memory FF00+C)
            0xF2 => (LoadAwithFF00plusC {}, 1),
            // ld   (FF00+C),A  E2         8 ---- write to io-port C (memory FF00+C)
            0xE2 => (LoadMemoryFF00plusCwithA {}, 1),
            // ldi  (HL),A      22         8 ---- (HL)=A, HL=HL+1
            0x22 => (LoadMemoryHLwithAandIncr {}, 1),
            // ldi  A,(HL)      2A         8 ---- A=(HL), HL=HL+1
            0x2A => (LoadAwithValHLandIncr {}, 1),
            // ldd  (HL),A      32         8 ---- (HL)=A, HL=HL-1
            0x32 => (LoadMemoryHLwithAandDecr {}, 1),
            // ldd  A,(HL)      3A         8 ---- A=(HL), HL=HL-1
            0x3A => (LoadAwithValHLandDecr {}, 1),

            // GMB 16bit-Load Commands
            // ld   rr,nn       x1 nn nn  12 ---- rr=nn (rr may be BC,DE,HL or SP)
            0x01 => (LoadRegWith16BitConstant { rr: BC, nn: immediate_u16() }, 3),
            0x11 => (LoadRegWith16BitConstant { rr: DE, nn: immediate_u16() }, 3),
            0x21 => (LoadRegWith16BitConstant { rr: HL, nn: immediate_u16() }, 3),
            0x31 => (LoadSPWith16BitConstant { nn: immediate_u16() }, 1),
            // ld   SP,HL       F9         8 ---- SP=HL
            0xF9 => (LoadSPwithHL {}, 1),
            // push rr          x5 (c5, d5, e5, f5)        16 ---- SP=SP-2  (SP)=rr   (rr may be BC,DE,HL,AF)
            0xc5 => (Push { rr: B }, 1),
            0xd5 => (Push { rr: D }, 1),
            0xe5 => (Push { rr: H }, 1),
            0xf5 => (Push { rr: A }, 1),
            // pop  rr          x1 (c1, d1, e1, f1)      12 (AF) rr=(SP)  SP=SP+2   (rr may be BC,DE,HL,AF)
            0xc1 => (Pop { rr: B }, 1),
            0xd1 => (Pop { rr: D }, 1),
            0xe1 => (Pop { rr: H }, 1),
            0xf1 => (Pop { rr: A }, 1),

            // GMB 8bit-Arithmetic/logical Commands
            //  add  A,r         8x         4 z0hc A=A+r
            0x80..=0x85 => (AddRegister { r: reg_offset(op) }, 1),
            0x86 => (AddMemoryHL {}, 1),
            0x87 => (AddRegister { r: A }, 1),
            //  add  A,n         C6 nn      8 z0hc A=A+n
            0xC6 => (AddConstant { n: immediate_u8() }, 2),
            //  add  A,(HL)      86         8 z0hc A=A+(HL)
            //  adc  A,r         8x         4 z0hc A=A+r+cy
            0x88..=0x8D => (AddRegisterWithCarry { r: reg_offset(op) }, 1),
            0x8E => (AddMemoryHLWithCarry {}, 1),
            0x8F => (AddRegisterWithCarry { r: A }, 1),
            //  adc  A,n         CE nn      8 z0hc A=A+n+cy
            0xCE => (AddConstantWithCarry { n: immediate_u8() }, 2),
            //  adc  A,(HL)      8E         8 z0hc A=A+(HL)+cy
            //  sub  r           9x         4 z1hc A=A-r
            0x90..=0x95 => (SubtractRegister { r: reg_offset(op) }, 1),
            0x96 => (SubtractMemoryHL {}, 1),
            0x97 => (SubtractRegister { r: A }, 1),
            //  sub  n           D6 nn      8 z1hc A=A-n
            0xD6 => (SubtractConstant { n: immediate_u8() }, 2),
            //  sub  (HL)        96         8 z1hc A=A-(HL)
            //  sbc  A,r         9x         4 z1hc A=A-r-cy
            0x98..=0x9D => (SubtractRegisterWithCarry { r: reg_offset(op) }, 1),
            0x9E => (SubtractMemoryHLWithCarry {}, 1),
            0x9F => (SubtractRegisterWithCarry { r: A }, 1),
            //  sbc  A,n         DE nn      8 z1hc A=A-n-cy
            0xDE => (SubtractConstantWithCarry { n: immediate_u8() }, 1),
            //  sbc  A,(HL)      9E         8 z1hc A=A-(HL)-cy
            //  and  r           Ax         4 z010 A=A & r
            0xA0..=0xA5 => (AndRegister { r: reg_offset(op) }, 1),
            0xA6 => (AndMemoryHL {}, 1),
            0xA7 => (AndRegister { r: A }, 1),
            //  and  n           E6 nn      8 z010 A=A & n
            0xE6 => (AndConstant { n: immediate_u8() }, 2),
            //  and  (HL)        A6         8 z010 A=A & (HL)
            //  xor  r           Ax         4 z000
            0xA8..=0xAD => (XorRegister { r: reg_offset(op) }, 1),
            0xAE => (XorMemoryHL {}, 1),
            0xAF => (XorRegister { r: A }, 1),
            //  xor  n           EE nn      8 z000
            0xEE => (XorConstant { n: immediate_u8() }, 2),
            //  xor  (HL)        AE         8 z000
            //  or   r           Bx         4 z000 A=A | r
            0xB0..=0xB5 => (OrRegister { r: reg_offset(op) }, 1),
            0xB6 => (OrMemoryHL {}, 1),
            0xB7 => (OrRegister { r: A }, 1),
            //  or   n           F6 nn      8 z000 A=A | n
            0xF6 => (OrConstant { n: immediate_u8() }, 2),
            //  or   (HL)        B6         8 z000 A=A | (HL)
            //  cp   r           Bx         4 z1hc compare A-r
            0xB8..=0xBD => (CompareRegister { r: reg_offset(op) }, 1),
            0xBE => (CompareMemoryHL {}, 1),
            0xBF => (CompareRegister { r: A }, 1),
            //  cp   n           FE nn      8 z1hc compare A-n
            0xFE => (CompareConstant { n: immediate_u8() }, 2),
            //  cp   (HL)        BE         8 z1hc compare A-(HL)
            //  inc  r           xx         4 z0h- r=r+1
            0x04 => (IncrementRegister { r: B }, 1),
            0x0c => (IncrementRegister { r: C }, 1),
            0x14 => (IncrementRegister { r: D }, 1),
            0x1c => (IncrementRegister { r: E }, 1),
            0x24 => (IncrementRegister { r: H }, 1),
            0x2c => (IncrementRegister { r: L }, 1),
            0x3c => (IncrementRegister { r: A }, 1),
            //  inc  (HL)        34        12 z0h- (HL)=(HL)+1
            0x34 => (IncrementMemoryHL {}, 1),
            //  dec  r           xx         4 z1h- r=r-1
            0x05 => (DecrementRegister { r: B }, 1),
            0x0d => (DecrementRegister { r: C }, 1),
            0x15 => (DecrementRegister { r: D }, 1),
            0x1d => (DecrementRegister { r: E }, 1),
            0x25 => (DecrementRegister { r: H }, 1),
            0x2d => (DecrementRegister { r: L }, 1),
            0x3d => (DecrementRegister { r: A }, 1),
            //  dec  (HL)        35        12 z1h- (HL)=(HL)-1
            0x35 => (DecrementMemoryHL {}, 1),
            //  daa              27         4 z-0x decimal adjust akku
            0x27 => (DecimalAdjust {}, 1),
            //  cpl              2F         4 -11- A = A xor FF
            0x2F => (CPL {}, 1),

            //GMB 16bit-Arithmetic/logical Commands
            //add  HL,rr     x9           8 -0hc HL = HL+rr     ;rr may be BC,DE,HL,SP
            0x09 => (AddRegisterHL16bit { r: B }, 1),
            0x19 => (AddRegisterHL16bit { r: D }, 1),
            0x29 => (AddRegisterHL16bit { r: H }, 1),
            0x39 => (AddRegisterSPtoHL16bit {}, 1),
            //inc  rr        x3           8 ---- rr = rr+1      ;rr may be BC,DE,HL,SP
            0x03 => (IncrementRegister16bit { r: B }, 1),
            0x13 => (IncrementRegister16bit { r: D }, 1),
            0x23 => (IncrementRegister16bit { r: H }, 1),
            0x33 => (IncrementSP {}, 1),
            //dec  rr        xB           8 ---- rr = rr-1      ;rr may be BC,DE,HL,SP
            0x0B => (DecrementRegister16bit { r: B }, 1),
            0x1B => (DecrementRegister16bit { r: D }, 1),
            0x2B => (DecrementRegister16bit { r: H }, 1),
            0x3B => (DecrementSP {}, 1),
            //add  SP,dd     E8          16 00hc SP = SP +/- dd ;dd is 8bit signed number
            0xE8 => (AddSP { d: immediate_i8() }, 2),
            //ld   HL,SP+dd  F8          12 00hc HL = SP +/- dd ;dd is 8bit signed number
            0xF8 => (LoadHLwithSPplus { d: immediate_i8() }, 2),

            //GMB Rotate- und Shift-Commands
            //rlca           07           4 000c rotate akku left
            0x07 => (RotateLeft {}, 1),
            //rla            17           4 000c rotate akku left through carry
            0x17 => (RotateLeftThroughCarry {}, 1),
            //rrca           0F           4 000c rotate akku right
            0x0F => (RotateRight {}, 1),
            //rra            1F           4 000c rotate akku right through carry
            0x1F => (RotateRightThroughCarry {}, 1),
            0xCB => {
                let op = self.mem[self.pc + 1];
                match op {
                    //rlc  r         CB 0x        8 z00c rotate left
                    //rlc  (HL)      CB 06       16 z00c rotate left
                    0x00..=0x05 => (RotateLeftRegisterThroughCarry { r: reg_offset(op) }, 2),
                    0x06 => (RotateLeftHLThroughCarry {}, 2),
                    0x07 => (RotateLeftRegisterThroughCarry { r: A }, 2),
                    //rrc  r         CB 0x        8 z00c rotate right
                    //rrc  (HL)      CB 0E       16 z00c rotate right
                    0x08..=0x0D => (RotateRightRegisterThroughCarry { r: reg_offset(op) }, 2),
                    0x0E => (RotateRightHLThroughCarry {}, 2),
                    0x0F => (RotateRightRegisterThroughCarry { r: A }, 2),
                    //rl   r         CB 1x        8 z00c rotate left through carry
                    //rl   (HL)      CB 16       16 z00c rotate left through carry
                    0x10..=0x15 => (RotateLeftRegister { r: reg_offset(op) }, 2),
                    0x16 => (RotateLeftHL {}, 2),
                    0x17 => (RotateLeftRegister { r: A }, 2),
                    //rr   r         CB 1x        8 z00c rotate right through carry
                    //rr   (HL)      CB 1E       16 z00c rotate right through carry
                    0x18..=0x1D => (RotateRightRegister { r: reg_offset(op) }, 2),
                    0x1E => (RotateRightHL {}, 2),
                    0x1F => (RotateRightRegister { r: A }, 2),
                    //sla  r         CB 2x        8 z00c shift left arithmetic (b0=0)
                    //sla  (HL)      CB 26       16 z00c shift left arithmetic (b0=0)
                    0x20..=0x25 => (ShiftLeftRegister { r: reg_offset(op) }, 2),
                    0x26 => (ShiftLeftHL {}, 2),
                    0x27 => (ShiftLeftRegister { r: A }, 2),
                    //sra  r         CB 2x        8 z00c shift right arithmetic (b7=b7)
                    //sra  (HL)      CB 2E       16 z00c shift right arithmetic (b7=b7)
                    0x28..=0x2D => (ShiftRightArithmeticRegister { r: reg_offset(op) }, 2),
                    0x2E => (ShiftRightArithmeticHL {}, 2),
                    0x2F => (ShiftRightArithmeticRegister { r: A }, 2),
                    //swap r         CB 3x        8 z000 exchange low/hi-nibble
                    //swap (HL)      CB 36       16 z000 exchange low/hi-nibble
                    0x30..=0x35 => (SwapRegister { r: reg_offset(op) }, 2),
                    0x36 => (SwapHL {}, 2),
                    0x37 => (SwapRegister { r: A }, 2),
                    //srl  r         CB 3x        8 z00c shift right logical (b7=0)
                    //srl  (HL)      CB 3E       16 z00c shift right logical (b7=0)
                    0x38..=0x3D => (ShiftRightLogicalRegister { r: reg_offset(op) }, 2),
                    0x3E => (ShiftRightLogicalHL {}, 2),
                    0x3F => (ShiftRightLogicalRegister { r: A }, 2),

                    //GMB Singlebit Operation Commands
                    //bit  n,r       CB xx        8 z01- test bit n
                    //bit  n,(HL)    CB xx       12 z01- test bit n
                    0x40..=0x45 => (TestBitRegister { bit: 0, r: reg_offset(op) }, 2),
                    0x46 => (TestBitMemoryHL { bit: 0 }, 2),
                    0x47 => (TestBitRegister { bit: 0, r: A }, 2),

                    0x48..=0x4D => (TestBitRegister { bit: 1, r: reg_offset(op) }, 2),
                    0x4E => (TestBitMemoryHL { bit: 1 }, 2),
                    0x4F => (TestBitRegister { bit: 1, r: A }, 2),

                    0x50..=0x55 => (TestBitRegister { bit: 2, r: reg_offset(op) }, 2),
                    0x56 => (TestBitMemoryHL { bit: 2 }, 2),
                    0x57 => (TestBitRegister { bit: 2, r: A }, 2),

                    0x58..=0x5D => (TestBitRegister { bit: 3, r: reg_offset(op) }, 2),
                    0x5E => (TestBitMemoryHL { bit: 3 }, 2),
                    0x5F => (TestBitRegister { bit: 3, r: A }, 2),

                    0x60..=0x65 => (TestBitRegister { bit: 4, r: reg_offset(op) }, 2),
                    0x66 => (TestBitMemoryHL { bit: 4 }, 2),
                    0x67 => (TestBitRegister { bit: 4, r: A }, 2),

                    0x68..=0x6D => (TestBitRegister { bit: 5, r: reg_offset(op) }, 2),
                    0x6E => (TestBitMemoryHL { bit: 5 }, 2),
                    0x6F => (TestBitRegister { bit: 5, r: A }, 2),

                    0x70..=0x75 => (TestBitRegister { bit: 6, r: reg_offset(op) }, 2),
                    0x76 => (TestBitMemoryHL { bit: 6 }, 2),
                    0x77 => (TestBitRegister { bit: 6, r: A }, 2),

                    0x78..=0x7D => (TestBitRegister { bit: 7, r: reg_offset(op) }, 2),
                    0x7E => (TestBitMemoryHL { bit: 7 }, 2),
                    0x7F => (TestBitRegister { bit: 7, r: A }, 2),

                    //res  n,r       CB xx        8 ---- reset bit n
                    //res  n,(HL)    CB xx       16 ---- reset bit n
                    0x80..=0x85 => (ResetBitRegister { bit: 0, r: reg_offset(op) }, 2),
                    0x86 => (ResetBitMemoryHL { bit: 0 }, 2),
                    0x87 => (ResetBitRegister { bit: 0, r: A }, 2),

                    0x88..=0x8D => (ResetBitRegister { bit: 1, r: reg_offset(op) }, 2),
                    0x8E => (ResetBitMemoryHL { bit: 1 }, 2),
                    0x8F => (ResetBitRegister { bit: 1, r: A }, 2),

                    0x90..=0x95 => (ResetBitRegister { bit: 2, r: reg_offset(op) }, 2),
                    0x96 => (ResetBitMemoryHL { bit: 2 }, 2),
                    0x97 => (ResetBitRegister { bit: 2, r: A }, 2),

                    0x98..=0x9D => (ResetBitRegister { bit: 3, r: reg_offset(op) }, 2),
                    0x9E => (ResetBitMemoryHL { bit: 3 }, 2),
                    0x9F => (ResetBitRegister { bit: 3, r: A }, 2),

                    0xA0..=0xA5 => (ResetBitRegister { bit: 4, r: reg_offset(op) }, 2),
                    0xA6 => (ResetBitMemoryHL { bit: 4 }, 2),
                    0xA7 => (ResetBitRegister { bit: 4, r: A }, 2),

                    0xA8..=0xAD => (ResetBitRegister { bit: 5, r: reg_offset(op) }, 2),
                    0xAE => (ResetBitMemoryHL { bit: 5 }, 2),
                    0xAF => (ResetBitRegister { bit: 5, r: A }, 2),

                    0xB0..=0xB5 => (ResetBitRegister { bit: 6, r: reg_offset(op) }, 2),
                    0xB6 => (ResetBitMemoryHL { bit: 6 }, 2),
                    0xB7 => (ResetBitRegister { bit: 6, r: A }, 2),

                    0xB8..=0xBD => (ResetBitRegister { bit: 7, r: reg_offset(op) }, 2),
                    0xBE => (ResetBitMemoryHL { bit: 7 }, 2),
                    0xBF => (ResetBitRegister { bit: 7, r: A }, 2),

                    //set  n,r       CB xx        8 ---- set bit n
                    //set  n,(HL)    CB xx       16 ---- set bit n
                    0xC0..=0xC5 => (SetBitRegister { bit: 0, r: reg_offset(op) }, 2),
                    0xC6 => (SetBitMemoryHL { bit: 0 }, 2),
                    0xC7 => (SetBitRegister { bit: 0, r: A }, 2),

                    0xC8..=0xCD => (SetBitRegister { bit: 1, r: reg_offset(op) }, 2),
                    0xCE => (SetBitMemoryHL { bit: 1 }, 2),
                    0xCF => (SetBitRegister { bit: 1, r: A }, 2),

                    0xD0..=0xD5 => (SetBitRegister { bit: 2, r: reg_offset(op) }, 2),
                    0xD6 => (SetBitMemoryHL { bit: 2 }, 2),
                    0xD7 => (SetBitRegister { bit: 2, r: A }, 2),

                    0xD8..=0xDD => (SetBitRegister { bit: 3, r: reg_offset(op) }, 2),
                    0xDE => (SetBitMemoryHL { bit: 3 }, 2),
                    0xDF => (SetBitRegister { bit: 3, r: A }, 2),

                    0xE0..=0xE5 => (SetBitRegister { bit: 4, r: reg_offset(op) }, 2),
                    0xE6 => (SetBitMemoryHL { bit: 4 }, 2),
                    0xE7 => (SetBitRegister { bit: 4, r: A }, 2),

                    0xE8..=0xED => (SetBitRegister { bit: 5, r: reg_offset(op) }, 2),
                    0xEE => (SetBitMemoryHL { bit: 5 }, 2),
                    0xEF => (SetBitRegister { bit: 5, r: A }, 2),

                    0xF0..=0xF5 => (SetBitRegister { bit: 6, r: reg_offset(op) }, 2),
                    0xF6 => (SetBitMemoryHL { bit: 6 }, 2),
                    0xF7 => (SetBitRegister { bit: 6, r: A }, 2),

                    0xF8..=0xFD => (SetBitRegister { bit: 7, r: reg_offset(op) }, 2),
                    0xFE => (SetBitMemoryHL { bit: 7 }, 2),
                    0xFF => (SetBitRegister { bit: 7, r: A }, 2),
                    _ => unreachable!()
                }
            },
            //GMB CPU-Control Commands
            //ccf            3F           4 -00c cy=cy xor 1
            0x3F => (CCF {}, 1),
            //scf            37           4 -001 cy=1
            0x37 => (SCF {}, 1),
            //nop            00           4 ---- no operation
            0x00 => (NOP {}, 1),
            //halt           76         N*4 ---- halt until interrupt occurs (low power)
            0x76 => (HALT {}, 1),
            //stop           10 00        ? ---- low power standby mode (VERY low power)
            0x10 => (STOP {}, 2),
            //di             F3           4 ---- disable interrupts, IME=0
            0xF3 => (DI {}, 1),
            //ei             FB           4 ---- enable interrupts, IME=1
            0xFB => (EI {}, 1),

            //GMB Jump Commands
            //jp   nn        C3 nn nn    16 ---- jump to nn, PC=nn
            0xC3 => (JP { nn: immediate_u16() as usize }, 3),
            //jp   HL        E9           4 ---- jump to HL, PC=HL
            0xE9 => (JPtoMemoryHL {}, 1),
            //jp   f,nn      xx nn nn 16;12 ---- conditional jump if nz,z,nc,c
            0xC2 => (CondJP { flag: Subtract {}, nn: immediate_u16() as usize }, 3),
            0xCA => (CondJP { flag: Zero {}, nn: immediate_u16() as usize }, 3),
            0xD2 => (CondJP { flag: HalfCarry {}, nn: immediate_u16() as usize }, 3),
            0xDA => (CondJP { flag: Carry {}, nn: immediate_u16() as usize }, 3),
            //jr   PC+dd     18 dd       12 ---- relative jump to nn (PC=PC+/-7bit)
            0x18 => (JRtoMemory { dd: immediate_i8() }, 2),
            //jr   f,PC+dd   xx dd     12;8 ---- conditional relative jump if nz,z,nc,c
            // TODO: Duty cycles depend on conditional too!
            0x20 => (CondJR { flag: Subtract {}, dd: immediate_i8() }, 2),
            0x28 => (CondJR { flag: Zero {}, dd: immediate_i8() }, 2),
            0x30 => (CondJR { flag: HalfCarry {}, dd: immediate_i8() }, 2),
            0x38 => (CondJR { flag: Carry {}, dd: immediate_i8() }, 2),
            //call nn        CD nn nn    24 ---- call to nn, SP=SP-2, (SP)=PC, PC=nn
            0xCD => (CALL { nn: immediate_u16() as usize }, 1),
            //call f,nn      xx nn nn 24;12 ---- conditional call if nz,z,nc,c
            0xC4 => (CondCALL { flag: Subtract {}, nn: immediate_u16() as usize }, 3),
            0xCC => (CondCALL { flag: Zero {}, nn: immediate_u16() as usize }, 3),
            0xD4 => (CondCALL { flag: HalfCarry {}, nn: immediate_u16() as usize }, 3),
            0xDC => (CondCALL { flag: Carry {}, nn: immediate_u16() as usize }, 3),
            //ret            C9          16 ---- return, PC=(SP), SP=SP+2
            0xC9 => (RET {}, 1),
            //ret  f         xx        20;8 ---- conditional return if nz,z,nc,c
            0xC0 => (CondRET { flag: Subtract {} }, 3),
            0xC8 => (CondRET { flag: Zero {} }, 3),
            0xD0 => (CondRET { flag: HalfCarry {} }, 3),
            0xD8 => (CondRET { flag: Carry {} }, 3),
            //reti           D9          16 ---- return and enable interrupts (IME=1)
            0xD9 => (RETI {}, 1),
            //rst  n         xx          16 ---- call to 00,08,10,18,20,28,30,38
            0xC7 => (RST { n: 0x00 }, 1),
            0xCF => (RST { n: 0x08 }, 1),
            0xD7 => (RST { n: 0x10 }, 1),
            0xDF => (RST { n: 0x18 }, 1),
            0xE7 => (RST { n: 0x20 }, 1),
            0xEF => (RST { n: 0x28 }, 1),
            0xF7 => (RST { n: 0x30 }, 1),
            0xFF => (RST { n: 0x38 }, 1),
            _ => unreachable!(),
        }
    }
}

fn main() {
//    let file_name = env::args().nth(1).expect("Must give game name as first file");
//    let mut file = File::open(file_name).expect("There was an issue opening the file");
//    let mut buffer = Vec::new();
//    let _bytes_read = file.read_to_end(&mut buffer);
//
//    let mut cpu = cpu::Cpu::new(buffer.as_slice());
//
//    const GUI_SCALE: f64 = 10.0;
//    let window_dimensions = [cpu::VIDEO_WIDTH * GUI_SCALE as u32, cpu::VIDEO_HEIGHT * GUI_SCALE as u32];
//    let mut window: PistonWindow = WindowSettings::new("Chip-8 Interpreter", window_dimensions)
//        .exit_on_esc(true)
//        .build()
//        .expect("Failed to create a piston window");

//    while let Some(e) = window.next() {
//        if let Some(_) = e.render_args() {
//            window.draw_2d(&e, |context, graphics, _| {
//                piston_window::clear(color::BLACK, graphics);
//
//                for (i, row) in cpu.video.chunks(cpu::VIDEO_WIDTH as usize).enumerate() {
//                    for (j, &val) in row.iter().enumerate() {
//                        if val > 0 {
//                            let d = [j as f64 * GUI_SCALE, i as f64 * GUI_SCALE, GUI_SCALE, GUI_SCALE];
//                            Rectangle::new(color::WHITE)
//                                .draw(d, &context.draw_state, context.transform, graphics);
//                        }
//                    }
//                }
//            });
//        }
//
//        if let Some(u) = e.update_args() {
//            cpu.cycle(u.dt);
//        }
//
//        if let Some(Button::Keyboard(key)) = e.release_args() {
//            cpu.handle_key(key.code(), false);
//        }
//
//        if let Some(Button::Keyboard(key)) = e.press_args() {
//            cpu.handle_key(key.code(), true);
//        }
//    }
}
