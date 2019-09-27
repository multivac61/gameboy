use crate::cpu::{Register, Register16bit, MemoryAddress, ConditionalFlag};

#[derive(Debug)]
pub enum Instruction {
    // n: unsigned 8-bit immediate data
    // nn: unsigned 16-bit immediate data
    // e: signed 8-bit immediate data
    // r: signed 8-bit immediate data, relative to PC

    // GMB 8bit-Load commands
    // ld   r,r         xx         4 ---- r=r
    LoadReg { to: Register, from: Register },
    // ld   r,n         xx nn      8 ---- r=n
    LoadRegWithConstant { to: Register, n: u8 },
    // ld   r,(HL)      xx         8 ---- r=(HL)
    LoadRegWithMemoryHL { to: Register },
    // ld   (HL),r      7x         8 ---- (HL)=r
    LoadMemoryHLwithRegister { from: Register },
    // ld   (HL),n      36 nn     12 ----
    LoadMemoryHLwithConstant { n: u8 },
    // ld   A,(BC)      0A         8 ----
    LoadAwithValBC,
    // ld   A,(DE)      1A         8 ----
    LoadAwithValDE,
    // ld   A,(nn)      FA        16 ----
    LoadAwithMemory { nn: MemoryAddress },
    // ld   (BC),A      02         8 ----
    LoadMemoryBCwithA,
    // ld   (DE),A      12         8 ----
    LoadMemoryDEwithA,
    // ld   (nn),A      EA        16 ----
    LoadMemoryNNwithA { nn: MemoryAddress },
    // ld   A,(FF00+n)  F0 nn     12 ---- read from io-port n (memory FF00+n)
    LoadAwithFF00plusN { n: u8 },
    // ld   (FF00+n),A  E0 nn     12 ---- write to io-port n (memory FF00+n)
    LoadMemoryFF00plusNwithA { nn: u8 },
    // ld   A,(FF00+C)  F2         8 ---- read from io-port C (memory FF00+C)
    LoadAwithFF00plusC,
    // ld   (FF00+C),A  E2         8 ---- write to io-port C (memory FF00+C)
    LoadMemoryFF00plusCwithA,
    // ldi  (HL),A      22         8 ---- (HL)=A, HL=HL+1
    LoadMemoryHLwithAandIncr,
    // ldi  A,(HL)      2A         8 ---- A=(HL), HL=HL+1
    LoadAwithValHLandIncr,

    // ldd  (HL),A      32         8 ---- (HL)=A, HL=HL-1
    LoadMemoryHLwithAandDecr,
    // ldd  A,(HL)      3A         8 ---- A=(HL), HL=HL-1
    LoadAwithValHLandDecr,

    // GMB 16bit-Load Commands
    // ld   rr,nn       x1 nn nn  12 ---- rr=nn (rr may be BC,DE,HL or SP)
    LoadRegWith16BitConstant { rr: Register16bit, nn: u16 },
    LoadSPWith16BitConstant { nn: u16 },
    // ld   SP,HL       F9         8 ---- SP=HL
    LoadSPwithHL,
    // push rr          x5 (c5, d5, e5, f5)        16 ---- SP=SP-2  (SP)=rr   (rr may be BC,DE,HL,AF)
    Push { rr: Register16bit },
    // pop  rr          x1 (c1, d1, e1, f1)      12 (AF) rr=(SP)  SP=SP+2   (rr may be BC,DE,HL,AF)
    Pop { rr: Register16bit },

    // GMB 8bit-Arithmetic/logical Commands
    //  add  A,r         8x         4 z0hc A=A+r
    AddRegister { r: Register },
    //  add  A,n         C6 nn      8 z0hc A=A+n
    AddConstant { n: u8 },
    //  add  A,(HL)      86         8 z0hc A=A+(HL)
    AddMemoryHL,
    //  adc  A,r         8x         4 z0hc A=A+r+cy
    AddRegisterWithCarry { r: Register },
    //  adc  A,n         CE nn      8 z0hc A=A+n+cy
    AddConstantWithCarry { n: u8 },
    //  adc  A,(HL)      8E         8 z0hc A=A+(HL)+cy
    AddMemoryHLWithCarry,
    //  sub  r           9x         4 z1hc A=A-r
    SubtractRegister { r: Register },
    //  sub  n           D6 nn      8 z1hc A=A-n
    SubtractConstant { n: u8 },
    //  sub  (HL)        96         8 z1hc A=A-(HL)
    SubtractMemoryHL,
    //  sbc  A,r         9x         4 z1hc A=A-r-cy
    SubtractRegisterWithCarry { r: Register },
    //  sbc  A,n         DE nn      8 z1hc A=A-n-cy
    SubtractConstantWithCarry { n: u8 },
    //  sbc  A,(HL)      9E         8 z1hc A=A-(HL)-cy
    SubtractMemoryHLWithCarry,
    //  and  r           Ax         4 z010 A=A & r
    AndRegister { r: Register },
    //  and  n           E6 nn      8 z010 A=A & n
    AndConstant { n: u8 },
    //  and  (HL)        A6         8 z010 A=A & (HL)
    AndMemoryHL,
    //  xor  r           Ax         4 z000
    XorRegister { r: Register },
    //  xor  n           EE nn      8 z000
    XorConstant { n: u8 },
    //  xor  (HL)        AE         8 z000
    XorMemoryHL,
    //  or   r           Bx         4 z000 A=A | r
    OrRegister { r: Register },
    //  or   n           F6 nn      8 z000 A=A | n
    OrConstant { n: u8 },
    //  or   (HL)        B6         8 z000 A=A | (HL)
    OrMemoryHL,
    //  cp   r           Bx         4 z1hc compare A-r
    CompareRegister { r: Register },
    //  cp   n           FE nn      8 z1hc compare A-n
    CompareConstant { n: u8 },
    //  cp   (HL)        BE         8 z1hc compare A-(HL)
    CompareMemoryHL,
    //  inc  r           xx         4 z0h- r=r+1
    IncrementRegister { r: Register },
    //  inc  (HL)        34        12 z0h- (HL)=(HL)+1
    IncrementMemoryHL,
    //  dec  r           xx         4 z1h- r=r-1
    DecrementRegister { r: Register },
    //  dec  (HL)        35        12 z1h- (HL)=(HL)-1
    DecrementMemoryHL,
    //  daa              27         4 z-0x decimal adjust akku
    DecimalAdjust,
    //  cpl              2F         4 -11- A = A xor FF
    CPL,

    //GMB 16bit-Arithmetic/logical Commands
    //add  HL,rr     x9           8 -0hc HL = HL+rr     ;rr may be BC,DE,HL,SP
    AddRegisterHL16bit { r: Register16bit },
    AddRegisterSPtoHL16bit,
    //inc  rr        x3           8 ---- rr = rr+1      ;rr may be BC,DE,HL,SP
    IncrementRegister16bit { r: Register16bit },
    IncrementSP,
    //dec  rr        xB           8 ---- rr = rr-1      ;rr may be BC,DE,HL,SP
    DecrementRegister16bit { r: Register16bit },
    DecrementSP,
    //add  SP,dd     E8          16 00hc SP = SP +/- dd ;dd is 8bit signed number
    AddSP { d: i8 },
    //ld   HL,SP+dd  F8          12 00hc HL = SP +/- dd ;dd is 8bit signed number
    LoadHLwithSPplus { d: i8 },
    LoadAddressWithSP { n: u16 },

    //GMB Rotate- und Shift-Commands
    //rlca           07           4 000c rotate akku left
    RotateLeft,
    //rla            17           4 000c rotate akku left through carry
    RotateLeftThroughCarry,
    //rrca           0F           4 000c rotate akku right
    RotateRight,
    //rra            1F           4 000c rotate akku right through carry
    RotateRightThroughCarry,
    //rlc  r         CB 0x        8 z00c rotate left
    RotateLeftRegisterThroughCarry { r: Register },
    //rlc  (HL)      CB 06       16 z00c rotate left
    RotateLeftHLThroughCarry,
    //rrc  r         CB 0x        8 z00c rotate right
    RotateRightRegisterThroughCarry { r: Register },
    //rrc  (HL)      CB 0E       16 z00c rotate right
    RotateRightHLThroughCarry,
    //rl   r         CB 1x        8 z00c rotate left through carry
    RotateLeftRegister { r: Register },
    //rl   (HL)      CB 16       16 z00c rotate left through carry
    RotateLeftHL,
    //rr   r         CB 1x        8 z00c rotate right through carry
    RotateRightRegister { r: Register },
    //rr   (HL)      CB 1E       16 z00c rotate right through carry
    RotateRightHL,
    //sla  r         CB 2x        8 z00c shift left arithmetic (b0=0)
    ShiftLeftRegister { r: Register },
    //sla  (HL)      CB 26       16 z00c shift left arithmetic (b0=0)
    ShiftLeftHL,
    //sra  r         CB 2x        8 z00c shift right arithmetic (b7=b7)
    ShiftRightArithmeticRegister { r: Register },
    //sra  (HL)      CB 2E       16 z00c shift right arithmetic (b7=b7)
    ShiftRightArithmeticHL,
    //swap r         CB 3x        8 z000 exchange low/hi-nibble
    SwapRegister { r: Register },
    //swap (HL)      CB 36       16 z000 exchange low/hi-nibble
    SwapHL,
    //srl  r         CB 3x        8 z00c shift right logical (b7=0)
    ShiftRightLogicalRegister { r: Register },
    //srl  (HL)      CB 3E       16 z00c shift right logical (b7=0)
    ShiftRightLogicalHL,

    //GMB Singlebit Operation Commands
    //bit  n,r       CB xx        8 z01- test bit n
    TestBitRegister { bit: u8, r: Register },
    //bit  n,(HL)    CB xx       12 z01- test bit n
    TestBitMemoryHL { bit: u8 },
    //set  n,r       CB xx        8 ---- set bit n
    SetBitRegister { bit: u8, r: Register },
    //set  n,(HL)    CB xx       16 ---- set bit n
    SetBitMemoryHL { bit: u8 },
    //res  n,r       CB xx        8 ---- reset bit n
    ResetBitRegister { bit: u8, r: Register },
    //res  n,(HL)    CB xx       16 ---- reset bit n
    ResetBitMemoryHL { bit: u8 },

    //GMB CPU-Control Commands
    //ccf            3F           4 -00c cy=cy xor 1
    CCF,
    //scf            37           4 -001 cy=1
    SCF,
    //nop            00           4 ---- no operation
    NOP,
    //halt           76         N*4 ---- halt until interrupt occurs (low power)
    HALT,
    //stop           10 00        ? ---- low power standby mode (VERY low power)
    STOP,
    //di             F3           4 ---- disable interrupts, IME=0
    DI,
    //ei             FB           4 ---- enable interrupts, IME=1
    EI,

    //GMB Jump Commands
    //jp   nn        C3 nn nn    16 ---- jump to nn, PC=nn
    JP { nn: u16 },
    //jp   HL        E9           4 ---- jump to HL, PC=HL
    JPtoMemoryHL,
    //jp   f,nn      xx nn nn 16;12 ---- conditional jump if nz,z,nc,c
    CondJP { flag: ConditionalFlag, check_state: bool, nn: u16 },
    //jr   PC+dd     18 dd       12 ---- relative jump to nn (PC=PC+/-7bit)
    JRtoMemory { dd: i8 },
    //jr   f,PC+dd   xx dd     12;8 ---- conditional relative jump if nz,z,nc,c
    CondJR { flag: ConditionalFlag, check_state: bool, dd: i8 },
    //call nn        CD nn nn    24 ---- call to nn, SP=SP-2, (SP)=PC, PC=nn
    CALL { nn: u16 },
    //call f,nn      xx nn nn 24;12 ---- conditional call if nz,z,nc,c
    CondCALL { flag: ConditionalFlag, check_state: bool, nn: u16 },
    //ret            C9          16 ---- return, PC=(SP), SP=SP+2
    RET,
    //ret  f         xx        20;8 ---- conditional return if nz,z,nc,c
    CondRET { flag: ConditionalFlag, check_state: bool },
    //reti           D9          16 ---- return and enable interrupts (IME=1)
    RETI,
    //rst  n         xx          16 ---- call to 00,08,10,18,20,28,30,38
    RST { n: u16 },
}
