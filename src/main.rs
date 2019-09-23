use crate::ConditionalFlag::*;
use crate::Instruction::*;

type MemoryAddress = u16;

#[derive(Clone, Copy)]
enum Register {
    A {},
    F {},
    B {},
    C {},
    D {},
    E {},
    H {},
    L {},
}

#[derive(Clone, Copy)]
enum Register16bit {
    AF {},
    BC {},
    DE {},
    HL {},
}

impl std::convert::From<Register> for usize {
    fn from(bank: Register) -> Self {
        match bank {
            Register::A {} => 0,
            Register::F {} => 1,
            Register::B {} => 2,
            Register::C {} => 3,
            Register::D {} => 4,
            Register::E {} => 5,
            Register::H {} => 6,
            Register::L {} => 7,
        }
    }
}

impl std::convert::From<usize> for Register {
    fn from(bank: usize) -> Self {
        match bank {
            0 => Register::A {},
            1 => Register::F {},
            2 => Register::B {},
            3 => Register::C {},
            4 => Register::D {},
            5 => Register::E {},
            6 => Register::H {},
            7 => Register::L {},
            _ => unreachable!(),
        }
    }
}

impl std::convert::From<Register16bit> for usize {
    fn from(bank: Register16bit) -> Self {
        match bank {
            Register16bit::AF {} => 0,
            Register16bit::BC {} => 2,
            Register16bit::DE {} => 4,
            Register16bit::HL {} => 6,
        }
    }
}

impl std::convert::From<usize> for Register16bit {
    fn from(bank: usize) -> Self {
        match bank {
            0 => Register16bit::AF {},
            2 => Register16bit::BC {},
            4 => Register16bit::DE {},
            6 => Register16bit::HL {},
            _ => unreachable!(),
        }
    }
}

enum ConditionalFlag {
    Zero {},
    Subtract {},
    HalfCarry {},
    Carry {},
}

impl std::convert::From<ConditionalFlag> for usize {
    fn from(bank: ConditionalFlag) -> Self {
        match bank {
            ConditionalFlag::Zero {} => 7,
            ConditionalFlag::Subtract {} => 6,
            ConditionalFlag::HalfCarry {} => 5,
            ConditionalFlag::Carry {} => 4,
        }
    }
}

enum Instruction {
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
    LoadAwithValBC {},
    // ld   A,(DE)      1A         8 ----
    LoadAwithValDE {},
    // ld   A,(nn)      FA        16 ----
    LoadAwithMemory { nn: MemoryAddress },
    // ld   (BC),A      02         8 ----
    LoadMemoryBCwithA {},
    // ld   (DE),A      12         8 ----
    LoadMemoryDEwithA {},
    // ld   (nn),A      EA        16 ----
    LoadMemoryNNwithA { nn: MemoryAddress },
    // ld   A,(FF00+n)  F0 nn     12 ---- read from io-port n (memory FF00+n)
    LoadAwithFF00plusN { nn: u8 },
    // ld   (FF00+n),A  E0 nn     12 ---- write to io-port n (memory FF00+n)
    LoadMemoryFF00plusNwithA { nn: u8 },
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
    LoadRegWith16BitConstant { rr: Register16bit, nn: u16 },
    LoadSPWith16BitConstant { nn: u16 },
    // ld   SP,HL       F9         8 ---- SP=HL
    LoadSPwithHL {},
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
    AddMemoryHL {},
    //  adc  A,r         8x         4 z0hc A=A+r+cy
    AddRegisterWithCarry { r: Register },
    //  adc  A,n         CE nn      8 z0hc A=A+n+cy
    AddConstantWithCarry { n: u8 },
    //  adc  A,(HL)      8E         8 z0hc A=A+(HL)+cy
    AddMemoryHLWithCarry {},
    //  sub  r           9x         4 z1hc A=A-r
    SubtractRegister { r: Register },
    //  sub  n           D6 nn      8 z1hc A=A-n
    SubtractConstant { n: u8 },
    //  sub  (HL)        96         8 z1hc A=A-(HL)
    SubtractMemoryHL {},
    //  sbc  A,r         9x         4 z1hc A=A-r-cy
    SubtractRegisterWithCarry { r: Register },
    //  sbc  A,n         DE nn      8 z1hc A=A-n-cy
    SubtractConstantWithCarry { n: u8 },
    //  sbc  A,(HL)      9E         8 z1hc A=A-(HL)-cy
    SubtractMemoryHLWithCarry {},
    //  and  r           Ax         4 z010 A=A & r
    AndRegister { r: Register },
    //  and  n           E6 nn      8 z010 A=A & n
    AndConstant { n: u8 },
    //  and  (HL)        A6         8 z010 A=A & (HL)
    AndMemoryHL {},
    //  xor  r           Ax         4 z000
    XorRegister { r: Register },
    //  xor  n           EE nn      8 z000
    XorConstant { n: u8 },
    //  xor  (HL)        AE         8 z000
    XorMemoryHL {},
    //  or   r           Bx         4 z000 A=A | r
    OrRegister { r: Register },
    //  or   n           F6 nn      8 z000 A=A | n
    OrConstant { n: u8 },
    //  or   (HL)        B6         8 z000 A=A | (HL)
    OrMemoryHL {},
    //  cp   r           Bx         4 z1hc compare A-r
    CompareRegister { r: Register },
    //  cp   n           FE nn      8 z1hc compare A-n
    CompareConstant { n: u8 },
    //  cp   (HL)        BE         8 z1hc compare A-(HL)
    CompareMemoryHL {},
    //  inc  r           xx         4 z0h- r=r+1
    IncrementRegister { r: Register },
    //  inc  (HL)        34        12 z0h- (HL)=(HL)+1
    IncrementMemoryHL {},
    //  dec  r           xx         4 z1h- r=r-1
    DecrementRegister { r: Register },
    //  dec  (HL)        35        12 z1h- (HL)=(HL)-1
    DecrementMemoryHL {},
    //  daa              27         4 z-0x decimal adjust akku
    DecimalAdjust {},
    //  cpl              2F         4 -11- A = A xor FF
    CPL {},

    //GMB 16bit-Arithmetic/logical Commands
    //add  HL,rr     x9           8 -0hc HL = HL+rr     ;rr may be BC,DE,HL,SP
    AddRegisterHL16bit { r: Register16bit },
    AddRegisterSPtoHL16bit {},
    //inc  rr        x3           8 ---- rr = rr+1      ;rr may be BC,DE,HL,SP
    IncrementRegister16bit { r: Register16bit },
    IncrementSP {},
    //dec  rr        xB           8 ---- rr = rr-1      ;rr may be BC,DE,HL,SP
    DecrementRegister16bit { r: Register16bit },
    DecrementSP {},
    //add  SP,dd     E8          16 00hc SP = SP +/- dd ;dd is 8bit signed number
    AddSP { d: i8 },
    //ld   HL,SP+dd  F8          12 00hc HL = SP +/- dd ;dd is 8bit signed number
    LoadHLwithSPplus { d: i8 },

    //GMB Rotate- und Shift-Commands
    //rlca           07           4 000c rotate akku left
    RotateLeft {},
    //rla            17           4 000c rotate akku left through carry
    RotateLeftThroughCarry {},
    //rrca           0F           4 000c rotate akku right
    RotateRight {},
    //rra            1F           4 000c rotate akku right through carry
    RotateRightThroughCarry {},
    //rlc  r         CB 0x        8 z00c rotate left
    RotateLeftRegisterThroughCarry { r: Register },
    //rlc  (HL)      CB 06       16 z00c rotate left
    RotateLeftHLThroughCarry {},
    //rrc  r         CB 0x        8 z00c rotate right
    RotateRightRegisterThroughCarry { r: Register },
    //rrc  (HL)      CB 0E       16 z00c rotate right
    RotateRightHLThroughCarry {},
    //rl   r         CB 1x        8 z00c rotate left through carry
    RotateLeftRegister { r: Register },
    //rl   (HL)      CB 16       16 z00c rotate left through carry
    RotateLeftHL {},
    //rr   r         CB 1x        8 z00c rotate right through carry
    RotateRightRegister { r: Register },
    //rr   (HL)      CB 1E       16 z00c rotate right through carry
    RotateRightHL {},
    //sla  r         CB 2x        8 z00c shift left arithmetic (b0=0)
    ShiftLeftRegister { r: Register },
    //sla  (HL)      CB 26       16 z00c shift left arithmetic (b0=0)
    ShiftLeftHL {},
    //sra  r         CB 2x        8 z00c shift right arithmetic (b7=b7)
    ShiftRightArithmeticRegister { r: Register },
    //sra  (HL)      CB 2E       16 z00c shift right arithmetic (b7=b7)
    ShiftRightArithmeticHL {},
    //swap r         CB 3x        8 z000 exchange low/hi-nibble
    SwapRegister { r: Register },
    //swap (HL)      CB 36       16 z000 exchange low/hi-nibble
    SwapHL {},
    //srl  r         CB 3x        8 z00c shift right logical (b7=0)
    ShiftRightLogicalRegister { r: Register },
    //srl  (HL)      CB 3E       16 z00c shift right logical (b7=0)
    ShiftRightLogicalHL {},

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
    CCF {},
    //scf            37           4 -001 cy=1
    SCF {},
    //nop            00           4 ---- no operation
    NOP {},
    //halt           76         N*4 ---- halt until interrupt occurs (low power)
    HALT {},
    //stop           10 00        ? ---- low power standby mode (VERY low power)
    STOP {},
    //di             F3           4 ---- disable interrupts, IME=0
    DI {},
    //ei             FB           4 ---- enable interrupts, IME=1
    EI {},

    //GMB Jump Commands
    //jp   nn        C3 nn nn    16 ---- jump to nn, PC=nn
    JP { nn: u16 },
    //jp   HL        E9           4 ---- jump to HL, PC=HL
    JPtoMemoryHL {},
    //jp   f,nn      xx nn nn 16;12 ---- conditional jump if nz,z,nc,c
    CondJP { flag: ConditionalFlag, nn: u16 },
    //jr   PC+dd     18 dd       12 ---- relative jump to nn (PC=PC+/-7bit)
    JRtoMemory { dd: i8 },
    //jr   f,PC+dd   xx dd     12;8 ---- conditional relative jump if nz,z,nc,c
    CondJR { flag: ConditionalFlag, dd: i8 },
    //call nn        CD nn nn    24 ---- call to nn, SP=SP-2, (SP)=PC, PC=nn
    CALL { nn: u16 },
    //call f,nn      xx nn nn 24;12 ---- conditional call if nz,z,nc,c
    CondCALL { flag: ConditionalFlag, nn: u16 },
    //ret            C9          16 ---- return, PC=(SP), SP=SP+2
    RET {},
    //ret  f         xx        20;8 ---- conditional return if nz,z,nc,c
    CondRET { flag: ConditionalFlag },
    //reti           D9          16 ---- return and enable interrupts (IME=1)
    RETI {},
    //rst  n         xx          16 ---- call to 00,08,10,18,20,28,30,38
    RST { n: u16 },
}

#[derive(Clone, Copy)]
enum Rom {
    Local {},
    Bank1 {},
    Bank2 {},
}

impl std::convert::From<usize> for Rom {
    fn from(bank: usize) -> Self {
        match bank {
            1 => Rom::Local {},
            2 => Rom::Bank1 {},
            3 => Rom::Bank2 {},
            _ => unreachable!(),
        }
    }
}

impl std::convert::From<Rom> for usize {
    fn from(bank: Rom) -> Self {
        match bank {
            Rom::Local {} => 1,
            Rom::Bank1 {} => 2,
            Rom::Bank2 {} => 3,
        }
    }
}

#[derive(Clone, Copy)]
enum Ram {
    Bank0 {},
    Bank1 {},
    Bank2 {},
    Bank3 {},
}

impl std::convert::From<usize> for Ram {
    fn from(bank: usize) -> Self {
        match bank {
            0 => Ram::Bank0 {},
            1 => Ram::Bank1 {},
            2 => Ram::Bank2 {},
            3 => Ram::Bank3 {},
            _ => unreachable!(),
        }
    }
}

impl std::convert::From<Ram> for usize {
    fn from(bank: Ram) -> Self {
        match bank {
            Ram::Bank0 {} => 0,
            Ram::Bank1 {} => 1,
            Ram::Bank2 {} => 2,
            Ram::Bank3 {} => 3,
        }
    }
}

struct MemoryBus {
    raw_memory: Vec<u8>,
    cartridge: Vec<u8>,

    ram_banks: Vec<u8>,
    ram_bank: Ram,

    rom_bank: Rom,

    should_enable_ram: bool,
    is_rom_banking: bool,
}

// 0000-3FFF 16KB ROM Bank 00 (in cartridge, fixed at bank 00)
// 4000-7FFF 16KB ROM Bank 01..NN (in cartridge, switchable bank number)
// 8000-9FFF 8KB Video RAM (VRAM) (switchable bank 0-1 in CGB Mode)
// A000-BFFF 8KB External RAM (in cartridge, switchable bank, if any)
// C000-CFFF 4KB Work RAM Bank 0 (WRAM)
// D000-DFFF 4KB Work RAM Bank 1 (WRAM) (switchable bank 1-7 in CGB Mode)
// E000-FDFF Same as C000-DDFF (ECHO) (typically not used)
// FE00-FE9F Sprite Attribute Table (OAM)
// FEA0-FEFF Not Usable
// FF00-FF7F I/O Ports
// FF80-FFFE High RAM (HRAM)
// FFFF Interrupt Enable Register

impl MemoryBus {
    pub fn new(cartridge: &[u8]) -> Self {
        let mut m = vec![0; 0x10000];
        // Nintendo logo etc.
        m[0xFF05] = 0x00;
        m[0xFF06] = 0x00;
        m[0xFF07] = 0x00;
        m[0xFF10] = 0x80;
        m[0xFF11] = 0xBF;
        m[0xFF12] = 0xF3;
        m[0xFF14] = 0xBF;
        m[0xFF16] = 0x3F;
        m[0xFF17] = 0x00;
        m[0xFF19] = 0xBF;
        m[0xFF1A] = 0x7F;
        m[0xFF1B] = 0xFF;
        m[0xFF1C] = 0x9F;
        m[0xFF1E] = 0xBF;
        m[0xFF20] = 0xFF;
        m[0xFF21] = 0x00;
        m[0xFF22] = 0x00;
        m[0xFF23] = 0xBF;
        m[0xFF24] = 0x77;
        m[0xFF25] = 0xF3;
        m[0xFF26] = 0xF1;
        m[0xFF40] = 0x91;
        m[0xFF42] = 0x00;
        m[0xFF43] = 0x00;
        m[0xFF45] = 0x00;
        m[0xFF47] = 0xFC;
        m[0xFF48] = 0xFF;
        m[0xFF49] = 0xFF;
        m[0xFF4A] = 0x00;
        m[0xFF4B] = 0x00;
        m[0xFFFF] = 0x00;

        let b = match m[0x147] {
            1..=3 => Rom::Bank1 {},
            5..=6 => Rom::Bank2 {},
            _ => Rom::Local {},
        };

        MemoryBus {
            raw_memory: m,
            cartridge: cartridge.to_vec(),
            ram_banks: vec![0; 4 * 0x2000],
            ram_bank: Ram::Bank0 {},
            rom_bank: b,
            should_enable_ram: false,
            is_rom_banking: false,
        }
    }

    fn write_word(&mut self, address: MemoryAddress, data: u16) {
        self.write(address, (data & 0xFF) as u8);
        self.write(address + 1, (data >> 8) as u8);
    }

    fn write(&mut self, address: MemoryAddress, data: u8) {
        match address {
            0x0000..=0x1FFF => {
                match self.ram_banks {
                    Ram::None {} => {}
                    _ => {
                        // Do RAM Enable
                        if let Rom::Bank2 {} = self.rom_bank {
                            if Cpu::ith_bit(self.raw_memory[address], 4) {
                                return;
                            }
                        }
                        if data & 0x0F == 0x0A {
                            self.should_enable_ram = true;
                        } else if data & 0x0F == 0x00 {
                            self.should_enable_ram = false;
                        }
                    }
                }
            }
            0x2000..=0x3FFF => {
                match self.ram_banks {
                    Ram::None {} => {}
                    _ => {
                        // Do ROM bank change
                        if let Rom::Bank2 {} = self.rom_bank {
                            self.rom_bank = Rom::from((data & 0xF) as usize);
                            return;
                        }

                        self.rom_bank =
                            Rom::from(((self.rom_bank_idx() as u8 & 244) | (data & 31)) as usize);
                    }
                }
            }
            0x4000..=0x5FFF => {
                match self.ram_banks {
                    Ram::Bank1 {} => {
                        if self.is_rom_banking {
                            let rom_bank_idx = usize::from(self.rom_bank);
                            self.rom_bank = Rom::from();

                            // Change Hi rom bank
                            //                            void Emulator::DoChangeHiRomBank(BYTE data)
                            //                            {
                            //                                // turn off the upper 3 bits of the current rom
                            //                                m_CurrentROMBank &= 31 ;
                            //
                            //                                // turn off the lower 5 bits of the data
                            //                                data &= 224 ;
                            //                                m_CurrentROMBank |= data ;
                            //                                if (m_CurrentROMBank == 0) m_CurrentROMBank++ ;
                            //                            }
                            unimplemented!();
                        } else {
                            // Do rom bank change
                            //                            void Emulator::DoRAMBankChange(BYTE data)
                            //                            {
                            //                                m_CurrentRAMBank = data & 0x3 ;
                            //                            }
                            self.rom_bank = Rom::from(
                                ((self.rom_bank_idx() as u8 & 244) | (data & 31)) as usize,
                            );
                            unimplemented!();
                        }
                    }
                    _ => {}
                }
            }
            0x6000..=0x7FFF => {
                match self.ram_banks {
                    Ram::Bank1 {} => {
                        // Change ROM/RAM mode
                        self.is_rom_banking = data & 0x01 == 0x01;
                        if (self.is_rom_banking) {
                            self.ram_bank = Ram::Bank0 {};
                        }
                    }
                    _ => {}
                }
            }
            0xA000..=0xBFFF => {
                if self.should_enable_ram {
                    self.ram_banks
                        [address as usize - 0xA000 + 0x2000 * usize::from(&self.ram_bank)] = data;
                }
            }
            0xE000..=0xFDFF => unreachable!(),
            0xFEA0..=0xFEFE => {
                self.write(address - 0x2000, data);
            }
            _ => self.raw_memory[address as usize] = data,
        }
    }

    fn read_word(&self, address: MemoryAddress) -> u16 {
        ((self.read(address + 1) as u16) << 8) | self.read(address) as u16
    }

    fn read(&self, address: MemoryAddress) -> u8 {
        match address {
            0x4000..=0x7FFF => {
                self.cartridge[address as usize - 0x4000 + 0x4000 * usize::from(self.rom_bank)]
            }
            0xA000..=0xBFFF => {
                self.ram_banks[address as usize - 0xA000 + 0x2000 * usize::from(self.ram_bank)]
            }
            _ => self.raw_memory[address as usize],
        }
    }
}

struct Cpu {
    reg: [u8; 8],
    pc: MemoryAddress,
    mem: MemoryBus,
    sp: MemoryAddress,
    is_halted: bool,
    is_stopped: bool,
    are_interrupts_enabled: bool,
}

const TILE_SET_1_PART_1_START: usize = 0x8000;
const TILE_SET_1_PART_1_END: usize = 0x87FF;
const TILE_SET_1_PART_2_START: usize = 0x8800;
const TILE_SET_1_PART_2_END: usize = 0x8FFF;

const TILE_SET_2_PART_1_START: usize = 0x8800;
const TILE_SET_2_PART_1_END: usize = 0x8FFF;
const TILE_SET_2_PART_2_START: usize = 0x9000;
const TILE_SET_2_PART_2_END: usize = 0x97FF;

const VRAM_BEGIN: usize = TILE_SET_1_PART_1_START;
const VRAM_END: usize = TILE_SET_2_PART_2_END;
const VRAM_SIZE: usize = VRAM_END - VRAM_BEGIN + 1;

struct Flags {
    z: bool,
    n: bool,
    h: bool,
    c: bool,
}

impl Cpu {
    pub fn new(cartridge: &[u8]) -> Self {
        Cpu {
            reg: [0x01, 0xB0, 0x00, 0x13, 0x00, 0xD8, 0x01, 0x4D],
            pc: 0x100,
            mem: MemoryBus::new(cartridge),
            sp: 0xFFFF,
            is_halted: false,
            is_stopped: false,
            are_interrupts_enabled: false,
        }
    }
    fn update_timers(&mut self, cycles: u8) {}

    fn update_graphics(&mut self, cycles: u8) {}

    fn do_interrupts(&mut self) {}

    pub fn cycle(&mut self, s: f64) {
        const CLOCKS_PER_SECOND: f64 = 4_194_304.0;

        const TIMER_SPEED: f64 = 60.0;

        if self.is_halted {
            return;
        }

        let total_cycles = (s * CLOCKS_PER_SECOND).round() as u64;
        let mut cur_num_cycles = 0;
        while cur_num_cycles < total_cycles {
            let (instruction, pc_increments) = self.fetch_instruction();

            #[cfg(debug_assertions)]
            println!("PC: 0x{:?} : {:?}", self.pc, instruction);

            let cycles = self.execute_instruction(instruction);
            cur_num_cycles += cycles as u64;
            self.update_timers(cycles);
            self.update_graphics(cycles);
            self.do_interrupts();
            self.pc += pc_increments;

            //            if self.cur_instruction_num % (CLOCK_SPEED / TIMER_SPEED) as u64 == 0 {
            //                self.delay_timer -= if self.delay_timer > 0 { 1 } else { 0 };
            //                self.sound_timer -= if self.sound_timer > 0 { 1 } else { 0 };
            //            }
            // update graphics
            // do interrupts
        }
    }
    fn write_word(&mut self, r: Register16bit, val: u16) {
        self.reg[r as usize + 1] = (val >> 8) as u8;
        self.reg[r as usize] = (val & 0xFF) as u8;
    }

    fn read_word(&self, r: Register16bit) -> u16 {
        ((self.reg[r as usize + 1] as u16) << 8) | self.reg[r as usize] as u16
    }

    fn set_flag(&mut self, flag: ConditionalFlag, state: bool) {
        // 7      	    6            	5            	4 	        3 	2 	1 	0
        // Z (Zero) 	N (Subtract) 	H (Half Carry) 	C (Carry) 	0 	0 	0 	0
        self.reg[Register::F {} as usize] &= (state as u8) << (flag as u8);
    }
    fn ith_bit(n: u8, i: u8) -> bool {
        n & (1 << i) == (1 << i)
    }

    fn get_flag(&self, flag: ConditionalFlag) -> bool {
        // 7      	    6            	5            	4 	        3 	2 	1 	0
        // Z (Zero) 	N (Subtract) 	H (Half Carry) 	C (Carry) 	0 	0 	0 	0
        Cpu::ith_bit(self.reg[Register::F {} as usize], flag as usize)
    }

    fn set_flags(&mut self, flags: Flags) {
        self.set_flag(Zero {}, flags.z);
        self.set_flag(Subtract {}, flags.n);
        self.set_flag(HalfCarry {}, flags.h);
        self.set_flag(Carry {}, flags.c);
    }

    fn add_16bit(a: u16, b: u16) -> (u16, Flags) {
        let res = a as u32 + b as u32;
        (
            res as u16,
            Flags {
                z: res == 0,
                n: false,
                h: ((a as u32 ^ b as u32 ^ res) & 0x1000) == 0x1000,
                c: res > 0xFFFF,
            },
        )
    }

    fn sub_16bit(a: u16, b: u16) -> (u16, Flags) {
        let res = a as i32 - b as i32;
        (
            res as u16,
            Flags {
                z: res == 0,
                n: true,
                h: ((a as i32 ^ -(b as i32) ^ res) & 0x1000) == 0x1000,
                c: res < 0,
            },
        )
    }

    fn add_8bit(a: u8, b: u8) -> (u8, Flags) {
        let res = a as u16 + b as u16;
        (
            res as u8,
            Flags {
                z: res == 0,
                n: false,
                h: ((a as u16 ^ b as u16 ^ res) & 0x0010) == 0x0010,
                c: res > 0xFF,
            },
        )
    }

    fn sub_8bit(a: u8, b: u8) -> (u8, Flags) {
        let res = a as i16 - b as i16;
        (
            res as u8,
            Flags {
                z: res == 0,
                n: true,
                h: ((a as i16 ^ -(b as i16) ^ res) & 0x0010) == 0x0010,
                c: res < 0,
            },
        )
    }

    fn execute_instruction(&mut self, instr: Instruction) -> u8 {
        let rotate_left = |x| ((x << 1) | Cpu::ith_bit(x, 7) as u8, Cpu::ith_bit(x, 7));
        let rotate_left_carry = |x| ((x << 1) | self.get_flag(Carry {}) as u8, Cpu::ith_bit(x, 7));

        let rotate_right = |x| {
            (
                (x >> 1) | ((Cpu::ith_bit(x, 0) as u8) << 7),
                Cpu::ith_bit(x, 0),
            )
        };
        let rotate_right_carry = |x| {
            (
                (x >> 1) | ((self.get_flag(Carry {}) as u8) << 7),
                Cpu::ith_bit(x, 0),
            )
        };

        match instr {
            // GMB 8bit-Load commands
            // ld   r,r         xx         4 ---- r=r
            LoadReg { to, from } => {
                self.reg[to as usize] = self.reg[from as usize];
                4
            }
            // ld   r,n         xx nn      8 ---- r=n
            LoadRegWithConstant { to, n } => {
                self.reg[to as usize] = n;
                8
            }
            // ld   r,(HL)      xx         8 ---- r=(HL)
            LoadRegWithMemoryHL { to } => {
                let address = self.read_word(Register16bit::HL {});
                self.reg[to as usize] = self.mem.read(address);
                8
            }
            // ld   (HL),r      7x         8 ---- (HL)=r
            LoadMemoryHLwithRegister { from } => {
                let address = self.read_word(Register16bit::HL {});
                self.mem.write(address, self.reg[from as usize]);
                8
            }
            // ld   (HL),n      36 nn     12 ----
            LoadMemoryHLwithConstant { n } => {
                let address = self.read_word(Register16bit::HL {});
                self.mem.write(address, n);
                12
            }
            // ld   A,(BC)      0A         8 ----
            LoadAwithValBC {} => {
                let address = self.read_word(Register16bit::BC {});
                self.reg[Register::A {} as usize] = self.mem.read(address);
                8
            }
            // ld   A,(DE)      1A         8 ----
            LoadAwithValDE {} => {
                let address = self.read_word(Register16bit::DE {});
                self.reg[Register::A {} as usize] = self.mem.read(address);
                8
            }
            // ld   A,(nn)      FA        16 ----
            LoadAwithMemory { nn } => {
                assert!(nn < 0xFFFF);
                self.reg[Register::A {} as usize] = self.mem.read(nn);
                16
            }
            // ld   (BC),A      02         8 ----
            LoadMemoryBCwithA {} => {
                let address = self.read_word(Register16bit::BC {});
                self.mem.write(address, self.reg[Register::A {} as usize]);
                8
            }
            // ld   (DE),A      12         8 ----
            LoadMemoryDEwithA {} => {
                let address = self.read_word(Register16bit::DE {});
                self.mem.write(address, self.reg[Register::A {} as usize]);
                8
            }
            // ld   (nn),A      EA        16 ----
            LoadMemoryNNwithA { nn } => {
                self.mem.write(nn, self.reg[Register::A {} as usize]);
                16
            }
            // ld   A,(FF00+n)  F0 nn     12 ---- read from io-port n (memory FF00+n)
            LoadAwithFF00plusN { nn } => {
                self.reg[Register::A {} as usize] = self.mem.read(nn as u16 + 0xFF00);
                12
            }
            // ld   (FF00+n),A  E0 nn     12 ---- write to io-port n (memory FF00+n)
            LoadMemoryFF00plusNwithA { nn } => {
                self.mem
                    .write(nn as u16 + 0xFF00, self.reg[Register::A {} as usize]);
                12
            }
            // ld   A,(FF00+C)  F2         8 ---- read from io-port C (memory FF00+C)
            LoadAwithFF00plusC {} => {
                let address = 0xFF00 + self.reg[Register::C {} as usize] as u16;
                self.reg[Register::A {} as usize] = self.mem.read(address);
                8
            }
            // ld   (FF00+C),A  E2         8 ---- write to io-port C (memory FF00+C)
            LoadMemoryFF00plusCwithA {} => {
                let address = 0xFF00 + self.reg[Register::C {} as usize] as u16;
                let val = self.reg[Register::A {} as usize];

                self.mem.write(address, val);

                8
            }
            // ldi  (HL),A      22         8 ---- (HL)=A, HL=HL+1
            LoadMemoryHLwithAandIncr {} => {
                let address = self.read_word(Register16bit::HL {});
                let val = self.reg[Register::A {} as usize];

                self.mem.write(address, val);

                self.write_word(Register16bit::HL {}, address + 1);
                8
            }
            // ldi  A,(HL)      2A         8 ---- A=(HL), HL=HL+1
            LoadAwithValHLandIncr {} => {
                let address = self.read_word(Register16bit::HL {});
                let val = self.mem.read(address);

                self.reg[Register::A {} as usize] = val;

                self.write_word(Register16bit::HL {}, address + 1);
                8
            }
            // ldd  (HL),A      32         8 ---- (HL)=A, HL=HL-1
            LoadMemoryHLwithAandDecr {} => {
                let address = self.read_word(Register16bit::HL {});
                let val = self.reg[Register::A {} as usize];

                self.mem.write(address, val);

                self.write_word(Register16bit::HL {}, address - 1);
                8
            }
            // ldd  A,(HL)      3A         8 ---- A=(HL), HL=HL-1
            LoadAwithValHLandDecr {} => {
                let address = self.read_word(Register16bit::HL {});
                let val = self.mem.read(address);

                self.reg[Register::A {} as usize] = val;

                self.write_word(Register16bit::HL {}, address - 1);
                8
            }

            // GMB 16bit-Load Commands
            // ld   rr,nn       x1 nn nn  12 ---- rr=nn (rr may be BC,DE,HL or SP)
            LoadRegWith16BitConstant { rr, nn } => {
                self.write_word(rr, nn);

                12
            }
            LoadSPWith16BitConstant { nn } => {
                self.sp = nn;

                12
            }
            // ld   SP,HL       F9         8 ---- SP=HL
            LoadSPwithHL {} => {
                self.sp = self.read_word(Register16bit::HL {});
                8
            }
            // push rr          x5 (c5, d5, e5, f5)        16 ---- SP=SP-2  (SP)=rr   (rr may be BC,DE,HL,AF)
            Push { rr } => {
                self.sp -= 2;
                let val = self.read_word(rr);
                self.mem.write_word(self.sp, val);

                16
            }
            // pop  rr          x1 (c1, d1, e1, f1)      12 (AF) rr=(SP)  SP=SP+2   (rr may be BC,DE,HL,AF)
            Pop { rr } => {
                let val = self.mem.read_word(self.sp);
                self.write_word(rr, val);
                self.sp += 2;
                12
            }

            // GMB 8bit-Arithmetic/logical Commands
            //  add  A,r         8x         4 z0hc A=A+r
            AddRegister { r } => {
                let (val, f) =
                    Cpu::add_8bit(self.reg[Register::A {} as usize], self.reg[r as usize]);

                self.reg[Register::A {} as usize] = val;
                self.set_flags(f);
                4
            }
            //  add  A,n         C6 nn      8 z0hc A=A+n
            AddConstant { n } => {
                let (val, f) = Cpu::add_8bit(self.reg[Register::A {} as usize], n);

                self.reg[Register::A {} as usize] = val;
                self.set_flags(f);
                8
            }
            //  add  A,(HL)      86         8 z0hc A=A+(HL)
            AddMemoryHL {} => {
                let address = self.read_word(Register16bit::HL {});
                let (val, f) =
                    Cpu::add_8bit(self.reg[Register::A {} as usize], self.mem.read(address));

                self.reg[Register::A {} as usize] = val;
                self.set_flags(f);
                8
            }
            //  adc  A,r         8x         4 z0hc A=A+r+cy
            AddRegisterWithCarry { r } => {
                let b = self.reg[r as usize] + self.get_flag(Carry {}) as u8;
                let (val, f) = Cpu::add_8bit(self.reg[Register::A {} as usize], b);

                self.reg[Register::A {} as usize] = val;
                self.set_flags(f);
                4
            }
            //  adc  A,n         CE nn      8 z0hc A=A+n+cy
            AddConstantWithCarry { n } => {
                let b = n + self.get_flag(Carry {}) as u8;
                let (val, f) = Cpu::add_8bit(self.reg[Register::A {} as usize], b);

                self.reg[Register::A {} as usize] = val;
                self.set_flags(f);
                8
            }
            //  adc  A,(HL)      8E         8 z0hc A=A+(HL)+cy
            AddMemoryHLWithCarry {} => {
                let address = self.read_word(Register16bit::HL {});
                let b = self.mem.read(address) + self.get_flag(Carry {}) as u8;
                let (val, f) = Cpu::add_8bit(self.reg[Register::A {} as usize], b);

                self.reg[Register::A {} as usize] = val;
                self.set_flags(f);
                8
            }
            //  sub  r           9x         4 z1hc A=A-r
            SubtractRegister { r } => {
                let val = self.reg[r as usize];
                let (val, f) = Cpu::sub_8bit(self.reg[Register::A {} as usize], val);
                self.reg[Register::A {} as usize] = val;
                self.set_flags(f);
                4
            }
            //  sub  n           D6 nn      8 z1hc A=A-n
            SubtractConstant { n } => {
                let (val, f) = Cpu::sub_8bit(self.reg[Register::A {} as usize], n);
                self.reg[Register::A {} as usize] = val;
                self.set_flags(f);
                8
            }
            //  sub  (HL)        96         8 z1hc A=A-(HL)
            SubtractMemoryHL {} => {
                let address = self.read_word(Register16bit::HL {});
                let val = self.mem.read(address);
                let (val, f) = Cpu::sub_8bit(self.reg[Register::A {} as usize], val);
                self.reg[Register::A {} as usize] = val;
                self.set_flags(f);
                8
            }
            //  sbc  A,r         9x         4 z1hc A=A-r-cy
            SubtractRegisterWithCarry { r } => {
                let val = self.reg[r as usize] - self.get_flag(Carry {}) as u8;
                let (val, f) = Cpu::sub_8bit(self.reg[Register::A {} as usize], val);
                self.reg[Register::A {} as usize] = val;
                self.set_flags(f);
                4
            }
            //  sbc  A,n         DE nn      8 z1hc A=A-n-cy
            SubtractConstantWithCarry { n } => {
                let val = n - self.get_flag(Carry {}) as u8;
                let (val, f) = Cpu::sub_8bit(self.reg[Register::A {} as usize], val);
                self.reg[Register::A {} as usize] = val;
                self.set_flags(f);
                4
            }
            //  sbc  A,(HL)      9E         8 z1hc A=A-(HL)-cy
            SubtractMemoryHLWithCarry {} => {
                let address = self.read_word(Register16bit::HL {});
                let val = self.mem.read(address) - self.get_flag(Carry {}) as u8;
                let (val, f) = Cpu::sub_8bit(self.reg[Register::A {} as usize], val);

                self.reg[Register::A {} as usize] = val;
                self.set_flags(f);
                8
            }
            //  and  r           Ax         4 z010 A=A & r
            AndRegister { r } => {
                self.reg[Register::A {} as usize] &= self.reg[r as usize];
                self.set_flags(Flags {
                    z: self.reg[Register::A {} as usize] == 0,
                    n: false,
                    h: true,
                    c: false,
                });
                4
            }
            //  and  n           E6 nn      8 z010 A=A & n
            AndConstant { n } => {
                self.reg[Register::A {} as usize] &= n;
                self.set_flags(Flags {
                    z: self.reg[Register::A {} as usize] == 0,
                    n: false,
                    h: true,
                    c: false,
                });
                8
            }
            //  and  (HL)        A6         8 z010 A=A & (HL)
            AndMemoryHL {} => {
                let address = self.read_word(Register16bit::HL {});
                self.reg[Register::A {} as usize] &= self.mem.read(address);
                self.set_flags(Flags {
                    z: self.reg[Register::A {} as usize] == 0,
                    n: false,
                    h: true,
                    c: false,
                });
                8
            }
            //  xor  r           Ax         4 z000
            XorRegister { r } => {
                self.reg[Register::A {} as usize] ^= self.reg[r as usize];
                self.set_flags(Flags {
                    z: self.reg[Register::A {} as usize] == 0,
                    n: false,
                    h: false,
                    c: false,
                });
                4
            }
            //  xor  n           EE nn      8 z000
            XorConstant { n } => {
                self.reg[Register::A {} as usize] ^= n;
                self.set_flags(Flags {
                    z: self.reg[Register::A {} as usize] == 0,
                    n: false,
                    h: false,
                    c: false,
                });
                8
            }
            //  xor  (HL)        AE         8 z000
            XorMemoryHL {} => {
                let address = self.read_word(Register16bit::HL {});
                self.reg[Register::A {} as usize] ^= self.mem.read(address);
                self.set_flags(Flags {
                    z: self.reg[Register::A {} as usize] == 0,
                    n: false,
                    h: false,
                    c: false,
                });
                8
            }
            //  or   r           Bx         4 z000 A=A | r
            OrRegister { r } => {
                self.reg[Register::A {} as usize] |= self.reg[r as usize];
                self.set_flags(Flags {
                    z: self.reg[Register::A {} as usize] == 0,
                    n: false,
                    h: false,
                    c: false,
                });
                8
            }
            //  or   n           F6 nn      8 z000 A=A | n
            OrConstant { n } => {
                self.reg[Register::A {} as usize] |= n;
                self.set_flags(Flags {
                    z: self.reg[Register::A {} as usize] == 0,
                    n: false,
                    h: false,
                    c: false,
                });
                8
            }
            //  or   (HL)        B6         8 z000 A=A | (HL)
            OrMemoryHL {} => {
                let address = self.read_word(Register16bit::HL {});
                self.reg[Register::A {} as usize] |= self.mem.read(address);
                self.set_flags(Flags {
                    z: self.reg[Register::A {} as usize] == 0,
                    n: false,
                    h: false,
                    c: false,
                });
                8
            }
            //  cp   r           Bx         4 z1hc compare A-r
            CompareRegister { r } => {
                let (_, f) = Cpu::sub_8bit(self.reg[Register::A {} as usize], self.reg[r as usize]);
                self.set_flags(f);
                4
            }
            //  cp   n           FE nn      8 z1hc compare A-n
            CompareConstant { n } => {
                let (_, f) = Cpu::sub_8bit(self.reg[Register::A {} as usize], n);
                self.set_flags(f);
                8
            }
            //  cp   (HL)        BE         8 z1hc compare A-(HL)
            CompareMemoryHL {} => {
                let address = self.read_word(Register16bit::HL {});
                let val = self.mem.read(address);
                let (_, f) = Cpu::sub_8bit(self.reg[Register::A {} as usize], val);

                self.set_flags(f);
                8
            }
            //  inc  r           xx         4 z0h- r=r+1
            IncrementRegister { r } => {
                let val = self.reg[r as usize];
                let (val, f) = Cpu::add_8bit(val, 1);

                self.reg[r as usize] = val;
                self.set_flags(Flags {
                    z: f.z,
                    n: false,
                    h: f.h,
                    c: self.get_flag(Carry {}),
                });
                4
            }
            //  inc  (HL)        34        12 z0h- (HL)=(HL)+1
            IncrementMemoryHL {} => {
                let address = self.read_word(Register16bit::HL {});
                let val = self.mem.read(address);
                let (val, f) = Cpu::add_8bit(val, 1);

                self.mem.write(address, val);
                self.set_flags(Flags {
                    z: f.z,
                    n: false,
                    h: f.h,
                    c: self.get_flag(Carry {}),
                });
                12
            }
            //  dec  r           xx         4 z1h- r=r-1
            DecrementRegister { r } => {
                let val = self.reg[r as usize];
                let (val, f) = Cpu::sub_8bit(val, 1);

                self.reg[r as usize] = val;
                self.set_flags(Flags {
                    z: f.z,
                    n: false,
                    h: f.h,
                    c: self.get_flag(Carry {}),
                });
                4
            }
            //  dec  (HL)        35        12 z1h- (HL)=(HL)-1
            DecrementMemoryHL {} => {
                let address = self.read_word(Register16bit::HL {});
                let val = self.mem.read(address);
                let (val, f) = Cpu::sub_8bit(val, 1);

                self.mem.write(address, val);
                self.set_flags(Flags {
                    z: f.z,
                    n: false,
                    h: f.h,
                    c: self.get_flag(Carry {}),
                });
                12
            }
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
                    if self.get_flag(Carry {}) || (self.reg[Register::A {} as usize] > 0x99) {
                        let (val, f) = Cpu::add_8bit(self.reg[Register::A {} as usize], 0x60);
                        self.reg[Register::A {} as usize] = val;
                        self.set_flags(f);
                    }
                    if self.get_flag(HalfCarry {})
                        || (self.reg[Register::A {} as usize] & 0x0f) > 0x09
                    {
                        let (val, f) = Cpu::add_8bit(self.reg[Register::A {} as usize], 0x6);
                        self.reg[Register::A {} as usize] = val;
                        self.set_flags(f);
                    }
                } else {
                    // after a subtraction, only adjust if (half-)carry occurred
                    if self.get_flag(Carry {}) {
                        let (val, f) = Cpu::sub_8bit(self.reg[Register::A {} as usize], 0x60);
                        self.reg[Register::A {} as usize] = val;
                    }
                    if self.get_flag(HalfCarry {}) {
                        let (val, f) = Cpu::sub_8bit(self.reg[Register::A {} as usize], 0x6);
                        self.reg[Register::A {} as usize] = val;
                    }
                }

                self.set_flag(HalfCarry {}, false);
                4
            }
            //  cpl              2F         4 -11- A = A xor FF
            CPL {} => {
                // Complement all bits
                self.reg[Register::A {} as usize] ^= 0xFF;
                4
            }

            //GMB 16bit-Arithmetic/logical Commands
            //add  HL,rr     x9           8 -0hc HL = HL+rr     ;rr may be BC,DE,HL,SP
            AddRegisterHL16bit { r } => {
                let (val, f) =
                    Cpu::add_16bit(self.read_word(Register16bit::HL {}), self.read_word(r));
                self.write_word(Register16bit::HL {}, val);
                self.set_flags(Flags {
                    z: self.get_flag(Zero {}),
                    n: false,
                    h: f.h,
                    c: f.c,
                });
                8
            }
            AddRegisterSPtoHL16bit {} => {
                let (val, f) = Cpu::add_16bit(self.read_word(Register16bit::HL {}), self.sp);
                self.write_word(Register16bit::HL {}, val);
                self.set_flags(Flags {
                    z: self.get_flag(Zero {}),
                    n: false,
                    h: f.h,
                    c: f.c,
                });
                8
            }
            //inc  rr        x3           8 ---- rr = rr+1      ;rr may be BC,DE,HL,SP
            IncrementRegister16bit { r } => {
                let (val, _) = Cpu::add_16bit(self.read_word(r), 1);
                self.write_word(r, val);
                8
            }
            IncrementSP {} => {
                self.sp += 1;
                8
            }
            //dec  rr        xB           8 ---- rr = rr-1      ;rr may be BC,DE,HL,SP
            DecrementRegister16bit { r } => {
                let (val, f) = Cpu::sub_16bit(self.read_word(r), 1);
                self.write_word(Register16bit::HL {}, val);
                8
            }
            DecrementSP {} => {
                self.sp -= 1;
                8
            }
            //add  SP,dd     E8          16 00hc SP = SP +/- dd ;dd is 8bit signed number
            AddSP { d } => {
                let (val, f) = if d > 0 {
                    Cpu::add_16bit(self.sp, d as u16)
                } else {
                    Cpu::sub_16bit(self.sp, d.abs() as u16)
                };
                self.sp = val;
                self.set_flags(Flags {
                    z: false,
                    n: false,
                    h: f.h,
                    c: f.c,
                });
                16
            }
            //ld   HL,SP+dd  F8          12 00hc HL = SP +/- dd ;dd is 8bit signed number
            LoadHLwithSPplus { d } => {
                let (val, f) = if d > 0 {
                    Cpu::add_16bit(self.sp, d as u16)
                } else {
                    Cpu::sub_16bit(self.sp, d.abs() as u16)
                };
                self.write_word(Register16bit::HL {}, val);
                self.set_flags(Flags {
                    z: false,
                    n: false,
                    h: f.h,
                    c: f.c,
                });
                12
            }

            //GMB Rotate- und Shift-Commands
            //rlca           07           4 000c rotate akku left
            RotateLeft {} => {
                let (val, carry) = rotate_left(self.reg[Register::A {} as usize]);
                self.reg[Register::A {} as usize] = val;
                self.set_flags(Flags {
                    z: false,
                    n: false,
                    h: false,
                    c: carry,
                });
                4
            }
            //rla            17           4 000c rotate akku left through carry
            RotateLeftThroughCarry {} => {
                let (val, carry) = rotate_left_carry(self.reg[Register::A {} as usize]);
                self.reg[Register::A {} as usize] = val;
                self.set_flags(Flags {
                    z: false,
                    n: false,
                    h: false,
                    c: carry,
                });
                4
            }
            //rrca           0F           4 000c rotate akku right
            RotateRight {} => {
                let (val, carry) = rotate_right(self.reg[Register::A {} as usize]);
                self.reg[Register::A {} as usize] = val;
                self.set_flags(Flags {
                    z: false,
                    n: false,
                    h: false,
                    c: carry,
                });
                4
            }
            //rra            1F           4 000c rotate akku right through carry
            RotateRightThroughCarry {} => {
                let (val, carry) = rotate_right_carry(self.reg[Register::A {} as usize]);
                self.reg[Register::A {} as usize] = val;
                self.set_flags(Flags {
                    z: false,
                    n: false,
                    h: false,
                    c: carry,
                });
                4
            }
            //rlc  r         CB 0x        8 z00c rotate left
            RotateLeftRegisterThroughCarry { r } => {
                let (val, carry) = rotate_left(self.reg[r as usize]);
                self.reg[r as usize] = val;
                self.set_flags(Flags {
                    z: val == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                4
            }
            //rlc  (HL)      CB 06       16 z00c rotate left
            RotateLeftHLThroughCarry {} => {
                let address = self.read_word(Register16bit::HL {});
                let (val, carry) = rotate_left_carry(self.mem.read(address));
                self.mem.write(address, val);
                self.set_flags(Flags {
                    z: val == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                4
            }
            //rrc  r         CB 0x        8 z00c rotate right
            RotateRightRegisterThroughCarry { r } => {
                let (val, carry) = rotate_right(self.reg[r as usize]);
                self.reg[r as usize] = val;
                self.set_flags(Flags {
                    z: val == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                8
            }
            //rrc  (HL)      CB 0E       16 z00c rotate right through carry
            RotateRightHLThroughCarry {} => {
                let address = self.read_word(Register16bit::HL {});
                let (val, carry) = rotate_right_carry(self.mem.read(address));
                self.mem.write(address, val);
                self.set_flags(Flags {
                    z: val == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                8
            }
            //rl   r         CB 1x        8 z00c rotate left
            RotateLeftRegister { r } => {
                let (val, carry) = rotate_left(self.reg[r as usize]);
                self.reg[r as usize] = val;
                self.set_flags(Flags {
                    z: val == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                4
            }
            //rl   (HL)      CB 16       16 z00c rotate left
            RotateLeftHL {} => {
                let address = self.read_word(Register16bit::HL {});
                let (val, carry) = rotate_left(self.mem.read(address));
                self.mem.write(address, val);
                self.set_flags(Flags {
                    z: val == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                16
            }
            //rr   r         CB 1x        8 z00c rotate right
            RotateRightRegister { r } => {
                let (val, carry) = rotate_right(self.reg[r as usize]);
                self.reg[r as usize] = val;
                self.set_flags(Flags {
                    z: val == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                4
            }
            //rr   (HL)      CB 1E       16 z00c rotate right
            RotateRightHL {} => {
                let address = self.read_word(Register16bit::HL {});
                let val = self.mem.read(address);
                let (val, carry) = rotate_right(val);
                self.mem.write(address, val);
                self.set_flags(Flags {
                    z: val == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                16
            }
            //sla  r         CB 2x        8 z00c shift left arithmetic (b0=0)
            ShiftLeftRegister { r } => {
                let carry = Cpu::ith_bit(self.reg[r as usize], 7);
                self.reg[r as usize] <<= 1;
                self.set_flags(Flags {
                    z: self.reg[r as usize] == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                8
            }
            //sla  (HL)      CB 26       16 z00c shift left arithmetic (b0=0)
            ShiftLeftHL {} => {
                let address = self.read_word(Register16bit::HL {});
                let val = self.mem.read(address);
                let carry = Cpu::ith_bit(val, 7);
                self.mem.write(address, val << 1);
                self.set_flags(Flags {
                    z: self.mem.read(address) == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                8
            }
            //sra  r         CB 2x        8 z00c shift right arithmetic (b7=b7)
            ShiftRightArithmeticRegister { r } => {
                let carry = Cpu::ith_bit(self.reg[r as usize], 0);
                self.reg[r as usize] = (self.reg[r as usize] >> 1) | ((carry as u8) << 7);
                self.set_flags(Flags {
                    z: self.reg[r as usize] == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                8
            }
            //sra  (HL)      CB 2E       16 z00c shift right arithmetic (b7=b7)
            ShiftRightArithmeticHL {} => {
                let address = self.read_word(Register16bit::HL {});
                let val = self.mem.read(address);
                let carry = Cpu::ith_bit(val, 0);
                self.mem.write(address, (val >> 1) | ((carry as u8) << 7));
                self.set_flags(Flags {
                    z: self.mem.read(address) == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                16
            }
            //swap r         CB 3x        8 z000 exchange low/hi-nibble
            SwapRegister { r } => {
                self.reg[r as usize] = (self.reg[r as usize] << 4) | (self.reg[r as usize] >> 4);
                self.set_flags(Flags {
                    z: self.reg[r as usize] == 0,
                    n: false,
                    h: false,
                    c: false,
                });
                8
            }
            //swap (HL)      CB 36       16 z000 exchange low/hi-nibble
            SwapHL {} => {
                let address = self.read_word(Register16bit::HL {});
                let val = self.mem.read(address);
                self.mem.write(address, (val << 4) | (val >> 4));
                self.set_flags(Flags {
                    z: self.mem.read(address) == 0,
                    n: false,
                    h: false,
                    c: false,
                });
                16
            }
            //srl  r         CB 3x        8 z00c shift right logical (b7=0)
            ShiftRightLogicalRegister { r } => {
                let carry = Cpu::ith_bit(self.reg[r as usize], 0);
                self.reg[r as usize] >>= 1;
                self.set_flags(Flags {
                    z: self.reg[r as usize] == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                8
            }
            //srl  (HL)      CB 3E       16 z00c shift right logical (b7=0)
            ShiftRightLogicalHL {} => {
                let address = self.read_word(Register16bit::HL {});
                let val = self.mem.read(address);
                let carry = Cpu::ith_bit(val, 0);
                self.mem.write(address, val >> 1);
                self.set_flags(Flags {
                    z: self.mem.read(address) == 0,
                    n: false,
                    h: false,
                    c: carry,
                });
                8
            }
            //GMB Singlebit Operation Commands
            //bit  n,r       CB xx        8 z01- test bit n
            TestBitRegister { bit, r } => {
                self.set_flags(Flags {
                    z: !Cpu::ith_bit(bit, self.reg[r as usize]),
                    n: false,
                    h: true,
                    c: self.get_flag(Carry {}),
                });
                8
            }
            //bit  n,(HL)    CB xx       12 z01- test bit n
            TestBitMemoryHL { bit } => {
                assert!(bit < 8);
                let address = self.read_word(Register16bit::HL {});
                self.set_flags(Flags {
                    z: !Cpu::ith_bit( bit, self.mem.read(address) ),
                    n: false,
                    h: true,
                    c: self.get_flag(Carry {}),
                });
                12
            }
            //set  n,r       CB xx        8 ---- set bit n
            SetBitRegister { bit, r } => {
                self.reg[r as usize] |= 1 << bit;
                8
            }
            //set  n,(HL)    CB xx       16 ---- set bit n
            SetBitMemoryHL { bit } => {
                let address = self.read_word(Register16bit::HL {});
                let val = self.mem.read(address);
                self.mem.write(address, val | 1 << bit);
                16
            }
            //res  n,r       CB xx        8 ---- reset bit n
            ResetBitRegister { bit, r } => {
                self.reg[r as usize] &= !(1 << bit);
                8
            }
            //res  n,(HL)    CB xx       16 ---- reset bit n
            ResetBitMemoryHL { bit } => {
                let address = self.read_word(Register16bit::HL {});
                let val = self.mem.read(address);
                self.mem.write(address, val & !(1 << bit));
                16
            }
            //GMB CPU-Control Commands
            //ccf            3F           4 -00c cy=cy xor 1
            CCF {} => {
                self.set_flags(Flags {
                    z: self.get_flag(Zero {}),
                    n: false,
                    h: false,
                    c: !self.get_flag(Carry {}),
                });
                4
            }
            //scf            37           4 -001 cy=1
            SCF {} => {
                self.set_flags(Flags {
                    z: self.get_flag(Zero {}),
                    n: false,
                    h: false,
                    c: true,
                });
                4
            }
            //nop            00           4 ---- no operation
            NOP {} => 4,
            //halt           76         N*4 ---- halt until interrupt occurs (low power)
            HALT {} => {
                self.is_halted = true;
                4
            }
            //stop           10 00        ? ---- low power standby mode (VERY low power)
            STOP {} => {
                self.is_stopped = true;
                4
            }
            //di             F3           4 ---- disable interrupts, IME=0
            DI {} => {
                self.are_interrupts_enabled = false;
                4
            }
            //ei             FB           4 ---- enable interrupts, IME=1
            EI {} => {
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
            JPtoMemoryHL {} => {
                let address = self.read_word(Register16bit::HL {});
                self.pc = self.read_word(Register16bit::HL {});
                4
            }
            //jp   f,nn      xx nn nn 16;12 ---- conditional jump if nz,z,nc,c
            CondJP { flag, nn } => {
                if self.get_flag(flag) {
                    self.pc = nn;
                    16
                } else {
                    12
                }
            }
            //jr   PC+dd     18 dd       12 ---- relative jump to nn (PC=PC+/-7bit)
            JRtoMemory { dd } => {
                self.pc = (self.pc as i32 + dd as i32) as u16;
                12
            }
            //jr   f,PC+dd   xx dd     12;8 ---- conditional relative jump if nz,z,nc,c
            CondJR { flag, dd } => {
                if self.get_flag(flag) {
                    self.pc = (self.pc as i32 + dd as i32) as u16;
                    12
                } else {
                    8
                }
            }
            //call nn        CD nn nn    24 ---- call to nn, SP=SP-2, (SP)=PC, PC=nn
            CALL { nn } => {
                self.sp -= 2;
                self.mem.write_word(self.sp, self.pc);
                self.pc = nn;
                24
            }
            //call f,nn      xx nn nn 24;12 ---- conditional call if nz,z,nc,c
            CondCALL { flag, nn } => {
                if self.get_flag(flag) {
                    self.sp -= 2;
                    self.mem.write_word(self.sp, self.pc);
                    self.pc = nn;
                    24
                } else {
                    12
                }
            }
            //ret            C9          16 ---- return, PC=(SP), SP=SP+2
            RET {} => {
                self.pc = self.mem.read_word(self.sp);
                self.sp += 2;
                16
            }
            //ret  f         xx        20;8 ---- conditional return if nz,z,nc,c
            CondRET { flag } => {
                if self.get_flag(flag) {
                    self.pc = self.mem.read_word(self.sp);
                    self.sp += 2;
                    20
                } else {
                    8
                }
            }
            //reti           D9          16 ---- return and enable interrupts (IME=1)
            RETI {} => {
                self.pc = self.mem.read_word(self.sp);
                self.sp += 2;
                self.are_interrupts_enabled = true;
                16
            }
            //rst  n         xx          16 ---- call to 00,08,10,18,20,28,30,38
            RST { n } => {
                self.sp -= 2;
                self.mem.write_word(self.sp, self.pc);
                self.pc = n;
                24
            }
        }
    }

    // (instruction, pc increments)
    fn fetch_instruction(&self) -> (Instruction, usize) {
        let immediate_u8 = || self.mem.read(self.pc + 1);
        let immediate_i8 = || self.mem.read(self.pc + 1) as i8;
        let immediate_u16 = || self.mem.read_word(self.pc + 1);
        let low_nibble = |op: u8| (op & 0b00001111) as usize;

        let op = self.mem.read(self.pc);

        match op {
            // GMB 8bit-Load commands
            // ld   r,r         xx         4 ---- r=r
            // ld   r,(HL)      xx         8 ---- r=(HL)
            0x40..=0x45 => (
                LoadReg {
                    to: Register::B {},
                    from: Register::from(low_nibble(op)),
                },
                1,
            ),
            0x46 => (LoadRegWithMemoryHL { to: Register::B {} }, 1),
            0x47 => (
                LoadReg {
                    to: Register::B {},
                    from: Register::A {},
                },
                1,
            ),

            0x48..=0x4D => (
                LoadReg {
                    to: Register::C {},
                    from: Register::from(low_nibble(op)),
                },
                1,
            ),
            0x4E => (LoadRegWithMemoryHL { to: Register::C {} }, 1),
            0x4F => (
                LoadReg {
                    to: Register::C {},
                    from: Register::A {},
                },
                1,
            ),

            0x50..=0x55 => (
                LoadReg {
                    to: Register::D {},
                    from: Register::from(low_nibble(op)),
                },
                1,
            ),
            0x56 => (LoadRegWithMemoryHL { to: Register::D {} }, 1),
            0x57 => (
                LoadReg {
                    to: Register::D {},
                    from: Register::A {},
                },
                1,
            ),

            0x58..=0x5D => (
                LoadReg {
                    to: Register::E {},
                    from: Register::from(low_nibble(op)),
                },
                1,
            ),
            0x5E => (LoadRegWithMemoryHL { to: Register::E {} }, 1),
            0x5F => (
                LoadReg {
                    to: Register::E {},
                    from: Register::A {},
                },
                1,
            ),

            0x60..=0x65 => (
                LoadReg {
                    to: Register::H {},
                    from: Register::from(low_nibble(op)),
                },
                1,
            ),
            0x66 => (LoadRegWithMemoryHL { to: Register::H {} }, 1),
            0x67 => (
                LoadReg {
                    to: Register::H {},
                    from: Register::A {},
                },
                1,
            ),

            0x68..=0x6D => (
                LoadReg {
                    to: Register::L {},
                    from: Register::from(low_nibble(op)),
                },
                1,
            ),
            0x6E => (LoadRegWithMemoryHL { to: Register::L {} }, 1),
            0x6F => (
                LoadReg {
                    to: Register::L {},
                    from: Register::A {},
                },
                1,
            ),

            0x78..=0x7D => (
                LoadReg {
                    to: Register::A {},
                    from: Register::from(low_nibble(op)),
                },
                1,
            ),
            0x7E => (LoadRegWithMemoryHL { to: Register::A {} }, 1),
            0x7F => (
                LoadReg {
                    to: Register::A {},
                    from: Register::A {},
                },
                1,
            ),

            // ld   r,n         xx nn      8 ---- r=n
            0x06 => (
                LoadRegWithConstant {
                    to: Register::B {},
                    n: immediate_u8(),
                },
                2,
            ),
            0x0E => (
                LoadRegWithConstant {
                    to: Register::C {},
                    n: immediate_u8(),
                },
                2,
            ),
            0x16 => (
                LoadRegWithConstant {
                    to: Register::D {},
                    n: immediate_u8(),
                },
                2,
            ),
            0x1E => (
                LoadRegWithConstant {
                    to: Register::E {},
                    n: immediate_u8(),
                },
                2,
            ),
            0x26 => (
                LoadRegWithConstant {
                    to: Register::H {},
                    n: immediate_u8(),
                },
                2,
            ),
            0x2E => (
                LoadRegWithConstant {
                    to: Register::L {},
                    n: immediate_u8(),
                },
                2,
            ),
            0x3E => (
                LoadRegWithConstant {
                    to: Register::A {},
                    n: immediate_u8(),
                },
                2,
            ),

            // ld   (HL),r      7x         8 ---- (HL)=r
            0x70 => (
                LoadMemoryHLwithRegister {
                    from: Register::B {},
                },
                1,
            ),
            0x71 => (
                LoadMemoryHLwithRegister {
                    from: Register::C {},
                },
                1,
            ),
            0x72 => (
                LoadMemoryHLwithRegister {
                    from: Register::D {},
                },
                1,
            ),
            0x73 => (
                LoadMemoryHLwithRegister {
                    from: Register::E {},
                },
                1,
            ),
            0x74 => (
                LoadMemoryHLwithRegister {
                    from: Register::H {},
                },
                1,
            ),
            0x75 => (
                LoadMemoryHLwithRegister {
                    from: Register::L {},
                },
                1,
            ),
            0x77 => (
                LoadMemoryHLwithRegister {
                    from: Register::A {},
                },
                1,
            ),

            // ld   (HL),n      36 nn     12 ----
            0x36 => (LoadMemoryHLwithConstant { n: immediate_u8() }, 2),
            // ld   A,(BC)      0A         8 ----
            0x0A => (LoadAwithValBC {}, 1),
            // ld   A,(DE)      1A         8 ----
            0x1A => (LoadAwithValDE {}, 1),
            // ld   A,(nn)      FA        16 ----
            0xFA => (
                LoadAwithMemory {
                    nn: immediate_u16(),
                },
                3,
            ),
            // ld   (BC),A      02         8 ----
            0x02 => (LoadMemoryBCwithA {}, 1),
            // ld   (DE),A      12         8 ----
            0x12 => (LoadMemoryDEwithA {}, 1),
            // ld   (nn),A      EA        16 ----
            0xEA => (
                LoadMemoryNNwithA {
                    nn: immediate_u16(),
                },
                3,
            ),
            // ld   A,(FF00+n)  F0 nn     12 ---- read from io-port n (memory FF00+n)
            0xF0 => (
                LoadAwithFF00plusN {
                    nn: immediate_u8(),
                },
                3,
            ),
            // ld   (FF00+n),A  E0 nn     12 ---- write to io-port n (memory FF00+n)
            0xE0 => (
                LoadMemoryFF00plusNwithA {
                    nn: immediate_u8(),
                },
                2,
            ),
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
            0x01 => (
                LoadRegWith16BitConstant {
                    rr: Register16bit::BC {},
                    nn: immediate_u16(),
                },
                3,
            ),
            0x11 => (
                LoadRegWith16BitConstant {
                    rr: Register16bit::DE {},
                    nn: immediate_u16(),
                },
                3,
            ),
            0x21 => (
                LoadRegWith16BitConstant {
                    rr: Register16bit::HL {},
                    nn: immediate_u16(),
                },
                3,
            ),
            0x31 => (
                LoadSPWith16BitConstant {
                    nn: immediate_u16(),
                },
                1,
            ),
            // ld   SP,HL       F9         8 ---- SP=HL
            0xF9 => (LoadSPwithHL {}, 1),
            // push rr          x5 (c5, d5, e5, f5)        16 ---- SP=SP-2  (SP)=rr   (rr may be BC,DE,HL,AF)
            0xc5 => (
                Push {
                    rr: Register16bit::BC {},
                },
                1,
            ),
            0xd5 => (
                Push {
                    rr: Register16bit::DE {},
                },
                1,
            ),
            0xe5 => (
                Push {
                    rr: Register16bit::HL {},
                },
                1,
            ),
            0xf5 => (
                Push {
                    rr: Register16bit::AF {},
                },
                1,
            ),
            // pop  rr          x1 (c1, d1, e1, f1)      12 (AF) rr=(SP)  SP=SP+2   (rr may be BC,DE,HL,AF)
            0xc1 => (
                Pop {
                    rr: Register16bit::BC {},
                },
                1,
            ),
            0xd1 => (
                Pop {
                    rr: Register16bit::DE {},
                },
                1,
            ),
            0xe1 => (
                Pop {
                    rr: Register16bit::HL {},
                },
                1,
            ),
            0xf1 => (
                Pop {
                    rr: Register16bit::AF {},
                },
                1,
            ),

            // GMB 8bit-Arithmetic/logical Commands
            //  add  A,r         8x         4 z0hc A=A+r
            0x80..=0x85 => (
                AddRegister {
                    r: Register::from(low_nibble(op)),
                },
                1,
            ),
            0x86 => (AddMemoryHL {}, 1),
            0x87 => (AddRegister { r: Register::A {} }, 1),
            //  add  A,n         C6 nn      8 z0hc A=A+n
            0xC6 => (AddConstant { n: immediate_u8() }, 2),
            //  add  A,(HL)      86         8 z0hc A=A+(HL)
            //  adc  A,r         8x         4 z0hc A=A+r+cy
            0x88..=0x8D => (
                AddRegisterWithCarry {
                    r: Register::from(low_nibble(op)),
                },
                1,
            ),
            0x8E => (AddMemoryHLWithCarry {}, 1),
            0x8F => (AddRegisterWithCarry { r: Register::A {} }, 1),
            //  adc  A,n         CE nn      8 z0hc A=A+n+cy
            0xCE => (AddConstantWithCarry { n: immediate_u8() }, 2),
            //  adc  A,(HL)      8E         8 z0hc A=A+(HL)+cy
            //  sub  r           9x         4 z1hc A=A-r
            0x90..=0x95 => (
                SubtractRegister {
                    r: Register::from(low_nibble(op)),
                },
                1,
            ),
            0x96 => (SubtractMemoryHL {}, 1),
            0x97 => (SubtractRegister { r: Register::A {} }, 1),
            //  sub  n           D6 nn      8 z1hc A=A-n
            0xD6 => (SubtractConstant { n: immediate_u8() }, 2),
            //  sub  (HL)        96         8 z1hc A=A-(HL)
            //  sbc  A,r         9x         4 z1hc A=A-r-cy
            0x98..=0x9D => (
                SubtractRegisterWithCarry {
                    r: Register::from(low_nibble(op)),
                },
                1,
            ),
            0x9E => (SubtractMemoryHLWithCarry {}, 1),
            0x9F => (SubtractRegisterWithCarry { r: Register::A {} }, 1),
            //  sbc  A,n         DE nn      8 z1hc A=A-n-cy
            0xDE => (SubtractConstantWithCarry { n: immediate_u8() }, 1),
            //  sbc  A,(HL)      9E         8 z1hc A=A-(HL)-cy
            //  and  r           Ax         4 z010 A=A & r
            0xA0..=0xA5 => (
                AndRegister {
                    r: Register::from(low_nibble(op)),
                },
                1,
            ),
            0xA6 => (AndMemoryHL {}, 1),
            0xA7 => (AndRegister { r: Register::A {} }, 1),
            //  and  n           E6 nn      8 z010 A=A & n
            0xE6 => (AndConstant { n: immediate_u8() }, 2),
            //  and  (HL)        A6         8 z010 A=A & (HL)
            //  xor  r           Ax         4 z000
            0xA8..=0xAD => (
                XorRegister {
                    r: Register::from(low_nibble(op)),
                },
                1,
            ),
            0xAE => (XorMemoryHL {}, 1),
            0xAF => (XorRegister { r: Register::A {} }, 1),
            //  xor  n           EE nn      8 z000
            0xEE => (XorConstant { n: immediate_u8() }, 2),
            //  xor  (HL)        AE         8 z000
            //  or   r           Bx         4 z000 A=A | r
            0xB0..=0xB5 => (
                OrRegister {
                    r: Register::from(low_nibble(op)),
                },
                1,
            ),
            0xB6 => (OrMemoryHL {}, 1),
            0xB7 => (OrRegister { r: Register::A {} }, 1),
            //  or   n           F6 nn      8 z000 A=A | n
            0xF6 => (OrConstant { n: immediate_u8() }, 2),
            //  or   (HL)        B6         8 z000 A=A | (HL)
            //  cp   r           Bx         4 z1hc compare A-r
            0xB8..=0xBD => (
                CompareRegister {
                    r: Register::from(low_nibble(op)),
                },
                1,
            ),
            0xBE => (CompareMemoryHL {}, 1),
            0xBF => (CompareRegister { r: Register::A {} }, 1),
            //  cp   n           FE nn      8 z1hc compare A-n
            0xFE => (CompareConstant { n: immediate_u8() }, 2),
            //  cp   (HL)        BE         8 z1hc compare A-(HL)
            //  inc  r           xx         4 z0h- r=r+1
            0x04 => (IncrementRegister { r: Register::B {} }, 1),
            0x0c => (IncrementRegister { r: Register::C {} }, 1),
            0x14 => (IncrementRegister { r: Register::D {} }, 1),
            0x1c => (IncrementRegister { r: Register::E {} }, 1),
            0x24 => (IncrementRegister { r: Register::H {} }, 1),
            0x2c => (IncrementRegister { r: Register::L {} }, 1),
            0x3c => (IncrementRegister { r: Register::A {} }, 1),
            //  inc  (HL)        34        12 z0h- (HL)=(HL)+1
            0x34 => (IncrementMemoryHL {}, 1),
            //  dec  r           xx         4 z1h- r=r-1
            0x05 => (DecrementRegister { r: Register::B {} }, 1),
            0x0d => (DecrementRegister { r: Register::C {} }, 1),
            0x15 => (DecrementRegister { r: Register::D {} }, 1),
            0x1d => (DecrementRegister { r: Register::E {} }, 1),
            0x25 => (DecrementRegister { r: Register::H {} }, 1),
            0x2d => (DecrementRegister { r: Register::L {} }, 1),
            0x3d => (DecrementRegister { r: Register::A {} }, 1),
            //  dec  (HL)        35        12 z1h- (HL)=(HL)-1
            0x35 => (DecrementMemoryHL {}, 1),
            //  daa              27         4 z-0x decimal adjust akku
            0x27 => (DecimalAdjust {}, 1),
            //  cpl              2F         4 -11- A = A xor FF
            0x2F => (CPL {}, 1),

            //GMB 16bit-Arithmetic/logical Commands
            //add  HL,rr     x9           8 -0hc HL = HL+rr     ;rr may be BC,DE,HL,SP
            0x09 => (AddRegisterHL16bit { r: Register16bit::BC {} }, 1),
            0x19 => (AddRegisterHL16bit { r: Register16bit::DE {} }, 1),
            0x29 => (AddRegisterHL16bit { r: Register16bit::HL {} }, 1),
            0x39 => (AddRegisterSPtoHL16bit {}, 1),
            //inc  rr        x3           8 ---- rr = rr+1      ;rr may be BC,DE,HL,SP
            0x03 => (IncrementRegister16bit { r: Register16bit::BC {} }, 1),
            0x13 => (IncrementRegister16bit { r: Register16bit::DE {} }, 1),
            0x23 => (IncrementRegister16bit { r: Register16bit::HL {} }, 1),
            0x33 => (IncrementSP {}, 1),
            //dec  rr        xB           8 ---- rr = rr-1      ;rr may be BC,DE,HL,SP
            0x0B => (DecrementRegister16bit { r: Register16bit::BC {} }, 1),
            0x1B => (DecrementRegister16bit { r: Register16bit::DE {} }, 1),
            0x2B => (DecrementRegister16bit { r: Register16bit::HL {} }, 1),
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
                let op = self.mem.read(self.pc + 1);
                match op {
                    //rlc  r         CB 0x        8 z00c rotate left
                    //rlc  (HL)      CB 06       16 z00c rotate left
                    0x00..=0x05 => (
                        RotateLeftRegisterThroughCarry {
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x06 => (RotateLeftHLThroughCarry {}, 2),
                    0x07 => (RotateLeftRegisterThroughCarry { r: Register::A {} }, 2),
                    //rrc  r         CB 0x        8 z00c rotate right
                    //rrc  (HL)      CB 0E       16 z00c rotate right
                    0x08..=0x0D => (
                        RotateRightRegisterThroughCarry {
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x0E => (RotateRightHLThroughCarry {}, 2),
                    0x0F => (RotateRightRegisterThroughCarry { r: Register::A {} }, 2),
                    //rl   r         CB 1x        8 z00c rotate left through carry
                    //rl   (HL)      CB 16       16 z00c rotate left through carry
                    0x10..=0x15 => (
                        RotateLeftRegister {
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x16 => (RotateLeftHL {}, 2),
                    0x17 => (RotateLeftRegister { r: Register::A {} }, 2),
                    //rr   r         CB 1x        8 z00c rotate right through carry
                    //rr   (HL)      CB 1E       16 z00c rotate right through carry
                    0x18..=0x1D => (
                        RotateRightRegister {
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x1E => (RotateRightHL {}, 2),
                    0x1F => (RotateRightRegister { r: Register::A {} }, 2),
                    //sla  r         CB 2x        8 z00c shift left arithmetic (b0=0)
                    //sla  (HL)      CB 26       16 z00c shift left arithmetic (b0=0)
                    0x20..=0x25 => (
                        ShiftLeftRegister {
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x26 => (ShiftLeftHL {}, 2),
                    0x27 => (ShiftLeftRegister { r: Register::A {} }, 2),
                    //sra  r         CB 2x        8 z00c shift right arithmetic (b7=b7)
                    //sra  (HL)      CB 2E       16 z00c shift right arithmetic (b7=b7)
                    0x28..=0x2D => (
                        ShiftRightArithmeticRegister {
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x2E => (ShiftRightArithmeticHL {}, 2),
                    0x2F => (ShiftRightArithmeticRegister { r: Register::A {} }, 2),
                    //swap r         CB 3x        8 z000 exchange low/hi-nibble
                    //swap (HL)      CB 36       16 z000 exchange low/hi-nibble
                    0x30..=0x35 => (
                        SwapRegister {
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x36 => (SwapHL {}, 2),
                    0x37 => (SwapRegister { r: Register::A {} }, 2),
                    //srl  r         CB 3x        8 z00c shift right logical (b7=0)
                    //srl  (HL)      CB 3E       16 z00c shift right logical (b7=0)
                    0x38..=0x3D => (
                        ShiftRightLogicalRegister {
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x3E => (ShiftRightLogicalHL {}, 2),
                    0x3F => (ShiftRightLogicalRegister { r: Register::A {} }, 2),

                    //GMB Singlebit Operation Commands
                    //bit  n,r       CB xx        8 z01- test bit n
                    //bit  n,(HL)    CB xx       12 z01- test bit n
                    0x40..=0x45 => (
                        TestBitRegister {
                            bit: 0,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x46 => (TestBitMemoryHL { bit: 0 }, 2),
                    0x47 => (
                        TestBitRegister {
                            bit: 0,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0x48..=0x4D => (
                        TestBitRegister {
                            bit: 1,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x4E => (TestBitMemoryHL { bit: 1 }, 2),
                    0x4F => (
                        TestBitRegister {
                            bit: 1,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0x50..=0x55 => (
                        TestBitRegister {
                            bit: 2,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x56 => (TestBitMemoryHL { bit: 2 }, 2),
                    0x57 => (
                        TestBitRegister {
                            bit: 2,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0x58..=0x5D => (
                        TestBitRegister {
                            bit: 3,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x5E => (TestBitMemoryHL { bit: 3 }, 2),
                    0x5F => (
                        TestBitRegister {
                            bit: 3,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0x60..=0x65 => (
                        TestBitRegister {
                            bit: 4,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x66 => (TestBitMemoryHL { bit: 4 }, 2),
                    0x67 => (
                        TestBitRegister {
                            bit: 4,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0x68..=0x6D => (
                        TestBitRegister {
                            bit: 5,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x6E => (TestBitMemoryHL { bit: 5 }, 2),
                    0x6F => (
                        TestBitRegister {
                            bit: 5,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0x70..=0x75 => (
                        TestBitRegister {
                            bit: 6,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x76 => (TestBitMemoryHL { bit: 6 }, 2),
                    0x77 => (
                        TestBitRegister {
                            bit: 6,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0x78..=0x7D => (
                        TestBitRegister {
                            bit: 7,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x7E => (TestBitMemoryHL { bit: 7 }, 2),
                    0x7F => (
                        TestBitRegister {
                            bit: 7,
                            r: Register::A {},
                        },
                        2,
                    ),

                    //res  n,r       CB xx        8 ---- reset bit n
                    //res  n,(HL)    CB xx       16 ---- reset bit n
                    0x80..=0x85 => (
                        ResetBitRegister {
                            bit: 0,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x86 => (ResetBitMemoryHL { bit: 0 }, 2),
                    0x87 => (
                        ResetBitRegister {
                            bit: 0,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0x88..=0x8D => (
                        ResetBitRegister {
                            bit: 1,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x8E => (ResetBitMemoryHL { bit: 1 }, 2),
                    0x8F => (
                        ResetBitRegister {
                            bit: 1,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0x90..=0x95 => (
                        ResetBitRegister {
                            bit: 2,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x96 => (ResetBitMemoryHL { bit: 2 }, 2),
                    0x97 => (
                        ResetBitRegister {
                            bit: 2,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0x98..=0x9D => (
                        ResetBitRegister {
                            bit: 3,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0x9E => (ResetBitMemoryHL { bit: 3 }, 2),
                    0x9F => (
                        ResetBitRegister {
                            bit: 3,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0xA0..=0xA5 => (
                        ResetBitRegister {
                            bit: 4,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0xA6 => (ResetBitMemoryHL { bit: 4 }, 2),
                    0xA7 => (
                        ResetBitRegister {
                            bit: 4,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0xA8..=0xAD => (
                        ResetBitRegister {
                            bit: 5,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0xAE => (ResetBitMemoryHL { bit: 5 }, 2),
                    0xAF => (
                        ResetBitRegister {
                            bit: 5,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0xB0..=0xB5 => (
                        ResetBitRegister {
                            bit: 6,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0xB6 => (ResetBitMemoryHL { bit: 6 }, 2),
                    0xB7 => (
                        ResetBitRegister {
                            bit: 6,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0xB8..=0xBD => (
                        ResetBitRegister {
                            bit: 7,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0xBE => (ResetBitMemoryHL { bit: 7 }, 2),
                    0xBF => (
                        ResetBitRegister {
                            bit: 7,
                            r: Register::A {},
                        },
                        2,
                    ),

                    //set  n,r       CB xx        8 ---- set bit n
                    //set  n,(HL)    CB xx       16 ---- set bit n
                    0xC0..=0xC5 => (
                        SetBitRegister {
                            bit: 0,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0xC6 => (SetBitMemoryHL { bit: 0 }, 2),
                    0xC7 => (
                        SetBitRegister {
                            bit: 0,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0xC8..=0xCD => (
                        SetBitRegister {
                            bit: 1,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0xCE => (SetBitMemoryHL { bit: 1 }, 2),
                    0xCF => (
                        SetBitRegister {
                            bit: 1,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0xD0..=0xD5 => (
                        SetBitRegister {
                            bit: 2,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0xD6 => (SetBitMemoryHL { bit: 2 }, 2),
                    0xD7 => (
                        SetBitRegister {
                            bit: 2,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0xD8..=0xDD => (
                        SetBitRegister {
                            bit: 3,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0xDE => (SetBitMemoryHL { bit: 3 }, 2),
                    0xDF => (
                        SetBitRegister {
                            bit: 3,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0xE0..=0xE5 => (
                        SetBitRegister {
                            bit: 4,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0xE6 => (SetBitMemoryHL { bit: 4 }, 2),
                    0xE7 => (
                        SetBitRegister {
                            bit: 4,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0xE8..=0xED => (
                        SetBitRegister {
                            bit: 5,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0xEE => (SetBitMemoryHL { bit: 5 }, 2),
                    0xEF => (
                        SetBitRegister {
                            bit: 5,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0xF0..=0xF5 => (
                        SetBitRegister {
                            bit: 6,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0xF6 => (SetBitMemoryHL { bit: 6 }, 2),
                    0xF7 => (
                        SetBitRegister {
                            bit: 6,
                            r: Register::A {},
                        },
                        2,
                    ),

                    0xF8..=0xFD => (
                        SetBitRegister {
                            bit: 7,
                            r: Register::from(low_nibble(op)),
                        },
                        2,
                    ),
                    0xFE => (SetBitMemoryHL { bit: 7 }, 2),
                    0xFF => (
                        SetBitRegister {
                            bit: 7,
                            r: Register::A {},
                        },
                        2,
                    ),
                    _ => unreachable!(),
                }
            }
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
            0xC3 => (
                JP {
                    nn: immediate_u16(),
                },
                3,
            ),
            //jp   HL        E9           4 ---- jump to HL, PC=HL
            0xE9 => (JPtoMemoryHL {}, 1),
            //jp   f,nn      xx nn nn 16;12 ---- conditional jump if nz,z,nc,c
            0xC2 => (
                CondJP {
                    flag: Subtract {},
                    nn: immediate_u16(),
                },
                3,
            ),
            0xCA => (
                CondJP {
                    flag: Zero {},
                    nn: immediate_u16(),
                },
                3,
            ),
            0xD2 => (
                CondJP {
                    flag: HalfCarry {},
                    nn: immediate_u16(),
                },
                3,
            ),
            0xDA => (
                CondJP {
                    flag: Carry {},
                    nn: immediate_u16(),
                },
                3,
            ),
            //jr   PC+dd     18 dd       12 ---- relative jump to nn (PC=PC+/-7bit)
            0x18 => (JRtoMemory { dd: immediate_i8() }, 2),
            //jr   f,PC+dd   xx dd     12;8 ---- conditional relative jump if nz,z,nc,c
            // TODO: Duty cycles depend on conditional too!
            0x20 => (
                CondJR {
                    flag: Subtract {},
                    dd: immediate_i8(),
                },
                2,
            ),
            0x28 => (
                CondJR {
                    flag: Zero {},
                    dd: immediate_i8(),
                },
                2,
            ),
            0x30 => (
                CondJR {
                    flag: HalfCarry {},
                    dd: immediate_i8(),
                },
                2,
            ),
            0x38 => (
                CondJR {
                    flag: Carry {},
                    dd: immediate_i8(),
                },
                2,
            ),
            //call nn        CD nn nn    24 ---- call to nn, SP=SP-2, (SP)=PC, PC=nn
            0xCD => (
                CALL {
                    nn: immediate_u16(),
                },
                1,
            ),
            //call f,nn      xx nn nn 24;12 ---- conditional call if nz,z,nc,c
            0xC4 => (
                CondCALL {
                    flag: Subtract {},
                    nn: immediate_u16(),
                },
                3,
            ),
            0xCC => (
                CondCALL {
                    flag: Zero {},
                    nn: immediate_u16(),
                },
                3,
            ),
            0xD4 => (
                CondCALL {
                    flag: HalfCarry {},
                    nn: immediate_u16(),
                },
                3,
            ),
            0xDC => (
                CondCALL {
                    flag: Carry {},
                    nn: immediate_u16(),
                },
                3,
            ),
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
