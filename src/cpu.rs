use bitflags::bitflags;
use nes_emulator::opcodes::{AddressingMode, OpCode, OPCODES_MAP};
use std::collections::HashMap;

bitflags! {
    /// # Status Register (P) http://wiki.nesdev.com/w/index.php/Status_flags
    ///
    ///  7 6 5 4 3 2 1 0
    ///  N V _ B D I Z C
    ///  | |   | | | | +--- Carry Flag
    ///  | |   | | | +----- Zero Flag
    ///  | |   | | +------- Interrupt Disable
    ///  | |   | +--------- Decimal Mode (not used on NES)
    ///  | |   +----------- Break Command
    ///  | +--------------- Overflow Flag
    ///  +----------------- Negative Flag
    ///
    pub struct CpuFlags: u8 {
        const CARRY             = 0b00000001;
        const ZERO              = 0b00000010;
        const INTERRUPT_DISABLE = 0b00000100;
        const DECIMAL_MODE      = 0b00001000;
        const BREAK             = 0b00010000;
        const BREAK2            = 0b00100000;
        const OVERFLOW          = 0b01000000;
        const NEGATIV           = 0b10000000;
    }
}

const STACK: u16 = 0x0100;
const STACK_RESET: u8 = 0xfd;

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: CpuFlags,
    pub program_counter: u16,
    pub stack_ptr: u8,
    memory: [u8; 0xFFFF],
}

trait Mem {
    fn mem_read(&self, addr: u16) -> u8;

    fn mem_write(&mut self, addr: u16, data: u8);

    fn mem_read_u16(&mut self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos + 1) as u16;
        (hi << 8) | (lo as u16)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos + 1, hi);
    }
}

impl Mem for CPU {
    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: CpuFlags::from_bits_truncate(0b100100),
            program_counter: 0,
            stack_ptr: 0,
            memory: [0; 0xFFFF],
        }
    }

    fn set_carry_flag(&mut self) {
        self.status.insert(CpuFlags::CARRY)
    }

    fn clear_carry_flag(&mut self) {
        self.status.remove(CpuFlags::CARRY)
    }

    fn set_overflow_flag(&mut self) {
        self.status.insert(CpuFlags::OVERFLOW)
    }

    fn clear_overflow_flag(&mut self) {
        self.status.remove(CpuFlags::OVERFLOW)
    }

    fn get_operand_address(&mut self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,

            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,

            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),

            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }

            AddressingMode::Absolute_X => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_x as u16);
                addr
            }
            AddressingMode::Absolute_Y => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_y as u16);
                addr
            }

            AddressingMode::Indirect_X => {
                let base = self.mem_read(self.program_counter);

                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);

                let lo = self.mem_read(base as u16);
                let hi = self.mem_read((base as u8).wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                deref
            }

            AddressingMode::Accumulator => {
                panic!("mode {:?} should be its own function", mode);
            }

            AddressingMode::NoneAddressing => {
                panic!("mode {:?} is not supported", mode);
            }
        }
    }

    fn set_register_a(&mut self, value: u8) {
        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            self.status.insert(CpuFlags::ZERO);
        } else {
            self.status.remove(CpuFlags::ZERO);
        }

        if result & 0b1000_0000 != 0 {
            self.status.insert(CpuFlags::NEGATIV);
        } else {
            self.status.remove(CpuFlags::NEGATIV);
        }
    }

    /// note: ignoring decimal mode
    /// http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
    fn add_to_register_a(&mut self, data: u8) {
        let sum = self.register_a as u16
            + data as u16
            + (if self.status.contains(CpuFlags::CARRY) {
                1
            } else {
                0
            }) as u16;

        let carry = sum > 0xff;

        if carry {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }

        let result = sum as u8;

        if (data ^ result) & (result ^ self.register_a) & 0x80 != 0 {
            self.status.insert(CpuFlags::OVERFLOW);
        } else {
            self.status.remove(CpuFlags::OVERFLOW)
        }

        self.set_register_a(result);
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.add_to_register_a(value);
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(&mode);
        let data = self.mem_read(addr);
        self.add_to_register_a(((data as i8).wrapping_neg().wrapping_sub(1)) as u8);
        // -A = !A + 1
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(&mode);
        let data = self.mem_read(addr);
        self.set_register_a(self.register_a & data);
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }

    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
    }

    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut value = self.mem_read(addr);
        value = value.wrapping_add(1);
        self.mem_write(addr, value);
        self.update_zero_and_negative_flags(value);
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut value = self.mem_read(addr);
        value = value.wrapping_sub(1);
        self.mem_write(addr, value);
        self.update_zero_and_negative_flags(value);
    }

    fn asl_accumulator(&mut self) {
        let data = self.register_a;
        if data >> 7 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        self.set_register_a(data << 1);
    }

    fn asl(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(&mode);
        let data = self.mem_read(addr);
        if data >> 7 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        self.set_register_a(data << 1);
    }

    fn lsr_accumulator(&mut self) {
        let data = self.register_a;
        if data & 0x00000001 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        self.set_register_a(data >> 1);
    }

    fn lsr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(&mode);
        let data = self.mem_read(addr);
        if data & 0x00000001 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        self.set_register_a(data >> 1);
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(&mode);
        let data = self.mem_read(addr);
        let data = data & self.register_a;
        self.update_zero_and_negative_flags(data);

        if data & 0b0100_0000 != 0 {
            self.set_overflow_flag();
        } else {
            self.clear_overflow_flag();
        }
    }

    fn pla(&mut self) {
        let val = self.stack_pop();
        self.set_register_a(val);
    }

    fn txs(&mut self) {
        self.stack_ptr = self.register_x;
    }

    fn tsx(&mut self) {
        self.register_x = self.stack_ptr;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn plp(&mut self) {
        *self.status.0.bits_mut() = self.stack_pop();
        self.status.remove(CpuFlags::BREAK);
        self.status.insert(CpuFlags::BREAK2);
    }

    fn php(&mut self) {
        //http://wiki.nesdev.com/w/index.php/CPU_status_flag_behavior
        let mut flags = CpuFlags::from_bits_truncate(self.status.bits());
        flags.insert(CpuFlags::BREAK);
        flags.insert(CpuFlags::BREAK2);
        self.stack_push(flags.bits());
    }

    fn rts(&mut self) {
        let addr = self.stack_pop_u16();
        self.program_counter = addr + 1;
    }

    fn jsr(&mut self) {
        self.stack_push_u16(self.program_counter + 2 - 1);
        self.program_counter = self.mem_read_u16(self.program_counter);
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.set_register_a(data ^ self.register_a);
    }

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let data = self.mem_read(addr);
        self.set_register_a(data | self.register_a);
    }

    fn rol(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);
        let old_carry = self.status.contains(CpuFlags::CARRY);

        if data >> 7 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        data = data << 1;
        if old_carry {
            data = data | 1;
        }
        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
        data
    }

    fn rol_accumulator(&mut self) {
        let mut data = self.register_a;
        let old_carry = self.status.contains(CpuFlags::CARRY);

        if data >> 7 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        data = data << 1;
        if old_carry {
            data = data | 1;
        }
        self.set_register_a(data);
    }

    fn ror(&mut self, mode: &AddressingMode) -> u8 {
        let addr = self.get_operand_address(mode);
        let mut data = self.mem_read(addr);
        let old_carry = self.status.contains(CpuFlags::CARRY);

        if data & 1 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        data = data >> 1;
        if old_carry {
            data = data | 0b10000000;
        }
        self.mem_write(addr, data);
        self.update_zero_and_negative_flags(data);
        data
    }

    fn ror_accumulator(&mut self) {
        let mut data = self.register_a;
        let old_carry = self.status.contains(CpuFlags::CARRY);

        if data & 1 == 1 {
            self.set_carry_flag();
        } else {
            self.clear_carry_flag();
        }
        data = data >> 1;
        if old_carry {
            data = data | 0b10000000;
        }
        self.set_register_a(data);
    }

    // Branches are relative.
    // https://wiki.cdot.senecacollege.ca/wiki/6502_Jumps,_Branches,_and_Procedures
    fn branch(&mut self, condition: bool) {
        if condition {
            let jump: i8 = self.mem_read(self.program_counter) as i8; // address
            let jump_addr = self
                .program_counter
                .wrapping_add(1)
                .wrapping_add(jump as u16);

            self.program_counter = jump_addr;
        }
    }

    fn compare(&mut self, mode: &AddressingMode, compare_with: u8) {
        let addr = self.get_operand_address(&mode);
        let data = self.mem_read(addr);

        if data <= compare_with {
            self.status.insert(CpuFlags::CARRY);
        } else {
            self.status.remove(CpuFlags::CARRY);
        }

        self.update_zero_and_negative_flags(compare_with.wrapping_sub(data));
    }

    fn jmp_absolute(&mut self) {
        let addr = self.mem_read_u16(self.program_counter); // get next 16 bytes from code
        self.program_counter = addr;
    }

    fn jmp_indirect(&mut self) {
        let mem_address = self.mem_read_u16(self.program_counter);

        // I think this allows indirect ref to read memory across page boundaries
        // ngl I cheated and copied this bit
        let indirect_ref = if mem_address & 0x00FF == 0x00FF {
            let lo = self.mem_read(mem_address);
            let hi = self.mem_read(mem_address & 0xFF00);
            (hi as u16) << 8 | (lo as u16)
        } else {
            self.mem_read_u16(mem_address)
        };

        self.program_counter = indirect_ref;
    }

    fn stack_pop(&mut self) -> u8 {
        self.stack_ptr = self.stack_ptr.wrapping_add(1);
        self.mem_read((STACK as u16) + self.stack_ptr as u16)
    }

    fn stack_push(&mut self, data: u8) {
        self.mem_write((STACK as u16) + self.stack_ptr as u16, data);
        self.stack_ptr = self.stack_ptr.wrapping_sub(1);
    }

    fn stack_pop_u16(&mut self) -> u16 {
        let lo = self.stack_pop() as u16;
        let hi = self.stack_pop() as u16;

        hi << 8 | lo
    }

    fn stack_push_u16(&mut self, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.stack_push(hi);
        self.stack_push(lo);
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status = CpuFlags::from_bits_truncate(0b100100);
        self.stack_ptr = STACK_RESET;
        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run()
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F) where F: FnMut(&mut CPU) {
        let ref opcodes: HashMap<u8, &'static OpCode> = *OPCODES_MAP;

        loop {
            callback(self);

            let code: u8 = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let program_counter_state = self.program_counter;

            // Get opcode from hashmap
            let opcode = opcodes
                .get(&code)
                .expect(&format!("Opcode {:x} is not recognized", code));

            match code {
                /* LDA */
                0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => {
                    self.lda(&opcode.mode);
                }
                /* LDX */
                0xa2 | 0xa6 | 0xb6 | 0xae | 0xbe => {
                    self.ldx(&opcode.mode);
                }
                /* LDY */
                0xa0 | 0xa4 | 0xb4 | 0xac | 0xbc => {
                    self.ldy(&opcode.mode);
                }
                /* STA */
                0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => {
                    self.sta(&opcode.mode);
                }
                /* STX */
                0x86 | 0x96 | 0x8e => {
                    self.stx(&opcode.mode);
                }
                /* STY */
                0x84 | 0x94 | 0x8c => {
                    self.sty(&opcode.mode);
                }
                /* ADC */
                0x69 | 0x65 | 0x75 | 0x6d | 0x7d | 0x79 | 0x61 | 0x71 => {
                    self.adc(&opcode.mode);
                }
                /* SBC */
                0xe9 | 0xe5 | 0xf5 | 0xed | 0xfd | 0xf9 | 0xe1 | 0xf1 => {
                    self.sbc(&opcode.mode);
                }
                /* AND */
                0x29 | 0x25 | 0x35 | 0x2d | 0x3d | 0x39 | 0x21 | 0x31 => {
                    self.and(&opcode.mode);
                }
                /* ASL */
                0x0a => {
                    self.asl_accumulator();
                }
                0x06 | 0x16 | 0x0e | 0x1e => {
                    self.asl(&opcode.mode);
                }
                /* BIT */
                0x24 | 0x2c => {
                    self.bit(&opcode.mode);
                }
                /* BRANCHING */
                0x10 => {
                    // BPL
                    self.branch(!self.status.contains(CpuFlags::NEGATIV));
                }
                0x30 => {
                    // BMI
                    self.branch(self.status.contains(CpuFlags::NEGATIV));
                }
                0x50 => {
                    // BVC
                    self.branch(!self.status.contains(CpuFlags::OVERFLOW));
                }
                0x70 => {
                    // BVS
                    self.branch(self.status.contains(CpuFlags::OVERFLOW));
                }
                0x90 => {
                    // BCC
                    self.branch(!self.status.contains(CpuFlags::CARRY));
                }
                0xb0 => {
                    // BCS
                    self.branch(self.status.contains(CpuFlags::CARRY));
                }
                0xd0 => {
                    // BNE
                    self.branch(!self.status.contains(CpuFlags::ZERO));
                }
                0xf0 => {
                    // BEQ
                    self.branch(self.status.contains(CpuFlags::ZERO));
                }
                /* REGISTER INSTRUCTIONS */
                // X
                0xAA => self.tax(),
                0x8a => self.txa(),
                0xca => self.dex(),
                0xe8 => self.inx(),
                // Y
                0xa8 => self.tay(),
                0x98 => self.tya(),
                0x88 => self.dey(),
                0xc8 => self.iny(),
                /* COMPARISON */
                /* CMP */
                0xc9 | 0xc5 | 0xd5 | 0xcd | 0xdd | 0xd9 | 0xc1 | 0xd1 => {
                    self.compare(&opcode.mode, self.register_a);
                }
                /* CPX */
                0xe0 | 0xe4 | 0xec => {
                    self.compare(&opcode.mode, self.register_x);
                }
                /* CPY */
                0xc0 | 0xc4 | 0xcc => {
                    self.compare(&opcode.mode, self.register_y);
                }
                /* INC */
                0xe6 | 0xf6 | 0xee | 0xfe => {
                    self.inc(&opcode.mode);
                }
                /* DEC */
                0xc6 | 0xd6 | 0xce | 0xde => {
                    self.dec(&opcode.mode);
                }

                0x18 => self.clear_carry_flag(), // CLC
                0x38 => self.set_carry_flag(),   // SEC
                0x58 => self.status.remove(CpuFlags::INTERRUPT_DISABLE), // CLI
                0x78 => self.status.insert(CpuFlags::INTERRUPT_DISABLE), // SEI
                0xb8 => self.status.remove(CpuFlags::OVERFLOW), // CLV

                0x9a => self.txs(), // txs
                0xba => self.tsx(), // tsx

                0x48 => self.stack_push(self.register_a), // PHA
                0x68 => self.pla(),                       // PLA

                0x08 => self.php(), // php
                0x28 => self.plp(), // plp

                0x4c => self.jmp_absolute(), // JMP abs
                0x6c => self.jmp_indirect(), // JMP indirect

                0x20 => self.jsr(),
                0x60 => self.rts(),

                /* RTI */
                0x40 => {
                    *self.status.0.bits_mut() = self.stack_pop();
                    self.status.remove(CpuFlags::BREAK);
                    self.status.insert(CpuFlags::BREAK2);
                    self.program_counter = self.stack_pop_u16();
                }

                /* NOP */
                0xea => {
                    //do nothing
                }

                /* LSR */
                0x4a => {
                    self.lsr_accumulator();
                }
                0x46 | 0x56 | 0x4e | 0x5e => {
                    self.lsr(&opcode.mode);
                }

                /*ROL*/ 0x2a => self.rol_accumulator(),

                /* ROL */
                0x26 | 0x36 | 0x2e | 0x3e => {
                    self.rol(&opcode.mode);
                }

                /* ROR */ 0x6a => self.ror_accumulator(),

                /* ROR */
                0x66 | 0x76 | 0x6e | 0x7e => {
                    self.ror(&opcode.mode);
                }

                /* EOR */
                0x49 | 0x45 | 0x55 | 0x4d | 0x5d | 0x59 | 0x41 | 0x51 => {
                    self.eor(&opcode.mode);
                }

                /* ORA */
                0x09 | 0x05 | 0x15 | 0x0d | 0x1d | 0x19 | 0x01 | 0x11 => {
                    self.ora(&opcode.mode);
                }

                0x00 => return,
                _ => todo!(),
            }

            if program_counter_state == self.program_counter {
                self.program_counter += (opcode.len - 1) as u16;
            }
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 5);
        assert!(cpu.status.bits() & 0b0000_0010 == 0);
        assert!(cpu.status.bits() & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xa9_lda_negative_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xff, 0x00]);
        assert!(cpu.status.bits() & 0b1000_0000 == 0b1000_0000);
    }

    /* LDX */
    #[test]
    fn test_0xa2_ldx_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa2, 0x05, 0x00]);
        assert_eq!(cpu.register_x, 5);
        assert!(cpu.status.bits() & 0b0000_0010 == 0);
        assert!(cpu.status.bits() & 0b1000_0000 == 0);
    }

    /* LDY */
    #[test]
    fn test_0xa0_ldy_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa0, 0x05, 0x00]);
        assert_eq!(cpu.register_y, 5);
        assert!(cpu.status.bits() & 0b0000_0010 == 0);
        assert!(cpu.status.bits() & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.register_a = 10;
        cpu.load(vec![0xaa, 0x00]);
        cpu.program_counter = cpu.mem_read_u16(0xFFFC);
        cpu.run();

        assert_eq!(cpu.register_x, 10)
    }

    #[test]
    fn test_0xaa_tay_move_a_to_y() {
        let mut cpu = CPU::new();
        cpu.register_a = 10;
        cpu.load(vec![0xa8, 0x00]);
        cpu.program_counter = cpu.mem_read_u16(0xFFFC);
        cpu.run();

        assert_eq!(cpu.register_y, 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.register_x = 0xff;
        cpu.load(vec![0xe8, 0xe8, 0x00]);
        cpu.program_counter = cpu.mem_read_u16(0xFFFC);
        cpu.run();

        assert_eq!(cpu.register_x, 1)
    }

    #[test]
    fn test_lda_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.register_a, 0x55);
    }

    #[test]
    fn test_lda_immediate() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x50]); // load 80 immediate
        assert_eq!(cpu.register_a, 0x50);
    }

    /* ADC */
    #[test]
    fn test_adc_immediate() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x01, 0x69, 0x01]); // load 80 into a, ADC #$80 into a

        assert_eq!(cpu.register_a, 0x02);
        assert!(cpu.status.bits() & 0b0000_0001 == 0b0000_0000); // no carry
        assert!(cpu.status.bits() & 0b0100_0000 == 0b0000_0000); // no overflow
    }

    #[test]
    fn test_adc_immediate_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x50, 0x69, 0x50]); // load 80 into a, ADC #$80 into a

        assert_eq!(cpu.register_a, 0xa0);
        assert!(cpu.status.bits() & 0b0000_0001 == 0b0000_0000); // no carry
        assert!(cpu.status.bits() & 0b0100_0000 == 0b0100_0000); // overflow
    }

    #[test]
    fn test_adc_immediate_carry() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xff, 0x69, 0x01]); // load 255 into a, ADC #$01 into a

        assert_eq!(cpu.register_a, 0x00);
        assert!(cpu.status.bits() & 0b0000_0001 == 0b0000_0001); // carry
        assert!(cpu.status.bits() & 0b0100_0000 == 0b0000_0000); // no overflow
    }

    #[test]
    fn test_adc_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x03);
        cpu.load_and_run(vec![0xa9, 0x33, 0x65, 0x10]); // load 0x33 into a, ADC 0x03 into a

        assert_eq!(cpu.register_a, 0x36);
        assert!(cpu.status.bits() & 0b0000_0001 == 0b0000_0000); // no carry
        assert!(cpu.status.bits() & 0b0100_0000 == 0b0000_0000); // no overflow
    }

    /* SBC */
    #[test]
    fn test_sbc_immediate() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0xe9, 0x01]); // load 02 into a, SBC #$01 into a

        assert_eq!(cpu.register_a, 0x03);
        assert!(cpu.status.bits() & 0b0000_0001 == 0b0000_0001); // carry
        assert!(cpu.status.bits() & 0b0100_0000 == 0b0000_0000); // no overflow
    }

    #[test]
    fn test_sbc_immediate_overflow() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0xe9, 0x06]); // load 02 into a, SBC #$01 into a

        assert_eq!(cpu.register_a, 0xfe);
        assert!(cpu.status.bits() & 0b0000_0001 == 0b0000_0000); // no carry
        assert!(cpu.status.bits() & 0b0100_0000 == 0b0000_0000); // no overflow
    }

    #[test]
    fn test_sbc_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x06);
        cpu.load_and_run(vec![0xa9, 0x05, 0xe5, 0x10]); // load 0x05 into a, SBC 0x06 into a

        assert_eq!(cpu.register_a, 0xfe);
        assert!(cpu.status.bits() & 0b0000_0001 == 0b0000_0000); // no carry
        assert!(cpu.status.bits() & 0b0100_0000 == 0b0000_0000); // no overflow
    }

    /* AND */
    #[test]
    fn test_and() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x29, 0x0c]); // 0b0101 & 0b1100 == 0b0100

        assert_eq!(cpu.register_a, 0x04);
    }

    /* ASL */
    #[test]
    fn test_asl_accumulator() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x04, 0x0a]); // 0b0100 => 0b1000

        assert_eq!(cpu.register_a, 0x08);
    }

    #[test]
    fn test_asl() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x04);
        cpu.load_and_run(vec![0x06, 0x10]); // 0b0100 => 0b1000

        assert_eq!(cpu.register_a, 0x08);
    }

    /* BIT */
    #[test]
    fn test_bit() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0xc1);
        cpu.load_and_run(vec![0xa9, 0xc1, 0x24, 0x10]); // 0b0100 => 0b1000

        assert_eq!(cpu.register_a, 0xc1);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b0000_0000); // zero
        assert!(cpu.status.bits() & 0b0100_0000 == 0b0100_0000); // overflow
        assert!(cpu.status.bits() & 0b1000_0000 == 0b1000_0000); // negative
    }

    /* BRANCHING */
    #[test]
    fn test_bpl() {
        let mut cpu = CPU::new();
        cpu.status = CpuFlags::from_bits_truncate(0b1111_1111);
        cpu.status.remove(CpuFlags::NEGATIV);
        cpu.load(vec![0x10, 0x10]);
        cpu.program_counter = cpu.mem_read_u16(0xFFFC);
        cpu.run();

        assert_eq!(cpu.program_counter, 0x8013);
    }

    #[test]
    fn test_bmi() {
        let mut cpu = CPU::new();
        cpu.status.insert(CpuFlags::NEGATIV);
        cpu.load(vec![0x30, 0x10]);
        cpu.program_counter = cpu.mem_read_u16(0xFFFC);
        cpu.run();

        assert_eq!(cpu.program_counter, 0x8013);
    }

    #[test]
    fn test_bvc() {
        let mut cpu = CPU::new();
        cpu.status = CpuFlags::from_bits_truncate(0b1111_1111);
        cpu.status.remove(CpuFlags::OVERFLOW);
        cpu.load(vec![0x50, 0x10]);
        cpu.program_counter = cpu.mem_read_u16(0xFFFC);
        cpu.run();

        assert_eq!(cpu.program_counter, 0x8013);
    }

    #[test]
    fn test_bvs() {
        let mut cpu = CPU::new();
        cpu.status.insert(CpuFlags::OVERFLOW);
        cpu.load(vec![0x70, 0x10]);
        cpu.program_counter = cpu.mem_read_u16(0xFFFC);
        cpu.run();

        assert_eq!(cpu.program_counter, 0x8013);
    }

    #[test]
    fn test_bcc() {
        let mut cpu = CPU::new();
        cpu.status = CpuFlags::from_bits_truncate(0b1111_1111);
        cpu.status.remove(CpuFlags::CARRY);
        cpu.load(vec![0x90, 0x10]);
        cpu.program_counter = cpu.mem_read_u16(0xFFFC);
        cpu.run();

        assert_eq!(cpu.program_counter, 0x8013);
    }

    #[test]
    fn test_bcs() {
        let mut cpu = CPU::new();
        cpu.status.insert(CpuFlags::CARRY);
        cpu.load(vec![0xb0, 0x10]);
        cpu.program_counter = cpu.mem_read_u16(0xFFFC);
        cpu.run();

        assert_eq!(cpu.program_counter, 0x8013);
    }

    #[test]
    fn test_bne() {
        let mut cpu = CPU::new();
        cpu.status = CpuFlags::from_bits_truncate(0b1111_1111);
        cpu.status.remove(CpuFlags::ZERO);
        cpu.load(vec![0xd0, 0x10]);
        cpu.program_counter = cpu.mem_read_u16(0xFFFC);
        cpu.run();

        assert_eq!(cpu.program_counter, 0x8013);
    }

    #[test]
    fn test_beq() {
        let mut cpu = CPU::new();
        cpu.status.insert(CpuFlags::ZERO);
        cpu.load(vec![0xf0, 0x10]);
        cpu.program_counter = cpu.mem_read_u16(0xFFFC);
        cpu.run();

        assert_eq!(cpu.program_counter, 0x8013);
    }

    #[test]
    fn test_sta() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x10, 0x85, 0x00]);
        assert_eq!(cpu.mem_read(0x00), 0x10);
    }

    #[test]
    fn test_stx() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa2, 0x10, 0x86, 0x00]);
        assert_eq!(cpu.mem_read(0x00), 0x10);
    }

    #[test]
    fn test_sty() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa0, 0x10, 0x84, 0x00]);
        assert_eq!(cpu.mem_read(0x00), 0x10);
    }

    #[test]
    fn test_cmp_immediate_eq() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x10, 0xc9, 0x10]);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b0000_0010); // zero
        assert!(cpu.status.bits() & 0b0000_0001 == 0b0000_0001); // carry
        assert!(cpu.status.bits() & 0b1000_0000 == 0b0000_0000); // negative
    }

    #[test]
    fn test_cmp_immediate_neq() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x10, 0xc9, 0x00]);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b0000_0000); // zero
        assert!(cpu.status.bits() & 0b0000_0001 == 0b0000_0001); // carry
        assert!(cpu.status.bits() & 0b1000_0000 == 0b0000_0000); // negative
    }

    #[test]
    fn test_cmp_immediate_eq_neg() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x10, 0xc9, 0x11]);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b0000_0000); // zero
        assert!(cpu.status.bits() & 0b0000_0001 == 0b0000_0000); // carry
        assert!(cpu.status.bits() & 0b1000_0000 == 0b1000_0000); // negative
    }

    #[test]
    fn test_cpx_immediate_eq() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa2, 0x10, 0xe0, 0x10]);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b0000_0010); // zero
        assert!(cpu.status.bits() & 0b0000_0001 == 0b0000_0001); // carry
        assert!(cpu.status.bits() & 0b1000_0000 == 0b0000_0000); // negative
    }

    #[test]
    fn test_cpx_immediate_neq() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa2, 0x10, 0xe0, 0x00]);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b0000_0000); // zero
        assert!(cpu.status.bits() & 0b0000_0001 == 0b0000_0001); // carry
        assert!(cpu.status.bits() & 0b1000_0000 == 0b0000_0000); // negative
    }

    #[test]
    fn test_cpy_immediate_eq() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa0, 0x10, 0xc0, 0x10]);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b0000_0010); // zero
        assert!(cpu.status.bits() & 0b0000_0001 == 0b0000_0001); // carry
        assert!(cpu.status.bits() & 0b1000_0000 == 0b0000_0000); // negative
    }

    #[test]
    fn test_cpy_immediate_neq() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa0, 0x10, 0xc0, 0x00]);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b0000_0000); // zero
        assert!(cpu.status.bits() & 0b0000_0001 == 0b0000_0001); // carry
        assert!(cpu.status.bits() & 0b1000_0000 == 0b0000_0000); // negative
    }

    /* INC */
    #[test]
    fn test_inc_pos() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x01);
        cpu.load(vec![0xe6, 0x10]);
        cpu.program_counter = cpu.mem_read_u16(0xFFFC);
        cpu.run();

        assert_eq!(cpu.mem_read(0x10), 0x02);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b0000_0000); // zero
        assert!(cpu.status.bits() & 0b1000_0000 == 0b0000_0000); // negative
    }

    #[test]
    fn test_inc_neg() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0xc1);
        cpu.load(vec![0xe6, 0x10]);
        cpu.program_counter = cpu.mem_read_u16(0xFFFC);
        cpu.run();

        assert_eq!(cpu.mem_read(0x10), 0xc2);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b0000_0000); // zero
        assert!(cpu.status.bits() & 0b1000_0000 == 0b1000_0000); // negative
    }

    /* DEC */
    #[test]
    fn test_dec_pos() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x01);
        cpu.load(vec![0xc6, 0x10]);
        cpu.program_counter = cpu.mem_read_u16(0xFFFC);
        cpu.run();

        assert_eq!(cpu.mem_read(0x10), 0x00);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b0000_0010); // zero
        assert!(cpu.status.bits() & 0b0000_0000 == 0b0000_0000); // negative
    }

    #[test]
    fn test_dec_neg() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x00);
        cpu.load(vec![0xc6, 0x10]);
        cpu.program_counter = cpu.mem_read_u16(0xFFFC);
        cpu.run();

        assert_eq!(cpu.mem_read(0x10), 0xff);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b0000_0000); // zero
        assert!(cpu.status.bits() & 0b1000_0000 == 0b1000_0000); // negative
    }
}
