use std::collections::HashMap;

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
   Immediate,
   ZeroPage,
   ZeroPage_X,
   ZeroPage_Y,
   Absolute,
   Absolute_X,
   Absolute_Y,
   Indirect_X,
   Indirect_Y,
   NoneAddressing,
}

pub struct OpCode {
    pub code: u8,
    pub mnemonic: &'static str,
    pub len: u8, 
    pub cycles: u8,
    pub mode: AddressingMode 
}

impl OpCode {
    pub fn new(code: u8, mnemonic: &'static str, len: u8, cycles: u8, mode: AddressingMode) -> OpCode {
        OpCode {
            code: code,
            mnemonic: mnemonic, 
            len: len,
            cycles: cycles, 
            mode: mode
        }
    }
}

use lazy_static::lazy_static;

// Lazy static can allocate statics to heap @ runtime
lazy_static! {
    pub static ref CPU_OPS_CODES: Vec<OpCode> = vec![
        OpCode::new(0x00, "BRK", 1, 7, AddressingMode::NoneAddressing),
        OpCode::new(0xAA, "TAX", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0xe8, "INX", 1, 2, AddressingMode::NoneAddressing),

        /* LDA */
        OpCode::new(0xa9, "LDA", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xa5, "LDA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xb5, "LDA", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xad, "LDA", 2, 4 /* +1 if page crossed */, AddressingMode::Absolute),
        OpCode::new(0xbd, "LDA", 3, 4 /* +1 if page crossed */, AddressingMode::Absolute_X),
        OpCode::new(0xa1, "LDA", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0xb1, "LDA", 2, 5 /* +1 if page crossed */, AddressingMode::Indirect_Y),

        /* STA */
        OpCode::new(0x85, "STA", 2, 3, AddressingMode::Immediate),
        OpCode::new(0x95, "STA", 2, 4, AddressingMode::ZeroPage),
        OpCode::new(0x8d, "STA", 3, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x9d, "STA", 3, 5, AddressingMode::Absolute),
        OpCode::new(0x99, "STA", 3, 5, AddressingMode::Absolute_X),
        OpCode::new(0x81, "STA", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x91, "STA", 2, 6, AddressingMode::Indirect_Y),

    ];

    pub static ref OPCODES_MAP: HashMap<u8, &'static OpCode> = {
        let mut map = HashMap::new();
        for cpuop in &*CPU_OPS_CODES {
            map.insert(cpuop.code, cpuop);
        }
        map
    };
}