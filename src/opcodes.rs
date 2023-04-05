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
    pub opcode: u8,
    pub name: String,
    pub bytes: u8, 
    pub cycles: u8,
    pub addressingMode: AddressingMode 
}

impl OpCode {
    pub fn new(opcode: u8, name: &str, bytes: u8, cycles: u8, addressingMode: AddressingMode) -> OpCode {
        OpCode {
            opcode: opcode,
            name: name.to_string(),
            bytes: bytes,
            cycles: cycles, 
            addressingMode: addressingMode
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
}