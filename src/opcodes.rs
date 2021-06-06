use crate::cpu::AddressingMode;
use std::collections::HashMap;

pub struct OpCode {
    pub code: u8,
    pub mnemonic: &'static str,
    pub cycles: u8,
    pub mode: AddressingMode,
}

impl OpCode {
    pub fn new(code: u8, mnemonic: &'static str, cycles: u8, mode: AddressingMode) -> Self {
        OpCode {
            code: code,
            mnemonic: mnemonic,
            cycles: cycles,
            mode: mode,
        }
    }
}

lazy_static! {
    pub static ref CPU_OPS_CODES: Vec<OpCode> = vec![
        OpCode::new(0x00, "BRK", 1, AddressingMode::NoneAddressing),
        // INC
        OpCode::new(0xe6, "INC", 5, AddressingMode::ZeroPage),
        OpCode::new(0xf6, "INC", 6, AddressingMode::ZeroPageX),
        OpCode::new(0xee, "INC", 6, AddressingMode::Absolute),
        OpCode::new(0xfe, "INC", 7, AddressingMode::AbsoluteX),
        //
        OpCode::new(0xe8, "INX", 2, AddressingMode::NoneAddressing),
        OpCode::new(0xc8, "INY", 2, AddressingMode::NoneAddressing),
        // LDA
        OpCode::new(0xa9, "LDA", 2, AddressingMode::Immediate),
        OpCode::new(0xa5, "LDA", 3, AddressingMode::ZeroPage),
        OpCode::new(0xb5, "LDA", 4, AddressingMode::ZeroPageX),
        OpCode::new(0xad, "LDA", 4, AddressingMode::Absolute),
        OpCode::new(0xbd, "LDA", 4, AddressingMode::AbsoluteX),
        OpCode::new(0xb9, "LDA", 4, AddressingMode::AbsoluteY),
        OpCode::new(0xa1, "LDA", 6, AddressingMode::IndirectX),
        OpCode::new(0xb1, "LDA", 5, AddressingMode::IndirectY),
        // LDX
        OpCode::new(0xa2, "LDX", 2, AddressingMode::Immediate),
        OpCode::new(0xa6, "LDX", 3, AddressingMode::ZeroPage),
        OpCode::new(0xb6, "LDX", 4, AddressingMode::ZeroPageY),
        OpCode::new(0xae, "LDX", 4, AddressingMode::Absolute),
        OpCode::new(0xbe, "LDX", 4, AddressingMode::AbsoluteY),
        // LDY
        OpCode::new(0xa0, "LDY", 2, AddressingMode::Immediate),
        OpCode::new(0xa4, "LDY", 3, AddressingMode::ZeroPage),
        OpCode::new(0xb4, "LDY", 4, AddressingMode::ZeroPageX),
        OpCode::new(0xac, "LDY", 4, AddressingMode::Absolute),
        OpCode::new(0xbc, "LDY", 4, AddressingMode::AbsoluteX),
        // STA
        OpCode::new(0x85, "STA", 3, AddressingMode::ZeroPage),
        OpCode::new(0x95, "STA", 4, AddressingMode::ZeroPageX),
        OpCode::new(0x8d, "STA", 4, AddressingMode::Absolute),
        OpCode::new(0x9d, "STA", 5, AddressingMode::AbsoluteX),
        OpCode::new(0x99, "STA", 5, AddressingMode::AbsoluteY),
        OpCode::new(0x81, "STA", 6, AddressingMode::IndirectX),
        OpCode::new(0x91, "STA", 6, AddressingMode::IndirectY),
        // STX
        OpCode::new(0x86, "STX", 3, AddressingMode::ZeroPage),
        OpCode::new(0x96, "STX", 4, AddressingMode::ZeroPageY),
        OpCode::new(0x8e, "STX", 4, AddressingMode::Absolute),
        // STY
        OpCode::new(0x84, "STY", 3, AddressingMode::ZeroPage),
        OpCode::new(0x94, "STY", 4, AddressingMode::ZeroPageX),
        OpCode::new(0x8c, "STY", 4, AddressingMode::Absolute),
        //
        OpCode::new(0xaa, "TAX", 2, AddressingMode::NoneAddressing),
        OpCode::new(0xa8, "TAY", 2, AddressingMode::NoneAddressing),
        OpCode::new(0xba, "TSX", 2, AddressingMode::NoneAddressing),
        OpCode::new(0x8a, "TXA", 2, AddressingMode::NoneAddressing),
        OpCode::new(0x9a, "TXS", 2, AddressingMode::NoneAddressing),
        OpCode::new(0x98, "TYA", 2, AddressingMode::NoneAddressing),
    ];
    pub static ref OPCODES_MAP: HashMap<u8, &'static OpCode> = {
        let mut map = HashMap::new();
        for opcode in &*CPU_OPS_CODES {
            map.insert(opcode.code, opcode);
        }
        map
    };
}
