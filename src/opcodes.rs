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
        // ADC
        OpCode::new(0x69, "ADC", 2, AddressingMode::Immediate),
        OpCode::new(0x65, "ADC", 3, AddressingMode::ZeroPage),
        OpCode::new(0x75, "ADC", 4, AddressingMode::ZeroPageX),
        OpCode::new(0x6d, "ADC", 4, AddressingMode::Absolute),
        OpCode::new(0x7d, "ADC", 4, AddressingMode::AbsoluteX),
        OpCode::new(0x79, "ADC", 4, AddressingMode::AbsoluteY),
        OpCode::new(0x61, "ADC", 6, AddressingMode::IndirectX),
        OpCode::new(0x71, "ADC", 5, AddressingMode::IndirectY),
        // AND
        OpCode::new(0x29, "AND", 2, AddressingMode::Immediate),
        OpCode::new(0x25, "AND", 3, AddressingMode::ZeroPage),
        OpCode::new(0x35, "AND", 4, AddressingMode::ZeroPageX),
        OpCode::new(0x2d, "AND", 4, AddressingMode::Absolute),
        OpCode::new(0x3d, "AND", 4, AddressingMode::AbsoluteX),
        OpCode::new(0x39, "AND", 4, AddressingMode::AbsoluteY),
        OpCode::new(0x21, "AND", 6, AddressingMode::IndirectX),
        OpCode::new(0x31, "AND", 5, AddressingMode::IndirectY),
        // ASL
        OpCode::new(0x0a, "ASL", 2, AddressingMode::Accumulator),
        OpCode::new(0x06, "ASL", 5, AddressingMode::ZeroPage),
        OpCode::new(0x16, "ASL", 6, AddressingMode::ZeroPageX),
        OpCode::new(0x0e, "ASL", 6, AddressingMode::Absolute),
        OpCode::new(0x1e, "ASL", 7, AddressingMode::AbsoluteX),
        // Branches
        OpCode::new(0x90, "BCC", 2, AddressingMode::Relative),
        OpCode::new(0xb0, "BCS", 2, AddressingMode::Relative),
        OpCode::new(0xf0, "BEQ", 2, AddressingMode::Relative),
        // BIT
        OpCode::new(0x24, "BIT", 3, AddressingMode::ZeroPage),
        OpCode::new(0x2c, "BIT", 4, AddressingMode::Absolute),
        // Branches
        OpCode::new(0x30, "BMI", 2, AddressingMode::Relative),
        OpCode::new(0xd0, "BNE", 2, AddressingMode::Relative),
        OpCode::new(0x10, "BPL", 2, AddressingMode::Relative),
        //
        OpCode::new(0x00, "BRK", 1, AddressingMode::NoneAddressing),
        // Branches
        OpCode::new(0x50, "BVC", 2, AddressingMode::Relative),
        OpCode::new(0x70, "BVS", 2, AddressingMode::Relative),
        // CL*
        OpCode::new(0x18, "CLC", 2, AddressingMode::NoneAddressing),
        OpCode::new(0xd8, "CLD", 2, AddressingMode::NoneAddressing),
        OpCode::new(0x58, "CLI", 2, AddressingMode::NoneAddressing),
        OpCode::new(0xb8, "CLV", 2, AddressingMode::NoneAddressing),
        // CMP
        OpCode::new(0xc9, "CMP", 2, AddressingMode::Immediate),
        OpCode::new(0xc5, "CMP", 3, AddressingMode::ZeroPage),
        OpCode::new(0xd5, "CMP", 4, AddressingMode::ZeroPageX),
        OpCode::new(0xcd, "CMP", 4, AddressingMode::Absolute),
        OpCode::new(0xdd, "CMP", 4, AddressingMode::AbsoluteX),
        OpCode::new(0xd9, "CMP", 4, AddressingMode::AbsoluteY),
        OpCode::new(0xc1, "CMP", 6, AddressingMode::IndirectX),
        OpCode::new(0xd1, "CMP", 5, AddressingMode::IndirectY),
        // CPX
        OpCode::new(0xe0, "CPX", 2, AddressingMode::Immediate),
        OpCode::new(0xe4, "CPX", 3, AddressingMode::ZeroPage),
        OpCode::new(0xec, "CPX", 4, AddressingMode::Absolute),
        // CPY
        OpCode::new(0xc0, "CPY", 2, AddressingMode::Immediate),
        OpCode::new(0xc4, "CPY", 3, AddressingMode::ZeroPage),
        OpCode::new(0xcc, "CPY", 4, AddressingMode::Absolute),
        // DCP
        OpCode::new(0xc7, "*DCP", 5, AddressingMode::ZeroPage),
        OpCode::new(0xd7, "*DCP", 6, AddressingMode::ZeroPageX),
        OpCode::new(0xcf, "*DCP", 6, AddressingMode::Absolute),
        OpCode::new(0xdf, "*DCP", 7, AddressingMode::AbsoluteX),
        OpCode::new(0xdb, "*DCP", 7, AddressingMode::AbsoluteY),
        OpCode::new(0xc3, "*DCP", 2, AddressingMode::IndirectX),
        OpCode::new(0xd3, "*DCP", 2, AddressingMode::IndirectY),
        // DEC
        OpCode::new(0xc6, "DEC", 5, AddressingMode::ZeroPage),
        OpCode::new(0xd6, "DEC", 6, AddressingMode::ZeroPageX),
        OpCode::new(0xce, "DEC", 6, AddressingMode::Absolute),
        OpCode::new(0xde, "DEC", 7, AddressingMode::AbsoluteX),
        //
        OpCode::new(0xca, "DEX", 2, AddressingMode::NoneAddressing),
        OpCode::new(0x88, "DEY", 2, AddressingMode::NoneAddressing),
        // EOR
        OpCode::new(0x49, "EOR", 2, AddressingMode::Immediate),
        OpCode::new(0x45, "EOR", 3, AddressingMode::ZeroPage),
        OpCode::new(0x55, "EOR", 4, AddressingMode::ZeroPageX),
        OpCode::new(0x4d, "EOR", 4, AddressingMode::Absolute),
        OpCode::new(0x5d, "EOR", 4, AddressingMode::AbsoluteX),
        OpCode::new(0x59, "EOR", 4, AddressingMode::AbsoluteY),
        OpCode::new(0x41, "EOR", 6, AddressingMode::IndirectX),
        OpCode::new(0x51, "EOR", 5, AddressingMode::IndirectY),
        // INC
        OpCode::new(0xe6, "INC", 5, AddressingMode::ZeroPage),
        OpCode::new(0xf6, "INC", 6, AddressingMode::ZeroPageX),
        OpCode::new(0xee, "INC", 6, AddressingMode::Absolute),
        OpCode::new(0xfe, "INC", 7, AddressingMode::AbsoluteX),
        //
        OpCode::new(0xe8, "INX", 2, AddressingMode::NoneAddressing),
        OpCode::new(0xc8, "INY", 2, AddressingMode::NoneAddressing),
        // ISC (unofficial)
        OpCode::new(0xe7, "*ISB", 5, AddressingMode::ZeroPage),
        OpCode::new(0xf7, "*ISB", 6, AddressingMode::ZeroPageX),
        OpCode::new(0xef, "*ISB", 6, AddressingMode::Absolute),
        OpCode::new(0xff, "*ISB", 7, AddressingMode::AbsoluteX),
        OpCode::new(0xfb, "*ISB", 7, AddressingMode::AbsoluteY),
        OpCode::new(0xe3, "*ISB", 8, AddressingMode::IndirectX),
        OpCode::new(0xf3, "*ISB", 8, AddressingMode::IndirectY),
        // JMP
        OpCode::new(0x4c, "JMP", 3, AddressingMode::Absolute),
        OpCode::new(0x6c, "JMP", 5, AddressingMode::Indirect),
        OpCode::new(0x20, "JSR", 6, AddressingMode::Absolute),
        OpCode::new(0x60, "RTS", 6, AddressingMode::NoneAddressing),
        // LAX (unofficial)
        OpCode::new(0xa7, "*LAX", 3, AddressingMode::ZeroPage),
        OpCode::new(0xb7, "*LAX", 4, AddressingMode::ZeroPageY),
        OpCode::new(0xaf, "*LAX", 4, AddressingMode::Absolute),
        OpCode::new(0xbf, "*LAX", 4, AddressingMode::AbsoluteY),
        OpCode::new(0xa3, "*LAX", 6, AddressingMode::IndirectX),
        OpCode::new(0xb3, "*LAX", 5, AddressingMode::IndirectY),
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
        // LSR
        OpCode::new(0x4a, "LSR", 2, AddressingMode::Accumulator),
        OpCode::new(0x46, "LSR", 5, AddressingMode::ZeroPage),
        OpCode::new(0x56, "LSR", 6, AddressingMode::ZeroPageX),
        OpCode::new(0x4e, "LSR", 6, AddressingMode::Absolute),
        OpCode::new(0x5e, "LSR", 7, AddressingMode::AbsoluteX),
        // NOP
        OpCode::new(0xea, "NOP", 2, AddressingMode::NoneAddressing),
        // NOP (unofficial)
        OpCode::new(0x04, "*NOP", 3, AddressingMode::ZeroPage),
        OpCode::new(0x14, "*NOP", 4, AddressingMode::ZeroPageX),
        OpCode::new(0x34, "*NOP", 4, AddressingMode::ZeroPageX),
        OpCode::new(0x44, "*NOP", 3, AddressingMode::ZeroPage),
        OpCode::new(0x54, "*NOP", 4, AddressingMode::ZeroPageX),
        OpCode::new(0x64, "*NOP", 3, AddressingMode::ZeroPage),
        OpCode::new(0x74, "*NOP", 4, AddressingMode::ZeroPageX),
        OpCode::new(0x80, "*NOP", 2, AddressingMode::Immediate),
        OpCode::new(0x82, "*NOP", 2, AddressingMode::Immediate),
        OpCode::new(0x89, "*NOP", 2, AddressingMode::Immediate),
        OpCode::new(0xc2, "*NOP", 2, AddressingMode::Immediate),
        OpCode::new(0xd4, "*NOP", 4, AddressingMode::ZeroPageX),
        OpCode::new(0xe2, "*NOP", 2, AddressingMode::Immediate),
        OpCode::new(0xf4, "*NOP", 4, AddressingMode::ZeroPageX),
        OpCode::new(0x0c, "*NOP", 4, AddressingMode::Absolute),
        OpCode::new(0x1c, "*NOP", 4, AddressingMode::AbsoluteX),
        OpCode::new(0x3c, "*NOP", 4, AddressingMode::AbsoluteX),
        OpCode::new(0x5c, "*NOP", 4, AddressingMode::AbsoluteX),
        OpCode::new(0x7c, "*NOP", 4, AddressingMode::AbsoluteX),
        OpCode::new(0xdc, "*NOP", 4, AddressingMode::AbsoluteX),
        OpCode::new(0xfc, "*NOP", 4, AddressingMode::AbsoluteX),
        OpCode::new(0x1a, "*NOP", 2, AddressingMode::NoneAddressing),
        OpCode::new(0x3a, "*NOP", 2, AddressingMode::NoneAddressing),
        OpCode::new(0x5a, "*NOP", 2, AddressingMode::NoneAddressing),
        OpCode::new(0x7a, "*NOP", 2, AddressingMode::NoneAddressing),
        OpCode::new(0xda, "*NOP", 2, AddressingMode::NoneAddressing),
        OpCode::new(0xfa, "*NOP", 2, AddressingMode::NoneAddressing),
        // ORA
        OpCode::new(0x09, "ORA", 2, AddressingMode::Immediate),
        OpCode::new(0x05, "ORA", 3, AddressingMode::ZeroPage),
        OpCode::new(0x15, "ORA", 4, AddressingMode::ZeroPageX),
        OpCode::new(0x0d, "ORA", 4, AddressingMode::Absolute),
        OpCode::new(0x1d, "ORA", 4, AddressingMode::AbsoluteX),
        OpCode::new(0x19, "ORA", 4, AddressingMode::AbsoluteY),
        OpCode::new(0x01, "ORA", 6, AddressingMode::IndirectX),
        OpCode::new(0x11, "ORA", 5, AddressingMode::IndirectY),
        // PHA
        OpCode::new(0x48, "PHA", 3, AddressingMode::NoneAddressing),
        OpCode::new(0x08, "PHP", 3, AddressingMode::NoneAddressing),
        OpCode::new(0x68, "PLA", 4, AddressingMode::NoneAddressing),
        OpCode::new(0x28, "PLP", 4, AddressingMode::NoneAddressing),
        // RLA (unofficial)
        OpCode::new(0x27, "*RLA", 5, AddressingMode::ZeroPage),
        OpCode::new(0x37, "*RLA", 6, AddressingMode::ZeroPageX),
        OpCode::new(0x2f, "*RLA", 6, AddressingMode::Absolute),
        OpCode::new(0x3f, "*RLA", 7, AddressingMode::AbsoluteX),
        OpCode::new(0x3b, "*RLA", 7, AddressingMode::AbsoluteY),
        OpCode::new(0x23, "*RLA", 8, AddressingMode::IndirectX),
        OpCode::new(0x33, "*RLA", 8, AddressingMode::IndirectY),
        // ROL
        OpCode::new(0x2a, "ROL", 2, AddressingMode::Accumulator),
        OpCode::new(0x26, "ROL", 5, AddressingMode::ZeroPage),
        OpCode::new(0x36, "ROL", 6, AddressingMode::ZeroPageX),
        OpCode::new(0x2e, "ROL", 6, AddressingMode::Absolute),
        OpCode::new(0x3e, "ROL", 7, AddressingMode::AbsoluteX),
        // ROR
        OpCode::new(0x6a, "ROR", 2, AddressingMode::Accumulator),
        OpCode::new(0x66, "ROR", 5, AddressingMode::ZeroPage),
        OpCode::new(0x76, "ROR", 6, AddressingMode::ZeroPageX),
        OpCode::new(0x6e, "ROR", 6, AddressingMode::Absolute),
        OpCode::new(0x7e, "ROR", 7, AddressingMode::AbsoluteX),
        // SBC
        OpCode::new(0xe9, "SBC", 2, AddressingMode::Immediate),
        OpCode::new(0xe5, "SBC", 3, AddressingMode::ZeroPage),
        OpCode::new(0xf5, "SBC", 4, AddressingMode::ZeroPageX),
        OpCode::new(0xed, "SBC", 4, AddressingMode::Absolute),
        OpCode::new(0xfd, "SBC", 4, AddressingMode::AbsoluteX),
        OpCode::new(0xf9, "SBC", 4, AddressingMode::AbsoluteY),
        OpCode::new(0xe1, "SBC", 6, AddressingMode::IndirectX),
        OpCode::new(0xf1, "SBC", 5, AddressingMode::IndirectY),
        // SBC (unofficial)
        OpCode::new(0xeb, "*SBC", 2, AddressingMode::Immediate),
        // S*
        OpCode::new(0x38, "SEC", 2, AddressingMode::NoneAddressing),
        OpCode::new(0xf8, "SED", 2, AddressingMode::NoneAddressing),
        OpCode::new(0x78, "SEI", 2, AddressingMode::NoneAddressing),
        // SAX
        OpCode::new(0x87, "*SAX", 3, AddressingMode::ZeroPage),
        OpCode::new(0x97, "*SAX", 4, AddressingMode::ZeroPageY),
        OpCode::new(0x83, "*SAX", 6, AddressingMode::IndirectX),
        OpCode::new(0x8f, "*SAX", 4, AddressingMode::Absolute),
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
        // SLO (unofficial)
        OpCode::new(0x07, "*SLO", 5, AddressingMode::ZeroPage),
        OpCode::new(0x17, "*SLO", 6, AddressingMode::ZeroPageX),
        OpCode::new(0x0f, "*SLO", 6, AddressingMode::Absolute),
        OpCode::new(0x1f, "*SLO", 7, AddressingMode::AbsoluteX),
        OpCode::new(0x1b, "*SLO", 7, AddressingMode::AbsoluteY),
        OpCode::new(0x03, "*SLO", 8, AddressingMode::IndirectX),
        OpCode::new(0x13, "*SLO", 8, AddressingMode::IndirectY),
        // SRE (unofficial)
        OpCode::new(0x47, "*SRE", 5, AddressingMode::ZeroPage),
        OpCode::new(0x57, "*SRE", 6, AddressingMode::ZeroPageX),
        OpCode::new(0x4f, "*SRE", 6, AddressingMode::Absolute),
        OpCode::new(0x5f, "*SRE", 7, AddressingMode::AbsoluteX),
        OpCode::new(0x5b, "*SRE", 7, AddressingMode::AbsoluteY),
        OpCode::new(0x43, "*SRE", 8, AddressingMode::IndirectX),
        OpCode::new(0x53, "*SRE", 8, AddressingMode::IndirectY),
        // RRA (unofficial)
        OpCode::new(0x67, "*RRA", 5, AddressingMode::ZeroPage),
        OpCode::new(0x77, "*RRA", 6, AddressingMode::ZeroPageX),
        OpCode::new(0x6f, "*RRA", 6, AddressingMode::Absolute),
        OpCode::new(0x7f, "*RRA", 7, AddressingMode::AbsoluteX),
        OpCode::new(0x7b, "*RRA", 7, AddressingMode::AbsoluteY),
        OpCode::new(0x63, "*RRA", 8, AddressingMode::IndirectX),
        OpCode::new(0x73, "*RRA", 8, AddressingMode::IndirectY),
        // RTI
        OpCode::new(0x40, "RTI", 6, AddressingMode::NoneAddressing),
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
