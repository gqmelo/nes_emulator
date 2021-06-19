use crate::opcodes;

#[derive(Debug)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY,
    NoneAddressing,
}

#[derive(Debug)]
pub enum StatusFlag {
    Carry = 0,
    Zero = 1,
    InterruptDisable = 2,
    Decimal = 3,
    Overflow = 6,
    Negative = 7,
}

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub register_s: u8,
    pub status: u8,
    pub program_counter: u16,
    memory: [u8; 0xFFFF],
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            register_s: 0,
            status: 0,
            program_counter: 0,
            memory: [0; 0xFFFF],
        }
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.run();
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
        self.reset();
    }

    fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.register_s = 0;
        self.status = 0;
        self.program_counter = self.mem_read_u16(0xFFFC)
    }

    pub fn run(&mut self) {
        let ref opcodes = *opcodes::OPCODES_MAP;
        loop {
            println!(
                "a: {:#04x}; x: {:#04x}; y: {:#04x}",
                self.register_a, self.register_x, self.register_y
            );
            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;

            let opcode = opcodes
                .get(&code)
                .expect(&format!("invalid opcode: {:#04x}", code));

            match opcode.mnemonic {
                "AND" => self.and(&opcode.mode),
                "BIT" => self.bit(&opcode.mode),
                "BRK" => return,
                "CLC" => self.clear_status_flag(StatusFlag::Carry),
                "CLD" => self.clear_status_flag(StatusFlag::Decimal),
                "CLI" => self.clear_status_flag(StatusFlag::InterruptDisable),
                "CLV" => self.clear_status_flag(StatusFlag::Overflow),
                "DEC" => self.dec(&opcode.mode),
                "DEX" => self.dex(),
                "DEY" => self.dey(),
                "EOR" => self.eor(&opcode.mode),
                "INC" => self.inc(&opcode.mode),
                "INX" => self.inx(),
                "INY" => self.iny(),
                "LDA" => self.lda(&opcode.mode),
                "LDX" => self.ldx(&opcode.mode),
                "LDY" => self.ldy(&opcode.mode),
                "ORA" => self.ora(&opcode.mode),
                "SEC" => self.set_status_flag(StatusFlag::Carry),
                "SED" => self.set_status_flag(StatusFlag::Decimal),
                "SEI" => self.set_status_flag(StatusFlag::InterruptDisable),
                "STA" => self.sta(&opcode.mode),
                "STX" => self.stx(&opcode.mode),
                "STY" => self.sty(&opcode.mode),
                "TAX" => self.tax(),
                "TAY" => self.tay(),
                "TSX" => self.tsx(),
                "TXA" => self.txa(),
                "TXS" => self.txs(),
                "TYA" => self.tya(),
                _ => todo!("{:#04x}", code),
            };

            self.program_counter += self.get_program_counter_increment(&opcode.mode);
        }
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.register_a = self.register_a & value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        if self.register_a & value > 0 {
            self.status &= 0b1111_1101
        } else {
            self.status |= 0b0000_0010
        };

        self.status |= value & 0b1100_0000
    }

    fn clear_status_flag(&mut self, status_flag: StatusFlag) {
        let mask = 0b1111_1111 ^ (0b0000_0001 << status_flag as u8);
        self.status &= mask;
    }

    fn set_status_flag(&mut self, status_flag: StatusFlag) {
        let mask = 0b0000_0001 << status_flag as u8;
        self.status |= mask;
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let new_value = self.mem_read(addr).wrapping_sub(1);
        self.mem_write(addr, new_value);
        self.update_zero_and_negative_flags(new_value);
    }

    fn dex(&mut self) {
        self.register_x = self.register_x.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn dey(&mut self) {
        self.register_y = self.register_y.wrapping_sub(1);
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.register_a = self.register_a ^ value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let new_value = self.mem_read(addr).wrapping_add(1);
        self.mem_write(addr, new_value);
        self.update_zero_and_negative_flags(new_value);
    }

    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn iny(&mut self) {
        self.register_y = self.register_y.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_y);
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

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.register_a = self.register_a | value;
        self.update_zero_and_negative_flags(self.register_a);
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

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn tsx(&mut self) {
        self.register_x = self.register_s;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn txs(&mut self) {
        self.register_s = self.register_x;
        self.update_zero_and_negative_flags(self.register_s);
    }

    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,
            AddressingMode::ZeroPage => self.mem_read(self.program_counter).into(),
            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),
            AddressingMode::ZeroPageX => self
                .mem_read(self.program_counter)
                .wrapping_add(self.register_x)
                .into(),
            AddressingMode::ZeroPageY => self
                .mem_read(self.program_counter)
                .wrapping_add(self.register_y)
                .into(),
            AddressingMode::AbsoluteX => self
                .mem_read_u16(self.program_counter)
                .wrapping_add(self.register_x.into()),
            AddressingMode::AbsoluteY => self
                .mem_read_u16(self.program_counter)
                .wrapping_add(self.register_y.into()),
            AddressingMode::IndirectX => {
                let ptr = self
                    .mem_read(self.program_counter)
                    .wrapping_add(self.register_x.into());
                self.mem_read_u16(ptr.into())
            }
            AddressingMode::IndirectY => {
                let ptr = self.mem_read(self.program_counter);
                self.mem_read_u16(ptr.into())
                    .wrapping_add(self.register_y.into())
            }
            AddressingMode::NoneAddressing => {
                panic!("mode {:?} not supported", mode);
            }
        }
    }

    fn get_program_counter_increment(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::NoneAddressing => 0,
            AddressingMode::Immediate
            | AddressingMode::ZeroPage
            | AddressingMode::ZeroPageX
            | AddressingMode::ZeroPageY
            | AddressingMode::IndirectX
            | AddressingMode::IndirectY => 1,
            AddressingMode::Absolute | AddressingMode::AbsoluteX | AddressingMode::AbsoluteY => 2,
        }
    }

    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_read_u16(&self, addr: u16) -> u16 {
        u16::from_le_bytes([self.memory[addr as usize], self.memory[(addr + 1) as usize]])
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        self.memory[addr as usize..(addr + 2) as usize].copy_from_slice(&data.to_le_bytes());
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            self.status |= 0b0000_0010;
        } else {
            self.status &= 0b1111_1101;
        }

        if result & 0b1000_0000 != 0 {
            self.status |= 0b1000_0000;
        } else {
            self.status &= 0b0111_1111;
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use rstest::*;

    #[fixture]
    fn cpu() -> CPU {
        CPU::new()
    }

    #[rstest]
    #[case(vec![0x29, 0x5e, 0x00])] // and #$5e
    #[case(vec![
        0xa2, 0x5e, // ldx #$5e
        0x86, 0x10, // stx $10
        0x25, 0x10, // and $10
        0x00
    ])]
    #[case(vec![
        0xa2, 0x5e, // ldx #$5e
        0x86, 0x15, // stx $15
        0xa2, 0x05, // ldx #$05
        0x35, 0x10, // and $10,X
        0x00
    ])]
    #[case(vec![
        0xa2, 0x5e, // ldx #$5e
        0x8e, 0xe4, 0x70, // stx $70e4
        0x2d, 0xe4, 0x70, // and $70e4
        0x00
    ])]
    #[case(vec![
        0xa2, 0x5e, // ldx #$5e
        0x8e, 0xe9, 0x70, // stx $70e9
        0xa2, 0x05, // ldx #$05
        0x3d, 0xe4, 0x70, // and $70e4,X
        0x00
    ])]
    #[case(vec![
        0xa2, 0x5e, // ldx #$5e
        0x8e, 0xe9, 0x70, // stx $70e9
        0xa0, 0x05, // ldy #$05
        0x39, 0xe4, 0x70, // lda $70e4,Y
        0x00
    ])]
    fn and(mut cpu: CPU, #[case] program: Vec<u8>) {
        cpu.load(program);
        cpu.register_a = 0x97;
        cpu.run();
        assert_eq!(cpu.register_a, 0x16);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn and_indirect_x(mut cpu: CPU) {
        cpu.load(vec![0x21, 0x01, 0x00]);
        cpu.register_a = 0x97;
        cpu.register_x = 0x01;
        cpu.mem_write_u16(0x0002, 0x70e4);
        cpu.mem_write(0x70e4, 0x5e);
        cpu.run();
        assert_eq!(cpu.register_a, 0x16);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn and_indirect_y(mut cpu: CPU) {
        cpu.load(vec![0x31, 0x02, 0x00]);
        cpu.register_a = 0x97;
        cpu.register_y = 0x01;
        cpu.mem_write_u16(0x02, 0x70e4);
        cpu.mem_write(0x70e5, 0x5e);
        cpu.run();
        assert_eq!(cpu.register_a, 0x16);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn and_zero_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0x29, 0x5e, 0x00]);
        assert_eq!(cpu.register_a, 0x00);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn and_negative_flag(mut cpu: CPU) {
        cpu.load(vec![0x29, 0x9e, 0x00]);
        cpu.register_a = 0xa5;
        cpu.run();
        assert_eq!(cpu.register_a, 0x84);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    #[case(0x18, 0b1111_1111, 0b1111_1110)] // clc
    #[case(0x18, 0b0000_1111, 0b0000_1110)] // clc
    #[case(0x18, 0b1111_0000, 0b1111_0000)] // clc
    #[case(0x18, 0b0000_0000, 0b0000_0000)] // clc
    #[case(0xd8, 0b1111_1111, 0b1111_0111)] // cld
    #[case(0xd8, 0b0000_1111, 0b0000_0111)] // cld
    #[case(0xd8, 0b1111_0000, 0b1111_0000)] // cld
    #[case(0xd8, 0b0000_0000, 0b0000_0000)] // cld
    #[case(0x58, 0b1111_1111, 0b1111_1011)] // cli
    #[case(0x58, 0b0000_1111, 0b0000_1011)] // cli
    #[case(0x58, 0b1111_0000, 0b1111_0000)] // cli
    #[case(0x58, 0b0000_0000, 0b0000_0000)] // cli
    #[case(0xb8, 0b1111_1111, 0b1011_1111)] // clv
    #[case(0xb8, 0b0000_1111, 0b0000_1111)] // clv
    #[case(0xb8, 0b1111_0000, 0b1011_0000)] // clv
    #[case(0xb8, 0b0000_0000, 0b0000_0000)] // clv
    #[case(0x38, 0b1111_1111, 0b1111_1111)] // sec
    #[case(0x38, 0b0000_1111, 0b0000_1111)] // sec
    #[case(0x38, 0b1111_0000, 0b1111_0001)] // sec
    #[case(0x38, 0b0000_0000, 0b0000_0001)] // sec
    #[case(0xf8, 0b1111_1111, 0b1111_1111)] // sed
    #[case(0xf8, 0b0000_1111, 0b0000_1111)] // sed
    #[case(0xf8, 0b1111_0000, 0b1111_1000)] // sed
    #[case(0xf8, 0b0000_0000, 0b0000_1000)] // sed
    #[case(0x78, 0b1111_1111, 0b1111_1111)] // sei
    #[case(0x78, 0b0000_1111, 0b0000_1111)] // sei
    #[case(0x78, 0b1111_0000, 0b1111_0100)] // sei
    #[case(0x78, 0b0000_0000, 0b0000_0100)] // sei
    fn clear_set_status_flags(
        mut cpu: CPU,
        #[case] instruction: u8,
        #[case] initial_status: u8,
        #[case] expected_status: u8,
    ) {
        cpu.load(vec![instruction, 0x00]);
        cpu.status = initial_status;
        cpu.run();
        assert_eq!(
            cpu.status, expected_status,
            "status register {:#08b} not as expected {:#08b}",
            cpu.status, expected_status
        );
    }

    #[rstest]
    #[case(0x80, 0xca, 0b1100_0000)]
    #[case(0x40, 0xca, 0b1100_0000)]
    #[case(0x20, 0xca, 0b1100_0010)]
    #[case(0x10, 0xca, 0b1100_0010)]
    #[case(0x08, 0xca, 0b1100_0000)]
    #[case(0x04, 0xca, 0b1100_0010)]
    #[case(0x02, 0xca, 0b1100_0000)]
    #[case(0x01, 0xca, 0b1100_0010)]
    #[case(0x01, 0x8a, 0b1000_0010)]
    #[case(0x01, 0x4a, 0b0100_0010)]
    #[case(0x01, 0x3a, 0b0000_0010)]
    fn bit_zero_page(
        mut cpu: CPU,
        #[case] register_a: u8,
        #[case] memory_value: u8,
        #[case] expected_status: u8,
    ) {
        cpu.load(vec![0x24, 0x02, 0x00]);
        cpu.mem_write(0x02, memory_value);
        cpu.register_a = register_a;
        cpu.run();
        assert_eq!(cpu.register_a, register_a);
        assert_eq!(cpu.status, expected_status);
    }

    #[rstest]
    #[case(0x80, 0xca, 0b1100_0000)]
    #[case(0x40, 0xca, 0b1100_0000)]
    #[case(0x20, 0xca, 0b1100_0010)]
    #[case(0x10, 0xca, 0b1100_0010)]
    #[case(0x08, 0xca, 0b1100_0000)]
    #[case(0x04, 0xca, 0b1100_0010)]
    #[case(0x02, 0xca, 0b1100_0000)]
    #[case(0x01, 0xca, 0b1100_0010)]
    #[case(0x01, 0x8a, 0b1000_0010)]
    #[case(0x01, 0x4a, 0b0100_0010)]
    #[case(0x01, 0x3a, 0b0000_0010)]
    fn bit_absolute(
        mut cpu: CPU,
        #[case] register_a: u8,
        #[case] memory_value: u8,
        #[case] expected_status: u8,
    ) {
        cpu.load(vec![0x2c, 0xe2, 0x70, 0x00]);
        cpu.mem_write(0x70e2, memory_value);
        cpu.register_a = register_a;
        cpu.run();
        assert_eq!(cpu.register_a, register_a);
        assert_eq!(cpu.status, expected_status);
    }

    #[rstest]
    #[case(vec![0xc6, 0x10, 0x00])] // dec $10
    #[case(vec![
        0xa2, 0x02, // ldx #$02
        0xd6, 0x0e, // dec $0e,X
        0x00
    ])]
    fn dec_zero_page(mut cpu: CPU, #[case] program: Vec<u8>) {
        cpu.load(program);
        cpu.mem_write(0x10, 5);
        cpu.run();
        assert_eq!(cpu.mem_read(0x10), 4);
    }

    #[rstest]
    #[case(vec![0xce, 0xe4, 0x70, 0x00])] // dec $70e4
    #[case(vec![
        0xa2, 0x02, // ldx #$02
        0xde, 0xe2, 0x70, // dec $70e2,X
        0x00
    ])]
    fn dec_absolute(mut cpu: CPU, #[case] program: Vec<u8>) {
        cpu.load(program);
        cpu.mem_write(0x70e4, 5);
        cpu.run();
        assert_eq!(cpu.mem_read_u16(0x70e4), 4);
    }

    #[rstest]
    fn dex(mut cpu: CPU) {
        cpu.load(vec![0xca, 0x00]);
        cpu.register_x = 10;
        cpu.run();
        assert_eq!(cpu.register_x, 9);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn dex_zero_flag(mut cpu: CPU) {
        cpu.load(vec![0xca, 0x00]);
        cpu.register_x = 0x01;
        cpu.run();
        assert_eq!(cpu.register_x, 0);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn dex_negative_flag(mut cpu: CPU) {
        cpu.load(vec![0xca, 0x00]);
        cpu.register_x = 0x00;
        cpu.run();
        assert_eq!(cpu.register_x, 0xff);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    fn dey(mut cpu: CPU) {
        cpu.load(vec![0x88, 0x00]);
        cpu.register_y = 10;
        cpu.run();
        assert_eq!(cpu.register_y, 9);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn dey_zero_flag(mut cpu: CPU) {
        cpu.load(vec![0x88, 0x00]);
        cpu.register_y = 0x01;
        cpu.run();
        assert_eq!(cpu.register_y, 0);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn dey_negative_flag(mut cpu: CPU) {
        cpu.load(vec![0x88, 0x00]);
        cpu.register_y = 0x00;
        cpu.run();
        assert_eq!(cpu.register_y, 0xff);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    #[case(vec![0x49, 0x5e, 0x00])] // eor #$5e
    #[case(vec![
        0xa2, 0x5e, // ldx #$5e
        0x86, 0x10, // stx $10
        0x45, 0x10, // eor $10
        0x00
    ])]
    #[case(vec![
        0xa2, 0x5e, // ldx #$5e
        0x86, 0x15, // stx $15
        0xa2, 0x05, // ldx #$05
        0x55, 0x10, // eor $10,X
        0x00
    ])]
    #[case(vec![
        0xa2, 0x5e, // ldx #$5e
        0x8e, 0xe4, 0x70, // stx $70e4
        0x4d, 0xe4, 0x70, // eor $70e4
        0x00
    ])]
    #[case(vec![
        0xa2, 0x5e, // ldx #$5e
        0x8e, 0xe9, 0x70, // stx $70e9
        0xa2, 0x05, // ldx #$05
        0x5d, 0xe4, 0x70, // eor $70e4,X
        0x00
    ])]
    #[case(vec![
        0xa2, 0x5e, // ldx #$5e
        0x8e, 0xe9, 0x70, // stx $70e9
        0xa0, 0x05, // ldy #$05
        0x59, 0xe4, 0x70, // eor $70e4,Y
        0x00
                                ])]
    fn eor(mut cpu: CPU, #[case] program: Vec<u8>) {
        cpu.load(program);
        cpu.register_a = 0x67;
        cpu.run();
        assert_eq!(cpu.register_a, 0x39);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn eor_indirect_x(mut cpu: CPU) {
        cpu.load(vec![0x41, 0x01, 0x00]);
        cpu.register_a = 0x67;
        cpu.register_x = 0x01;
        cpu.mem_write_u16(0x0002, 0x70e4);
        cpu.mem_write(0x70e4, 0x5e);
        cpu.run();
        assert_eq!(cpu.register_a, 0x39);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn eor_indirect_y(mut cpu: CPU) {
        cpu.load(vec![0x51, 0x02, 0x00]);
        cpu.register_a = 0x67;
        cpu.register_y = 0x01;
        cpu.mem_write_u16(0x02, 0x70e4);
        cpu.mem_write(0x70e5, 0x5e);
        cpu.run();
        assert_eq!(cpu.register_a, 0x39);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn eor_zero_flag(mut cpu: CPU) {
        cpu.load(vec![0x49, 0x5e, 0x00]);
        cpu.register_a = 0x5e;
        cpu.run();
        assert_eq!(cpu.register_a, 0x00);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn eor_negative_flag(mut cpu: CPU) {
        cpu.load(vec![0x49, 0x9e, 0x00]);
        cpu.register_a = 0x15;
        cpu.run();
        assert_eq!(cpu.register_a, 0x8b);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    #[case(vec![0xa9, 0x05, 0x00])] // lda #$05
    #[case(vec![
        0xa2, 0x05, // ldx #$05
        0x86, 0x10, // stx $10
        0xa5, 0x10, // lda $10
        0x00
    ])]
    #[case(vec![
        0xa2, 0x05, // ldx #$05
        0x86, 0x15, // stx $15
        0xb5, 0x10, // lda $10,X
        0x00
    ])]
    #[case(vec![
        0xa2, 0x05, // ldx #$05
        0x8e, 0xe4, 0x70, // stx $70e4
        0xad, 0xe4, 0x70, // lda $70e4
        0x00
    ])]
    #[case(vec![
        0xa2, 0x05, // ldx #$05
        0x8e, 0xe9, 0x70, // stx $70e9
        0xbd, 0xe4, 0x70, // lda $70e4,X
        0x00
    ])]
    #[case(vec![
        0xa0, 0x05, // ldy #$05
        0x8c, 0xe9, 0x70, // sty $70e9
        0xb9, 0xe4, 0x70, // lda $70e4,Y
        0x00
    ])]
    fn lda(mut cpu: CPU, #[case] program: Vec<u8>) {
        cpu.load_and_run(program);
        assert_eq!(cpu.register_a, 0x05);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn lda_indirect_x(mut cpu: CPU) {
        cpu.load(vec![0xa1, 0x01, 0x00]);
        cpu.register_x = 0x01;
        cpu.mem_write_u16(0x0002, 0x70e4);
        cpu.mem_write(0x70e4, 0x05);
        cpu.run();
        assert_eq!(cpu.register_a, 0x05);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn lda_indirect_y(mut cpu: CPU) {
        cpu.load(vec![0xb1, 0x02, 0x00]);
        cpu.register_y = 0x01;
        cpu.mem_write_u16(0x02, 0x70e4);
        cpu.mem_write(0x70e5, 0x05);
        cpu.run();
        assert_eq!(cpu.register_a, 0x05);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn lda_zero_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn lda_negative_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa9, 0xf0, 0x00]);
        assert_eq!(cpu.register_a, 0xf0);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    #[case(vec![0xa2, 0x05, 0x00])] // ldx #$05
    #[case(vec![
        0xa9, 0x05, // lda #$05
        0x85, 0x10, // sta $10
        0xa6, 0x10, // ldx $10
        0x00
    ])]
    #[case(vec![
        0xa0, 0x05, // ldy #$05
        0x84, 0x15, // sty $15
        0xb6, 0x10, // ldx $10,X
        0x00
    ])]
    #[case(vec![
        0xa9, 0x05, // lda #$05
        0x8d, 0xe4, 0x70, // sta $70e4
        0xae, 0xe4, 0x70, // ldx $70e4
        0x00
    ])]
    #[case(vec![
        0xa0, 0x05, // ldy #$05
        0x8c, 0xe9, 0x70, // sty $70e9
        0xbe, 0xe4, 0x70, // ldx $70e4,Y
        0x00
    ])]
    fn ldx(mut cpu: CPU, #[case] program: Vec<u8>) {
        cpu.load_and_run(program);
        assert_eq!(cpu.register_x, 0x05);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn ldx_zero_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa2, 0x00, 0x00]);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn ldx_negative_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa2, 0xf0, 0x00]);
        assert_eq!(cpu.register_x, 0xf0);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    #[case(vec![0xa0, 0x05, 0x00])] // ldy #$05
    #[case(vec![
        0xa9, 0x05, // lda #$05
        0x85, 0x10, // sta $10
        0xa4, 0x10, // ldy $10
        0x00
    ])]
    #[case(vec![
        0xa2, 0x05, // ldx #$05
        0x86, 0x15, // stx $15
        0xb4, 0x10, // ldy $10,X
        0x00
    ])]
    #[case(vec![
        0xa9, 0x05, // lda #$05
        0x8d, 0xe4, 0x70, // sta $70e4
        0xac, 0xe4, 0x70, // ldy $70e4
        0x00
    ])]
    #[case(vec![
        0xa2, 0x05, // ldx #$05
        0x8e, 0xe9, 0x70, // stx $70e9
        0xbc, 0xe4, 0x70, // ldy $70e4,Y
        0x00
    ])]
    fn ldy(mut cpu: CPU, #[case] program: Vec<u8>) {
        cpu.load_and_run(program);
        assert_eq!(cpu.register_y, 0x05);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn ldy_zero_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa0, 0x00, 0x00]);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn ldy_negative_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa0, 0xf0, 0x00]);
        assert_eq!(cpu.register_y, 0xf0);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    #[case(vec![0x09, 0x5e, 0x00])] // ora #$5e
    #[case(vec![
        0xa2, 0x5e, // ldx #$5e
        0x86, 0x10, // stx $10
        0x05, 0x10, // ora $10
        0x00
    ])]
    #[case(vec![
        0xa2, 0x5e, // ldx #$5e
        0x86, 0x15, // stx $15
        0xa2, 0x05, // ldx #$05
        0x15, 0x10, // ora $10,X
        0x00
    ])]
    #[case(vec![
        0xa2, 0x5e, // ldx #$5e
        0x8e, 0xe4, 0x70, // stx $70e4
        0x0d, 0xe4, 0x70, // ora $70e4
        0x00
    ])]
    #[case(vec![
        0xa2, 0x5e, // ldx #$5e
        0x8e, 0xe9, 0x70, // stx $70e9
        0xa2, 0x05, // ldx #$05
        0x1d, 0xe4, 0x70, // ora $70e4,X
        0x00
    ])]
    #[case(vec![
        0xa2, 0x5e, // ldx #$5e
        0x8e, 0xe9, 0x70, // stx $70e9
        0xa0, 0x05, // ldy #$05
        0x19, 0xe4, 0x70, // ora $70e4,Y
        0x00
    ])]
    fn ora(mut cpu: CPU, #[case] program: Vec<u8>) {
        cpu.load(program);
        cpu.register_a = 0x67;
        cpu.run();
        assert_eq!(cpu.register_a, 0x7f);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn ora_indirect_x(mut cpu: CPU) {
        cpu.load(vec![0x01, 0x01, 0x00]);
        cpu.register_a = 0x67;
        cpu.register_x = 0x01;
        cpu.mem_write_u16(0x0002, 0x70e4);
        cpu.mem_write(0x70e4, 0x5e);
        cpu.run();
        assert_eq!(cpu.register_a, 0x7f);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn ora_indirect_y(mut cpu: CPU) {
        cpu.load(vec![0x11, 0x02, 0x00]);
        cpu.register_a = 0x67;
        cpu.register_y = 0x01;
        cpu.mem_write_u16(0x02, 0x70e4);
        cpu.mem_write(0x70e5, 0x5e);
        cpu.run();
        assert_eq!(cpu.register_a, 0x7f);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn ora_zero_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0x09, 0x00, 0x00]);
        assert_eq!(cpu.register_a, 0x00);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn ora_negative_flag(mut cpu: CPU) {
        cpu.load(vec![0x09, 0x9e, 0x00]);
        cpu.register_a = 0x15;
        cpu.run();
        assert_eq!(cpu.register_a, 0x9f);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    #[case(vec![0x85, 0x10, 0x00])] // sta $10
    #[case(vec![
        0xa2, 0x02, // ldx #$02
        0x95, 0x0e, // sta $0e
        0x00
    ])]
    fn sta_zero_page(mut cpu: CPU, #[case] program: Vec<u8>) {
        cpu.load(program);
        cpu.register_a = 5;
        cpu.run();
        assert_eq!(cpu.mem_read(0x10), 5);
    }

    #[rstest]
    #[case(vec![0x8d, 0xe4, 0x70, 0x00])] // sta $70e4
    #[case(vec![
        0xa2, 0x02, // ldx #$02
        0x9d, 0xe2, 0x70, // // sta $70e4,X
        0x00
    ])]
    #[case(vec![
        0xa0, 0x02, // ldy #$02
        0x99, 0xe2, 0x70, // // sta $70e4,Y
        0x00
    ])]
    fn sta_absolute(mut cpu: CPU, #[case] program: Vec<u8>) {
        cpu.load(program);
        cpu.register_a = 5;
        cpu.run();
        assert_eq!(cpu.mem_read_u16(0x70e4), 5);
    }

    #[rstest]
    fn sta_indirect_x(mut cpu: CPU) {
        cpu.load(vec![0x81, 0x01, 0x00]);
        cpu.register_a = 5;
        cpu.register_x = 0x01;
        cpu.mem_write_u16(0x0002, 0x70e4);
        cpu.run();

        assert_eq!(cpu.mem_read_u16(0x70e4), 5);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn sta_indirect_y(mut cpu: CPU) {
        cpu.load(vec![0x91, 0x02, 0x00]);
        cpu.register_a = 5;
        cpu.register_y = 0x01;
        cpu.mem_write_u16(0x02, 0x70e3);
        cpu.run();

        assert_eq!(cpu.mem_read_u16(0x70e4), 5);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    #[case(vec![0x86, 0x10, 0x00])] // stx $10
    #[case(vec![
        0xa0, 0x02, // ldy #$02
        0x96, 0x0e, // stx $0e
        0x00
    ])]
    fn stx_zero_page(mut cpu: CPU, #[case] program: Vec<u8>) {
        cpu.load(program);
        cpu.register_x = 5;
        cpu.run();
        assert_eq!(cpu.mem_read(0x10), 5);
    }

    #[rstest]
    fn stx_absolute(mut cpu: CPU) {
        cpu.load(vec![0x8e, 0xe4, 0x70, 0x00]); // stx $70e4
        cpu.register_x = 5;
        cpu.run();
        assert_eq!(cpu.mem_read_u16(0x70e4), 5);
    }

    #[rstest]
    #[case(vec![0x84, 0x10, 0x00])] // sty $10
    #[case(vec![
        0xa2, 0x02, // ldx #$02
        0x94, 0x0e, // sty $0e
        0x00
    ])]
    fn sty_zero_page(mut cpu: CPU, #[case] program: Vec<u8>) {
        cpu.load(program);
        cpu.register_y = 5;
        cpu.run();
        assert_eq!(cpu.mem_read(0x10), 5);
    }

    #[rstest]
    fn sty_absolute(mut cpu: CPU) {
        cpu.load(vec![0x8c, 0xe4, 0x70, 0x00]); // sty $70e4
        cpu.register_y = 5;
        cpu.run();
        assert_eq!(cpu.mem_read_u16(0x70e4), 5);
    }

    #[rstest]
    fn tax(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa9, 0x05, 0xaa, 0x00]);
        assert_eq!(cpu.register_x, 0x05);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn tax_zero_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa9, 0x00, 0xaa, 0x00]);
        assert_eq!(cpu.register_x, 0x00);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn tax_negative_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa9, 0xf0, 0xaa, 0x00]);
        assert_eq!(cpu.register_x, 0xf0);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    fn tay(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa9, 0x05, 0xa8, 0x00]);
        assert_eq!(cpu.register_y, 0x05);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn tay_zero_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa9, 0x00, 0xa8, 0x00]);
        assert_eq!(cpu.register_y, 0x00);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn tay_negative_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa9, 0xf0, 0xa8, 0x00]);
        assert_eq!(cpu.register_y, 0xf0);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    fn tsx(mut cpu: CPU) {
        cpu.load(vec![0xba, 0x00]);
        cpu.register_s = 5;
        cpu.run();
        assert_eq!(cpu.register_x, 0x05);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn tsx_zero_flag(mut cpu: CPU) {
        cpu.load(vec![0xba, 0x00]);
        cpu.register_s = 0;
        cpu.run();
        assert_eq!(cpu.register_x, 0x00);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn tsx_negative_flag(mut cpu: CPU) {
        cpu.load(vec![0xba, 0x00]);
        cpu.register_s = 0xf0;
        cpu.run();
        assert_eq!(cpu.register_x, 0xf0);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    fn txa(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa2, 0x05, 0x8a, 0x00]);
        assert_eq!(cpu.register_a, 0x05);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn txa_zero_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa2, 0x00, 0x8a, 0x00]);
        assert_eq!(cpu.register_a, 0x00);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn txa_negative_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa2, 0xf0, 0x8a, 0x00]);
        assert_eq!(cpu.register_a, 0xf0);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    fn txs(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa2, 0x05, 0x9a, 0x00]);
        assert_eq!(cpu.register_s, 0x05);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn txs_zero_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa2, 0x00, 0x9a, 0x00]);
        assert_eq!(cpu.register_s, 0x00);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn txs_negative_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa2, 0xf0, 0x9a, 0x00]);
        assert_eq!(cpu.register_s, 0xf0);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    fn tya(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa0, 0x05, 0x98, 0x00]);
        assert_eq!(cpu.register_a, 0x05);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn tya_zero_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa0, 0x00, 0x98, 0x00]);
        assert_eq!(cpu.register_a, 0x00);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn tya_negative_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa0, 0xf0, 0x98, 0x00]);
        assert_eq!(cpu.register_a, 0xf0);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    #[case(vec![0xe6, 0x10, 0x00])] // inc $10
    #[case(vec![
        0xa2, 0x02, // ldx #$02
        0xf6, 0x0e, // inc $0e,X
        0x00
    ])]
    fn inc_zero_page(mut cpu: CPU, #[case] program: Vec<u8>) {
        cpu.load(program);
        cpu.mem_write(0x10, 5);
        cpu.run();
        assert_eq!(cpu.mem_read(0x10), 6);
    }

    #[rstest]
    #[case(vec![0xee, 0xe4, 0x70, 0x00])] // inc $70e4
    #[case(vec![
        0xa2, 0x02, // ldx #$02
        0xfe, 0xe2, 0x70, // inc $70e4,X
        0x00
    ])]
    fn inc_absolute(mut cpu: CPU, #[case] program: Vec<u8>) {
        cpu.load(program);
        cpu.mem_write(0x70e4, 5);
        cpu.run();
        assert_eq!(cpu.mem_read_u16(0x70e4), 6);
    }

    #[rstest]
    fn inx(mut cpu: CPU) {
        cpu.load(vec![0xe8, 0x00]);
        cpu.register_x = 10;
        cpu.run();
        assert_eq!(cpu.register_x, 11);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn inx_zero_flag(mut cpu: CPU) {
        cpu.load(vec![0xe8, 0x00]);
        cpu.register_x = 0xff;
        cpu.run();
        assert_eq!(cpu.register_x, 0);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn inx_negative_flag(mut cpu: CPU) {
        cpu.load(vec![0xe8, 0x00]);
        cpu.register_x = 0xfe;
        cpu.run();
        assert_eq!(cpu.register_x, 0xff);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    fn iny(mut cpu: CPU) {
        cpu.load(vec![0xc8, 0x00]);
        cpu.register_y = 10;
        cpu.run();
        assert_eq!(cpu.register_y, 11);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn iny_zero_flag(mut cpu: CPU) {
        cpu.load(vec![0xc8, 0x00]);
        cpu.register_y = 0xff;
        cpu.run();
        assert_eq!(cpu.register_y, 0);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn iny_negative_flag(mut cpu: CPU) {
        cpu.load(vec![0xc8, 0x00]);
        cpu.register_y = 0xfe;
        cpu.run();
        assert_eq!(cpu.register_y, 0xff);
        assert_eq!(cpu.status, 0b1000_0000);
    }
}
