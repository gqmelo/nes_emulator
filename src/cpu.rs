use std::convert::TryInto;

use crate::opcodes;

#[derive(Debug)]
pub enum AddressingMode {
    Accumulator,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
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
    pub stack_pointer: u8,
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
            stack_pointer: 0xFF,
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
        self.stack_pointer = 0xFF;
        self.status = 0;
        self.program_counter = self.mem_read_u16(0xFFFC)
    }

    pub fn run(&mut self) {
        let ref opcodes = *opcodes::OPCODES_MAP;
        loop {
            let code = self.mem_read(self.program_counter);

            let opcode = opcodes
                .get(&code)
                .expect(&format!("invalid opcode: {:#04x}", code));

            println!(
                "pc: {:#06x}, instr: {} ({:#04x}), a: {:#04x}; x: {:#04x}; y: {:#04x}, stack: {:#02x}, status: {:#010b}",
                self.program_counter,
                opcode.mnemonic,
                code,
                self.register_a,
                self.register_x,
                self.register_y,
                self.stack_pointer,
                self.status
            );

            self.program_counter += 1;

            match opcode.mnemonic {
                "ADC" => self.adc(&opcode.mode),
                "AND" => self.and(&opcode.mode),
                "ASL" => self.asl(&opcode.mode),
                "BCC" => self.branch_status_flag_clear(StatusFlag::Carry),
                "BCS" => self.branch_status_flag_set(StatusFlag::Carry),
                "BEQ" => self.branch_status_flag_set(StatusFlag::Zero),
                "BIT" => self.bit(&opcode.mode),
                "BMI" => self.branch_status_flag_set(StatusFlag::Negative),
                "BNE" => self.branch_status_flag_clear(StatusFlag::Zero),
                "BPL" => self.branch_status_flag_clear(StatusFlag::Negative),
                "BRK" => return, // TODO: http://www.obelisk.me.uk/6502/reference.html#BRK
                "BVC" => self.branch_status_flag_clear(StatusFlag::Overflow),
                "BVS" => self.branch_status_flag_set(StatusFlag::Overflow),
                "CLC" => self.clear_status_flag(StatusFlag::Carry),
                "CLD" => self.clear_status_flag(StatusFlag::Decimal),
                "CLI" => self.clear_status_flag(StatusFlag::InterruptDisable),
                "CLV" => self.clear_status_flag(StatusFlag::Overflow),
                "CMP" => self.cmp(&opcode.mode),
                "CPX" => self.cpx(&opcode.mode),
                "CPY" => self.cpy(&opcode.mode),
                "DEC" => self.dec(&opcode.mode),
                "DEX" => self.dex(),
                "DEY" => self.dey(),
                "EOR" => self.eor(&opcode.mode),
                "INC" => self.inc(&opcode.mode),
                "INX" => self.inx(),
                "INY" => self.iny(),
                "JMP" => self.jmp(&opcode.mode),
                "JSR" => self.jsr(&opcode.mode),
                "LDA" => self.lda(&opcode.mode),
                "LDX" => self.ldx(&opcode.mode),
                "LDY" => self.ldy(&opcode.mode),
                "LSR" => self.lsr(&opcode.mode),
                "NOP" => continue,
                "ORA" => self.ora(&opcode.mode),
                "PHA" => self.pha(),
                "PHP" => self.php(),
                "PLA" => self.pla(),
                "PLP" => self.plp(),
                "ROL" => self.rol(&opcode.mode),
                "ROR" => self.ror(&opcode.mode),
                // "RTI" => self.rti(&opcode.mode),
                "RTS" => self.rts(),
                // "SBC" => self.sbc(&opcode.mode),
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

            match opcode.mnemonic {
                "JMP" => {}
                "JSR" => {}
                "RTS" => {}
                _ => self.program_counter += self.get_program_counter_increment(&opcode.mode),
            };
        }
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.add_to_register_a(value);
    }

    /// note: ignoring decimal mode
    /// http://www.righto.com/2012/12/the-6502-overflow-flag-explained.html
    fn add_to_register_a(&mut self, value: u8) {
        let sum = self.register_a as u16
            + value as u16
            + (if self.is_status_flag_set(StatusFlag::Carry) {
                1
            } else {
                0
            }) as u16;

        let carry = sum > 0xff;
        if carry {
            self.set_status_flag(StatusFlag::Carry)
        } else {
            self.clear_status_flag(StatusFlag::Carry)
        }

        let result = sum as u8;

        if (value ^ result) & (result ^ self.register_a) & 0x80 != 0 {
            self.set_status_flag(StatusFlag::Overflow);
        } else {
            self.clear_status_flag(StatusFlag::Overflow);
        }

        self.register_a = result;
        self.update_zero_and_negative_flags(self.register_a)
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.register_a = self.register_a & value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn branch_status_flag_clear(&mut self, status_flag: StatusFlag) {
        let addr = self.get_operand_address(&AddressingMode::Immediate);
        let value = self.mem_read(addr);
        let mask = 0b0000_0001 << status_flag as u8;
        if self.status & mask == 0 {
            if value > 0xf0 {
                let offset = 0xff - value;
                self.program_counter -= offset as u16;
            } else {
                self.program_counter += value as u16;
            }
        }
    }

    fn branch_status_flag_set(&mut self, status_flag: StatusFlag) {
        let addr = self.get_operand_address(&AddressingMode::Immediate);
        let value = self.mem_read(addr);
        let mask = 0b0000_0001 << status_flag as u8;
        if self.status & mask != 0 {
            if value > 0xf0 {
                let offset = 0xff - value;
                self.program_counter -= offset as u16;
            } else {
                self.program_counter += value as u16;
            }
        }
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

    fn is_status_flag_set(&mut self, status_flag: StatusFlag) -> bool {
        let mask = 0b0000_0001 << status_flag as u8;
        return self.status & mask != 0;
    }

    fn cmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.compare(self.register_a, value);
    }

    fn cpx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.compare(self.register_x, value);
    }

    fn cpy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.compare(self.register_y, value);
    }

    fn compare(&mut self, register: u8, value: u8) {
        if register == value {
            self.set_status_flag(StatusFlag::Zero);
        } else {
            self.clear_status_flag(StatusFlag::Zero);
        }

        if register >= value {
            self.set_status_flag(StatusFlag::Carry);
        } else {
            self.clear_status_flag(StatusFlag::Carry);
        }

        if register & 0b1000_0000 != 0 {
            self.set_status_flag(StatusFlag::Negative)
        } else {
            self.clear_status_flag(StatusFlag::Negative);
        }
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

    fn jmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.program_counter = addr;
    }

    fn jsr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let return_addr = self.program_counter + self.get_program_counter_increment(&mode);
        self.push_to_stack(((return_addr & 0xFF00) >> 8).try_into().unwrap());
        self.push_to_stack((return_addr & 0x00FF).try_into().unwrap());
        self.program_counter = addr;
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

    fn asl(&mut self, mode: &AddressingMode) {
        let value = self.get_shift_value(mode);

        if value & 0b1000_0000 > 0 {
            self.set_status_flag(StatusFlag::Carry);
        } else {
            self.clear_status_flag(StatusFlag::Carry);
        }
        let new_value = value << 1;
        self.update_zero_and_negative_flags(new_value);

        self.save_shift_value(mode, new_value);
    }

    fn lsr(&mut self, mode: &AddressingMode) {
        let value = self.get_shift_value(mode);

        if value & 0b0000_0001 > 0 {
            self.set_status_flag(StatusFlag::Carry);
        } else {
            self.clear_status_flag(StatusFlag::Carry);
        }
        let new_value = value >> 1;
        self.update_zero_and_negative_flags(new_value);

        self.save_shift_value(mode, new_value);
    }

    fn rol(&mut self, mode: &AddressingMode) {
        let old_carry = (0b0000_0001 << StatusFlag::Carry as u8) & self.status;
        let value = self.get_shift_value(mode);

        let should_set_carry = value & 0b1000_0000 > 0;
        let mut new_value = value << 1;
        if old_carry > 0 {
            new_value |= 0b0000_0001;
        }
        if should_set_carry {
            self.set_status_flag(StatusFlag::Carry);
        } else {
            self.clear_status_flag(StatusFlag::Carry);
        }
        self.update_zero_and_negative_flags(new_value);

        self.save_shift_value(mode, new_value);
    }

    fn ror(&mut self, mode: &AddressingMode) {
        let old_carry = (0b0000_0001 << StatusFlag::Carry as u8) & self.status;
        let value = self.get_shift_value(mode);

        let should_set_carry = value & 0b0000_0001 > 0;
        let mut new_value = value >> 1;
        if old_carry > 0 {
            new_value |= 0b1000_0000;
        }
        if should_set_carry {
            self.set_status_flag(StatusFlag::Carry);
        } else {
            self.clear_status_flag(StatusFlag::Carry);
        }
        self.update_zero_and_negative_flags(new_value);

        self.save_shift_value(mode, new_value);
    }

    fn get_shift_value(&mut self, mode: &AddressingMode) -> u8 {
        match mode {
            AddressingMode::Accumulator => self.register_a,
            _ => {
                let addr = self.get_operand_address(mode);
                self.mem_read(addr)
            }
        }
    }

    fn save_shift_value(&mut self, mode: &AddressingMode, new_value: u8) {
        match mode {
            AddressingMode::Accumulator => self.register_a = new_value,
            _ => {
                let addr = self.get_operand_address(mode);
                self.mem_write(addr, new_value);
            }
        };
    }

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.register_a = self.register_a | value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn rts(&mut self) {
        let low_byte = self.pop_from_stack();
        let high_byte = self.pop_from_stack();
        let return_addr = ((high_byte as u16) << 8) | low_byte as u16;
        self.program_counter = return_addr;
    }

    fn pha(&mut self) {
        self.push_to_stack(self.register_a);
    }

    fn php(&mut self) {
        self.push_to_stack(self.status);
    }

    fn push_to_stack(&mut self, value: u8) {
        let addr: u16 = 0x100 as u16 + self.stack_pointer as u16;
        self.mem_write(addr, value);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn pla(&mut self) {
        self.register_a = self.pop_from_stack();
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn plp(&mut self) {
        self.status = self.pop_from_stack();
    }

    fn pop_from_stack(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        let addr: u16 = 0x100 as u16 + self.stack_pointer as u16;
        return self.mem_read(addr);
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
        self.register_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn txs(&mut self) {
        self.stack_pointer = self.register_x;
        self.update_zero_and_negative_flags(self.stack_pointer);
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
            AddressingMode::Indirect => {
                let ptr = self.mem_read_u16(self.program_counter);
                self.mem_read_u16(ptr.into())
            }
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
            _ => {
                panic!("mode {:?} not supported", mode);
            }
        }
    }

    fn get_program_counter_increment(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Accumulator | AddressingMode::NoneAddressing => 0,
            AddressingMode::Immediate
            | AddressingMode::ZeroPage
            | AddressingMode::ZeroPageX
            | AddressingMode::ZeroPageY
            | AddressingMode::IndirectX
            | AddressingMode::IndirectY => 1,
            AddressingMode::Absolute
            | AddressingMode::AbsoluteX
            | AddressingMode::AbsoluteY
            | AddressingMode::Indirect => 2,
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
            self.set_status_flag(StatusFlag::Zero)
        } else {
            self.clear_status_flag(StatusFlag::Zero)
        }

        if result & 0b1000_0000 != 0 {
            self.set_status_flag(StatusFlag::Negative)
        } else {
            self.clear_status_flag(StatusFlag::Negative)
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
    #[case(vec![
        0xa9, 0x70, // lda #$70
        0x69, 0x09, // adc #$09
    ])]
    #[case(vec![
        0xa9, 0x70, // lda #$70
        0xa2, 0x09, // ldx #$09
        0x86, 0x10, // stx $10
        0x65, 0x10, // adc $10
        0x00
    ])]
    #[case(vec![
        0xa9, 0x70, // lda #$70
        0xa2, 0x09, // ldx #$09
        0x86, 0x15, // stx $15
        0xa2, 0x05, // ldx #$05
        0x75, 0x10, // adc $10,X
        0x00
    ])]
    #[case(vec![
        0xa9, 0x70, // lda #$70
        0xa2, 0x09, // ldx #$09
        0x8e, 0xe4, 0x70, // stx $70e4
        0x6d, 0xe4, 0x70, // adc $70e4
        0x00
    ])]
    #[case(vec![
        0xa9, 0x70, // lda #$70
        0xa2, 0x09, // ldx #$09
        0x8e, 0xe9, 0x70, // stx $70e9
        0xa2, 0x05, // ldx #$05
        0x7d, 0xe4, 0x70, // adc $70e4,X
        0x00
    ])]
    #[case(vec![
        0xa9, 0x70, // lda #$70
        0xa2, 0x09, // ldx #$09
        0x8e, 0xe9, 0x70, // stx $70e9
        0xa0, 0x05, // ldy #$05
        0x79, 0xe4, 0x70, // adc $70e4,Y
        0x00
    ])]
    fn adc(mut cpu: CPU, #[case] program: Vec<u8>) {
        cpu.load(program);
        cpu.run();
        assert_eq!(cpu.register_a, 0x79);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn adc_indirect_x(mut cpu: CPU) {
        cpu.load(vec![0x61, 0x01, 0x00]);
        cpu.register_a = 0x70;
        cpu.register_x = 0x01;
        cpu.mem_write_u16(0x0002, 0x70e4);
        cpu.mem_write(0x70e4, 0x09);
        cpu.run();
        assert_eq!(cpu.register_a, 0x79);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn adc_indirect_y(mut cpu: CPU) {
        cpu.load(vec![0x71, 0x02, 0x00]);
        cpu.register_a = 0x70;
        cpu.register_y = 0x01;
        cpu.mem_write_u16(0x02, 0x70e4);
        cpu.mem_write(0x70e5, 0x09);
        cpu.run();
        assert_eq!(cpu.register_a, 0x79);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn adc_positive_overflow(mut cpu: CPU) {
        cpu.load(vec![
            0xa9, 0x7f, // lda #$7F
            0x69, 0x01, // adc #$01
        ]);
        cpu.run();
        assert_eq!(cpu.register_a, 0x80);
        assert_eq!(cpu.status, 0b1100_0000);
    }

    #[rstest]
    fn adc_negative_overflow(mut cpu: CPU) {
        cpu.load(vec![
            0xa9, 0x80, // lda #$80
            0x69, 0xff, // adc #$ff
        ]);
        cpu.run();
        assert_eq!(cpu.register_a, 0x7f);
        assert_eq!(cpu.status, 0b0100_0001);
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
        0x39, 0xe4, 0x70, // and $70e4,Y
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
    #[case(0x90, 0b1111_1110)] // bcc
    #[case(0x90, 0b0000_0000)] // bcc
    #[case(0xb0, 0b1111_1111)] // bcs
    #[case(0xb0, 0b0000_0001)] // bcs
    #[case(0xf0, 0b1111_1111)] // beq
    #[case(0xf0, 0b0000_0010)] // beq
    #[case(0x30, 0b1111_1111)] // bmi
    #[case(0x30, 0b1000_0000)] // bmi
    #[case(0xd0, 0b1111_1101)] // bne
    #[case(0xd0, 0b0000_0000)] // bne
    #[case(0x10, 0b0111_1111)] // bpl
    #[case(0x10, 0b0000_0000)] // bpl
    #[case(0x50, 0b1011_1111)] // bvc
    #[case(0x50, 0b0000_0000)] // bvc
    #[case(0x70, 0b1111_1111)] // bvs
    #[case(0x70, 0b0100_0000)] // bvs
    fn branch_forward(mut cpu: CPU, #[case] branch_instruction: u8, #[case] initial_status: u8) {
        #[rustfmt::skip]
        let program = vec![
            branch_instruction, 0x02,
            0xe8, // inx
            0xe8, // inx
            0xe8, // inx
            0x00,
        ];
        cpu.load(program);
        cpu.status = initial_status;
        cpu.register_x = 0x05;
        cpu.run();
        assert_eq!(cpu.register_x, 0x06);
    }

    #[rstest]
    #[case(0x90, 0b1111_1110)] // bcc
    #[case(0x90, 0b0000_0000)] // bcc
    #[case(0xb0, 0b1111_1111)] // bcs
    #[case(0xb0, 0b0000_0001)] // bcs
    #[case(0xf0, 0b1111_1111)] // beq
    #[case(0xf0, 0b0000_0010)] // beq
    #[case(0x30, 0b1111_1111)] // bmi
    #[case(0x30, 0b1000_0000)] // bmi
    #[case(0xd0, 0b1111_1101)] // bne
    #[case(0xd0, 0b0000_0000)] // bne
    #[case(0x10, 0b0111_1111)] // bpl
    #[case(0x10, 0b0000_0000)] // bpl
    #[case(0x50, 0b1011_1111)] // bvc
    #[case(0x50, 0b0000_0000)] // bvc
    #[case(0x70, 0b1111_1111)] // bvs
    #[case(0x70, 0b0100_0000)] // bvs
    fn branch_backward(mut cpu: CPU, #[case] branch_instruction: u8, #[case] initial_status: u8) {
        #[rustfmt::skip]
        let program = vec![
            0x4c, 0x09, 0x80, // jmp $8009
            0xe8, // inx
            0xe8, // inx
            0xe8, // inx
            0x4c, 0x0b, 0x80, // jmp $800b
            branch_instruction, 0xf7, // Goes back 8 bytes (6 bytes before instruction addr)
            0x00,
        ];
        cpu.load(program);
        cpu.status = initial_status;
        cpu.register_x = 0x05;
        cpu.run();
        assert_eq!(cpu.register_x, 0x08);
    }

    #[rstest]
    #[case(0x90, 0b1111_1111)] // bcc
    #[case(0x90, 0b0000_0001)] // bcc
    #[case(0xb0, 0b1111_1110)] // bcs
    #[case(0xb0, 0b0000_0000)] // bcs
    #[case(0xf0, 0b1111_1101)] // beq
    #[case(0xf0, 0b0000_0000)] // beq
    #[case(0x30, 0b0111_1111)] // bmi
    #[case(0x30, 0b0000_0000)] // bmi
    #[case(0xd0, 0b1111_1111)] // bne
    #[case(0xd0, 0b0000_0010)] // bne
    #[case(0x10, 0b1111_1111)] // bpl
    #[case(0x10, 0b1000_0000)] // bpl
    #[case(0x50, 0b1111_1111)] // bvc
    #[case(0x50, 0b0100_0000)] // bvc
    #[case(0x70, 0b1011_1111)] // bvs
    #[case(0x70, 0b0000_0000)] // bvs
    fn branch_not_taken(mut cpu: CPU, #[case] branch_instruction: u8, #[case] initial_status: u8) {
        #[rustfmt::skip]
        let program = vec![
            branch_instruction, 0x02,
            0xe8, // inx
            0xe8, // inx
            0xe8, // inx
            0x00,
        ];
        cpu.load(program);
        cpu.status = initial_status;
        cpu.register_x = 0x05;
        cpu.run();
        assert_eq!(cpu.register_x, 0x08);
    }

    #[rstest]
    fn jmp_absolute(mut cpu: CPU) {
        #[rustfmt::skip]
        let program = vec![
            0xa2, 0x05, // ldx #$05
            0x4c, 0x07, 0x80, // jmp $8007
            0xe8, // inx
            0xe8, // inx
            0xe8, // inx
            0x00,
        ];
        cpu.load_and_run(program);
        assert_eq!(cpu.register_x, 0x06);
    }

    #[rstest]
    fn jmp_indirect(mut cpu: CPU) {
        #[rustfmt::skip]
        let program = vec![
            0xa2, 0x05, // ldx #$05
            0x6c, 0xe4, 0x70, // jmp ($70e4)
            0xe8, // inx
            0xe8, // inx
            0xe8, // inx
            0x00,
        ];
        cpu.load(program);
        cpu.mem_write_u16(0x70e4, 0x8007);
        cpu.run();
        assert_eq!(cpu.register_x, 0x06);
    }

    #[rstest]
    fn jsr_and_rts(mut cpu: CPU) {
        #[rustfmt::skip]
        let program = vec![
            0xa2, 0x05, // ldx #$05
            0x20, 0x0b, 0x80, // jsr $800b
            0x4c, 0x0f, 0x80, // jmp $800f
            0xe8, // inx
            0xe8, // inx
            0xe8, // inx
            0xa0, 0x10, // ldy #$10
            0xe8, // inx
            0x60, // rts
            0x00,
        ];
        cpu.load_and_run(program);
        assert_eq!(cpu.register_x, 0x06);
        assert_eq!(cpu.register_y, 0x10);
    }

    #[rstest]
    #[case(vec![
        0xa9, 0x05,  // lda #$05
        0xc9, 0xff, // cmp #$ff (placeholder)
        0x00
    ])]
    #[case(vec![
        0x85, 0x10, // sta $10
        0xa9, 0x05,  // lda #$05
        0xc5, 0x10, // cmp $10
        0x00
    ])]
    #[case(vec![
        0x85, 0x10, // sta $10
        0xa9, 0x05,  // lda #$05
        0xa2, 0x02, // ldx #$02
        0xd5, 0x0e, // cmp $0e,X
        0x00
    ])]
    #[case(vec![
        0x8d, 0xe4, 0x70, // sta $70e4
        0xa9, 0x05,  // lda #$05
        0xcd, 0xe4, 0x70, // cmp $70e4
        0x00
    ])]
    #[case(vec![
        0x8d, 0xe9, 0x70, // sta $70e9
        0xa9, 0x05,  // lda #$05
        0xa2, 0x05, // ldx #$05
        0xdd, 0xe4, 0x70, // cmp $70e4,X
        0x00
    ])]
    #[case(vec![
        0x8d, 0xe9, 0x70, // sta $70e9
        0xa9, 0x05,  // lda #$05
        0xa0, 0x05, // ldy #$05
        0xd9, 0xe4, 0x70, // cmp $70e4,Y
        0x00
    ])]
    #[case(vec![
        0xa2, 0x05,  // ldx #$05
        0xe0, 0xff, // cpx #$ff (placeholder)
        0x00
    ])]
    #[case(vec![
        0x85, 0x10, // sta $10
        0xa2, 0x05,  // ldx #$05
        0xe4, 0x10, // cpx $10
        0x00
    ])]
    #[case(vec![
        0x8d, 0xe4, 0x70, // sta $70e4
        0xa2, 0x05,  // ldx #$05
        0xec, 0xe4, 0x70, // cmp $70e4
        0x00
    ])]
    #[case(vec![
        0xa0, 0x05,  // ldy #$05
        0xc0, 0xff, // cpy #$ff (placeholder)
        0x00
    ])]
    #[case(vec![
        0x85, 0x10, // sta $10
        0xa0, 0x05,  // ldy #$05
        0xc4, 0x10, // cpy $10
        0x00
    ])]
    #[case(vec![
        0x8d, 0xe4, 0x70, // sta $70e4
        0xa0, 0x05,  // ldy #$05
        0xcc, 0xe4, 0x70, // cpy $70e4
        0x00
    ])]
    fn cmp(
        mut cpu: CPU,
        #[case] program: Vec<u8>,
        #[rustfmt::skip]
        #[values(0b0000_0000, 0b0000_0001, 0b0000_0011, 0b0000_0010, 0b1000_0011)]
        initial_status: u8,
        #[values(
            (0x05, 0b0000_0011), // a == M
            (0x04, 0b0000_0001), // a >= M
            (0x06, 0b0000_0000), // a < M
        )]
        value_and_expected_status: (u8, u8),
    ) {
        let (value_to_compare, expected_status) = value_and_expected_status;
        let program = program
            .iter()
            .map(|&x| if x == 0xff { value_to_compare } else { x })
            .collect();
        cpu.load(program);
        cpu.register_a = value_to_compare;
        cpu.status = initial_status;
        cpu.run();
        assert_eq!(cpu.status, expected_status);
    }

    #[rstest]
    fn cmp_indirect_x(
        mut cpu: CPU,
        #[values(
            (0x05, 0b0000_0011), // a == M
            (0x04, 0b0000_0001), // a >= M
            (0x06, 0b0000_0000), // a < M
        )]
        value_and_status: (u8, u8),
    ) {
        let (value_to_compare, expected_status) = value_and_status;
        cpu.load(vec![0xc1, 0x01, 0x00]);
        cpu.register_a = 0x05;
        cpu.register_x = 0x01;
        cpu.mem_write_u16(0x0002, 0x70e4);
        cpu.mem_write(0x70e4, value_to_compare);
        cpu.run();
        assert_eq!(cpu.register_a, 0x05);
        assert_eq!(cpu.status, expected_status);
    }

    #[rstest]
    fn cmp_indirect_y(
        mut cpu: CPU,
        #[values(
            (0x05, 0b0000_0011), // a == M
            (0x04, 0b0000_0001), // a >= M
            (0x06, 0b0000_0000), // a < M
        )]
        value_and_status: (u8, u8),
    ) {
        let (value_to_compare, expected_status) = value_and_status;
        cpu.load(vec![0xd1, 0x02, 0x00]);
        cpu.register_a = 0x05;
        cpu.register_y = 0x01;
        cpu.mem_write_u16(0x02, 0x70e4);
        cpu.mem_write(0x70e5, value_to_compare);
        cpu.run();
        assert_eq!(cpu.register_a, 0x05);
        assert_eq!(cpu.status, expected_status);
    }

    #[rstest]
    fn cmp_negative_flag(
        mut cpu: CPU,
        #[values(
            (0xf5, 0b1000_0011), // a == M
            (0xf4, 0b1000_0001), // a >= M
            (0xf6, 0b1000_0000), // a < M
        )]
        value_and_status: (u8, u8),
    ) {
        let (value_to_compare, expected_status) = value_and_status;
        cpu.load(vec![0xc9, value_to_compare, 0x00]);
        cpu.register_a = 0xf5;
        cpu.run();
        assert_eq!(cpu.register_a, 0xf5);
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
    fn asl(
        mut cpu: CPU,
        #[values(
            // initial_value, expected_value, initial_status, expected_status
            (0b1111_1111, 0b1111_1110, 0b0000_0000, 0b1000_0001),
            (0b1111_0000, 0b1110_0000, 0b0000_0000, 0b1000_0001),
            (0b1000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0011),
            (0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0010),
            (0b1111_1111, 0b1111_1110, 0b1111_1111, 0b1111_1101),
            (0b1111_0000, 0b1110_0000, 0b1111_1111, 0b1111_1101),
            (0b0000_0000, 0b0000_0000, 0b1111_1111, 0b0111_1110),
        )]
        initial_and_expected: (u8, u8, u8, u8),
    ) {
        let (initial_value, expected_value, initial_status, expected_status) = initial_and_expected;
        cpu.load(vec![0x0a, 0x00]);
        cpu.register_a = initial_value;
        cpu.status = initial_status;
        cpu.run();
        assert_eq!(cpu.register_a, expected_value);
        assert_eq!(cpu.status, expected_status);
    }

    #[rstest]
    #[case(vec![0x06, 0x10, 0x00], 0x10)] // asl $10
    #[case(vec![
        0xa2, 0x08, // ldx #$08
        0x16, 0x08, // asl $08,X
        0x00
    ], 0x10)]
    #[case(vec![0x0e, 0xe4, 0x70, 0x00], 0x70e4)] // asl $70e4
    #[case(vec![
        0xa2, 0x05, // ldx #$05
        0x1e, 0xe4, 0x70, // asl $70e4,X
        0x00
    ], 0x70e9)]
    fn asl_memory(
        mut cpu: CPU,
        #[case] program: Vec<u8>,
        #[case] mem_addr: u16,
        #[values(
            // initial_value, expected_value, initial_status, expected_status
            (0b1111_1111, 0b1111_1110, 0b0000_0000, 0b1000_0001),
            (0b1111_0000, 0b1110_0000, 0b0000_0000, 0b1000_0001),
            (0b1000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0011),
            (0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0010),
            (0b1111_1111, 0b1111_1110, 0b1111_1111, 0b1111_1101),
            (0b1111_0000, 0b1110_0000, 0b1111_1111, 0b1111_1101),
            (0b0000_0000, 0b0000_0000, 0b1111_1111, 0b0111_1110),
        )]
        initial_and_expected: (u8, u8, u8, u8),
    ) {
        let (initial_value, expected_value, initial_status, expected_status) = initial_and_expected;
        cpu.load(program);
        cpu.mem_write(mem_addr, initial_value);
        cpu.status = initial_status;
        cpu.run();
        assert_eq!(cpu.mem_read(mem_addr), expected_value);
        assert_eq!(cpu.status, expected_status);
    }

    #[rstest]
    fn lsr(
        mut cpu: CPU,
        #[values(
            // initial_value, expected_value, initial_status, expected_status
            (0b1111_1111, 0b0111_1111, 0b0000_0000, 0b0000_0001),
            (0b0000_1111, 0b0000_0111, 0b0000_0000, 0b0000_0001),
            (0b0000_0001, 0b0000_0000, 0b0000_0000, 0b0000_0011),
            (0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0010),
            (0b1111_1111, 0b0111_1111, 0b1111_1111, 0b0111_1101),
            (0b0000_1111, 0b0000_0111, 0b1111_1111, 0b0111_1101),
            (0b0000_0000, 0b0000_0000, 0b1111_1111, 0b0111_1110),
        )]
        initial_and_expected: (u8, u8, u8, u8),
    ) {
        let (initial_value, expected_value, initial_status, expected_status) = initial_and_expected;
        cpu.load(vec![0x4a, 0x00]);
        cpu.register_a = initial_value;
        cpu.status = initial_status;
        cpu.run();
        assert_eq!(cpu.register_a, expected_value);
        assert_eq!(cpu.status, expected_status);
    }

    #[rstest]
    #[case(vec![0x46, 0x10, 0x00], 0x10)] // lsr $10
    #[case(vec![
        0xa2, 0x08, // ldx #$08
        0x56, 0x08, // lsr $08,X
        0x00
    ], 0x10)]
    #[case(vec![0x4e, 0xe4, 0x70, 0x00], 0x70e4)] // lsr $70e4
    #[case(vec![
        0xa2, 0x05, // ldx #$05
        0x5e, 0xe4, 0x70, // lsr $70e4,X
        0x00
    ], 0x70e9)]
    fn lsr_memory(
        mut cpu: CPU,
        #[case] program: Vec<u8>,
        #[case] mem_addr: u16,
        #[values(
            // initial_value, expected_value, initial_status, expected_status
            (0b1111_1111, 0b0111_1111, 0b0000_0000, 0b0000_0001),
            (0b0000_1111, 0b0000_0111, 0b0000_0000, 0b0000_0001),
            (0b0000_0001, 0b0000_0000, 0b0000_0000, 0b0000_0011),
            (0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0010),
            (0b1111_1111, 0b0111_1111, 0b1111_1111, 0b0111_1101),
            (0b0000_1111, 0b0000_0111, 0b1111_1111, 0b0111_1101),
            (0b0000_0000, 0b0000_0000, 0b1111_1111, 0b0111_1110),
        )]
        initial_and_expected: (u8, u8, u8, u8),
    ) {
        let (initial_value, expected_value, initial_status, expected_status) = initial_and_expected;
        cpu.load(program);
        cpu.mem_write(mem_addr, initial_value);
        cpu.status = initial_status;
        cpu.run();
        assert_eq!(cpu.mem_read(mem_addr), expected_value);
        assert_eq!(cpu.status, expected_status);
    }

    #[rstest]
    fn rol(
        mut cpu: CPU,
        #[values(
            // initial_value, expected_value, initial_status, expected_status
            (0b1111_1111, 0b1111_1110, 0b0000_0000, 0b1000_0001),
            (0b0000_1111, 0b0001_1110, 0b0000_0000, 0b0000_0000),
            (0b1111_0000, 0b1110_0000, 0b0000_0000, 0b1000_0001),
            (0b1000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0011),
            (0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0010),
            (0b1111_1111, 0b1111_1111, 0b1111_1111, 0b1111_1101),
            (0b0000_1111, 0b0001_1111, 0b1111_1111, 0b0111_1100),
            (0b1111_0000, 0b1110_0001, 0b1111_1111, 0b1111_1101),
            (0b0000_0000, 0b0000_0001, 0b1111_1111, 0b0111_1100),
        )]
        initial_and_expected: (u8, u8, u8, u8),
    ) {
        let (initial_value, expected_value, initial_status, expected_status) = initial_and_expected;
        cpu.load(vec![0x2a, 0x00]);
        cpu.register_a = initial_value;
        cpu.status = initial_status;
        cpu.run();
        assert_eq!(cpu.register_a, expected_value);
        assert_eq!(cpu.status, expected_status);
    }

    #[rstest]
    #[case(vec![0x26, 0x10, 0x00], 0x10)] // rol $10
    #[case(vec![
        0xa2, 0x08, // ldx #$08
        0x36, 0x08, // rol $08,X
        0x00
    ], 0x10)]
    #[case(vec![0x2e, 0xe4, 0x70, 0x00], 0x70e4)] // rol $70e4
    #[case(vec![
        0xa2, 0x05, // ldx #$05
        0x3e, 0xe4, 0x70, // rol $70e4,X
        0x00
    ], 0x70e9)]
    fn rol_memory(
        mut cpu: CPU,
        #[case] program: Vec<u8>,
        #[case] mem_addr: u16,
        #[values(
            // initial_value, expected_value, initial_status, expected_status
            (0b1111_1111, 0b1111_1110, 0b0000_0000, 0b1000_0001),
            (0b0000_1111, 0b0001_1110, 0b0000_0000, 0b0000_0000),
            (0b1111_0000, 0b1110_0000, 0b0000_0000, 0b1000_0001),
            (0b1000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0011),
            (0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0010),
            (0b1111_1111, 0b1111_1111, 0b1111_1111, 0b1111_1101),
            (0b0000_1111, 0b0001_1111, 0b1111_1111, 0b0111_1100),
            (0b1111_0000, 0b1110_0001, 0b1111_1111, 0b1111_1101),
            (0b0000_0000, 0b0000_0001, 0b1111_1111, 0b0111_1100),
        )]
        initial_and_expected: (u8, u8, u8, u8),
    ) {
        let (initial_value, expected_value, initial_status, expected_status) = initial_and_expected;
        cpu.load(program);
        cpu.mem_write(mem_addr, initial_value);
        cpu.status = initial_status;
        cpu.run();
        assert_eq!(cpu.mem_read(mem_addr), expected_value);
        assert_eq!(cpu.status, expected_status);
    }

    #[rstest]
    fn ror(
        mut cpu: CPU,
        #[values(
            // initial_value, expected_value, initial_status, expected_status
            (0b1111_1111, 0b0111_1111, 0b0000_0000, 0b0000_0001),
            (0b0000_1111, 0b0000_0111, 0b0000_0000, 0b0000_0001),
            (0b1111_0000, 0b0111_1000, 0b0000_0000, 0b0000_0000),
            (0b0000_0001, 0b0000_0000, 0b0000_0000, 0b0000_0011),
            (0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0010),
            (0b1111_1111, 0b1111_1111, 0b1111_1111, 0b1111_1101),
            (0b0000_1111, 0b1000_0111, 0b1111_1111, 0b1111_1101),
            (0b1111_0000, 0b1111_1000, 0b1111_1111, 0b1111_1100),
            (0b0000_0000, 0b1000_0000, 0b1111_1111, 0b1111_1100),
        )]
        initial_and_expected: (u8, u8, u8, u8),
    ) {
        let (initial_value, expected_value, initial_status, expected_status) = initial_and_expected;
        cpu.load(vec![0x6a, 0x00]);
        cpu.register_a = initial_value;
        cpu.status = initial_status;
        cpu.run();
        assert_eq!(cpu.register_a, expected_value);
        assert_eq!(cpu.status, expected_status);
    }

    #[rstest]
    #[case(vec![0x66, 0x10, 0x00], 0x10)] // ror $10
    #[case(vec![
        0xa2, 0x08, // ldx #$08
        0x76, 0x08, // ror $08,X
        0x00
    ], 0x10)]
    #[case(vec![0x6e, 0xe4, 0x70, 0x00], 0x70e4)] // ror $70e4
    #[case(vec![
        0xa2, 0x05, // ldx #$05
        0x7e, 0xe4, 0x70, // ror $70e4,X
        0x00
    ], 0x70e9)]
    fn ror_memory(
        mut cpu: CPU,
        #[case] program: Vec<u8>,
        #[case] mem_addr: u16,
        #[values(
            // initial_value, expected_value, initial_status, expected_status
            (0b1111_1111, 0b0111_1111, 0b0000_0000, 0b0000_0001),
            (0b0000_1111, 0b0000_0111, 0b0000_0000, 0b0000_0001),
            (0b1111_0000, 0b0111_1000, 0b0000_0000, 0b0000_0000),
            (0b0000_0001, 0b0000_0000, 0b0000_0000, 0b0000_0011),
            (0b0000_0000, 0b0000_0000, 0b0000_0000, 0b0000_0010),
            (0b1111_1111, 0b1111_1111, 0b1111_1111, 0b1111_1101),
            (0b0000_1111, 0b1000_0111, 0b1111_1111, 0b1111_1101),
            (0b1111_0000, 0b1111_1000, 0b1111_1111, 0b1111_1100),
            (0b0000_0000, 0b1000_0000, 0b1111_1111, 0b1111_1100),
        )]
        initial_and_expected: (u8, u8, u8, u8),
    ) {
        let (initial_value, expected_value, initial_status, expected_status) = initial_and_expected;
        cpu.load(program);
        cpu.mem_write(mem_addr, initial_value);
        cpu.status = initial_status;
        cpu.run();
        assert_eq!(cpu.mem_read(mem_addr), expected_value);
        assert_eq!(cpu.status, expected_status);
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
    fn pha(mut cpu: CPU) {
        cpu.load(vec![0x48, 0x00]);
        cpu.register_a = 0x05;
        cpu.run();
        assert_eq!(cpu.stack_pointer, 0xfe);
        assert_eq!(cpu.mem_read(0x01ff), 0x05);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn php(mut cpu: CPU) {
        cpu.load(vec![0x08, 0x00]);
        cpu.status = 0b1101_0010;
        cpu.run();
        assert_eq!(cpu.stack_pointer, 0xfe);
        assert_eq!(cpu.mem_read(0x01ff), 0b1101_0010);
        assert_eq!(cpu.status, 0b1101_0010);
    }

    #[rstest]
    fn pla(mut cpu: CPU) {
        cpu.load(vec![0x68, 0x00]);
        cpu.stack_pointer = 0xfe;
        cpu.mem_write(0x1ff, 0x05);
        cpu.run();
        assert_eq!(cpu.stack_pointer, 0xff);
        assert_eq!(cpu.register_a, 0x05);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn pla_zero_flag(mut cpu: CPU) {
        cpu.load(vec![0x68, 0x00]);
        cpu.stack_pointer = 0xfe;
        cpu.mem_write(0x1ff, 0x00);
        cpu.run();
        assert_eq!(cpu.stack_pointer, 0xff);
        assert_eq!(cpu.register_a, 0x00);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn pla_negative_flag(mut cpu: CPU) {
        cpu.load(vec![0x68, 0x00]);
        cpu.stack_pointer = 0xfe;
        cpu.mem_write(0x1ff, 0x95);
        cpu.run();
        assert_eq!(cpu.stack_pointer, 0xff);
        assert_eq!(cpu.register_a, 0x95);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    fn plp(mut cpu: CPU) {
        cpu.load(vec![0x28, 0x00]);
        cpu.stack_pointer = 0xfe;
        cpu.mem_write(0x1ff, 0b1101_0010);
        cpu.run();
        assert_eq!(cpu.stack_pointer, 0xff);
        assert_eq!(cpu.status, 0b1101_0010);
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
        cpu.stack_pointer = 5;
        cpu.run();
        assert_eq!(cpu.register_x, 0x05);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn tsx_zero_flag(mut cpu: CPU) {
        cpu.load(vec![0xba, 0x00]);
        cpu.stack_pointer = 0;
        cpu.run();
        assert_eq!(cpu.register_x, 0x00);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn tsx_negative_flag(mut cpu: CPU) {
        cpu.load(vec![0xba, 0x00]);
        cpu.stack_pointer = 0xf0;
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
        assert_eq!(cpu.stack_pointer, 0x05);
        assert_eq!(cpu.status, 0b0000_0000);
    }

    #[rstest]
    fn txs_zero_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa2, 0x00, 0x9a, 0x00]);
        assert_eq!(cpu.stack_pointer, 0x00);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn txs_negative_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa2, 0xf0, 0x9a, 0x00]);
        assert_eq!(cpu.stack_pointer, 0xf0);
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

    #[rstest]
    fn nop(mut cpu: CPU) {
        cpu.load(vec![0xea, 0x00]);
        cpu.register_a = 0x05;
        cpu.register_x = 0x1f;
        cpu.register_y = 0xfe;
        cpu.status = 0b1101_0010;
        cpu.run();
        assert_eq!(cpu.program_counter, 0x8002);
        assert_eq!(cpu.register_a, 0x05);
        assert_eq!(cpu.register_x, 0x1f);
        assert_eq!(cpu.register_y, 0xfe);
        assert_eq!(cpu.status, 0b1101_0010);
    }
}
