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

pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
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
            status: 0,
            program_counter: 0,
            memory: [0; 0xFFFF],
        }
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run();
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.register_y = 0;
        self.status = 0;
        self.program_counter = self.mem_read_u16(0xFFFC)
    }

    pub fn run(&mut self) {
        loop {
            println!(
                "a: {:#04x}; x: {:#04x}; y: {:#04x}",
                self.register_a, self.register_x, self.register_y
            );
            let opcode = self.mem_read(self.program_counter);
            self.program_counter += 1;

            match opcode {
                0xA9 => {
                    self.lda(&AddressingMode::Immediate);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::Immediate);
                }
                0xA5 => {
                    self.lda(&AddressingMode::ZeroPage);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::ZeroPage);
                }
                0xB5 => {
                    self.lda(&AddressingMode::ZeroPageX);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::ZeroPageX);
                }
                0xAD => {
                    self.lda(&AddressingMode::Absolute);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::Absolute);
                }
                0xBD => {
                    self.lda(&AddressingMode::AbsoluteX);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::AbsoluteX);
                }
                0xB9 => {
                    self.lda(&AddressingMode::AbsoluteY);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::AbsoluteY);
                }
                0xA1 => {
                    self.lda(&AddressingMode::IndirectX);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::IndirectX);
                }
                0xB1 => {
                    self.lda(&AddressingMode::IndirectY);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::IndirectY);
                }
                0xA2 => {
                    self.ldx(&AddressingMode::Immediate);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::Immediate);
                }
                0xA6 => {
                    self.ldx(&AddressingMode::ZeroPage);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::ZeroPage);
                }
                0xB6 => {
                    self.ldx(&AddressingMode::ZeroPageY);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::ZeroPageY);
                }
                0xAE => {
                    self.ldx(&AddressingMode::Absolute);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::Absolute);
                }
                0xBE => {
                    self.ldx(&AddressingMode::AbsoluteY);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::AbsoluteY);
                }
                0xA0 => {
                    self.ldy(&AddressingMode::Immediate);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::Immediate);
                }
                0xA4 => {
                    self.ldy(&AddressingMode::ZeroPage);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::ZeroPage);
                }
                0xB4 => {
                    self.ldy(&AddressingMode::ZeroPageX);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::ZeroPageX);
                }
                0xAC => {
                    self.ldy(&AddressingMode::Absolute);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::Absolute);
                }
                0xBC => {
                    self.ldy(&AddressingMode::AbsoluteX);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::AbsoluteX);
                }
                0x85 => {
                    self.sta(&AddressingMode::ZeroPage);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::ZeroPage);
                }
                0x95 => {
                    self.sta(&AddressingMode::ZeroPageX);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::ZeroPageX);
                }
                0x8D => {
                    self.sta(&AddressingMode::Absolute);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::Absolute);
                }
                0x9D => {
                    self.sta(&AddressingMode::AbsoluteX);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::AbsoluteX);
                }
                0x99 => {
                    self.sta(&AddressingMode::AbsoluteY);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::AbsoluteY);
                }
                0x81 => {
                    self.sta(&AddressingMode::IndirectX);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::IndirectX);
                }
                0x91 => {
                    self.sta(&AddressingMode::IndirectY);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::IndirectY);
                }
                0x86 => {
                    self.stx(&AddressingMode::ZeroPage);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::ZeroPage);
                }
                0x96 => {
                    self.stx(&AddressingMode::ZeroPageY);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::ZeroPageY);
                }
                0x8E => {
                    self.stx(&AddressingMode::Absolute);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::Absolute);
                }
                0x84 => {
                    self.sty(&AddressingMode::ZeroPage);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::ZeroPage);
                }
                0x94 => {
                    self.sty(&AddressingMode::ZeroPageX);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::ZeroPageX);
                }
                0x8C => {
                    self.sty(&AddressingMode::Absolute);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::Absolute);
                }
                0xAA => {
                    self.register_x = self.register_a;
                    self.update_zero_and_negative_flags(self.register_x);
                }
                0xE8 => {
                    self.register_x = self.register_x.wrapping_add(1);
                    self.update_zero_and_negative_flags(self.register_x);
                }
                0x00 => {
                    return;
                }
                _ => todo!("{:#04x}", opcode),
            }
        }
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
            AddressingMode::Immediate
            | AddressingMode::ZeroPage
            | AddressingMode::ZeroPageX
            | AddressingMode::ZeroPageY
            | AddressingMode::IndirectX
            | AddressingMode::IndirectY => 1,
            AddressingMode::Absolute | AddressingMode::AbsoluteX | AddressingMode::AbsoluteY => 2,
            AddressingMode::NoneAddressing => {
                panic!("mode {:?} not supported", mode);
            }
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
            self.status = self.status | 0b0000_0010;
        } else {
            self.status = self.status & 0b1111_1101;
        }

        if result & 0b1000_0000 != 0 {
            self.status = self.status | 0b1000_0000;
        } else {
            self.status = self.status & 0b0111_1111;
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
        assert_eq!(cpu.status, 0x0000_0000);
    }

    #[rstest]
    fn lda_indirect_x(mut cpu: CPU) {
        cpu.load(vec![0xa1, 0x01, 0x00]);
        cpu.reset();
        cpu.register_x = 0x01;
        cpu.mem_write_u16(0x0002, 0x70e4);
        cpu.mem_write(0x70e4, 0x05);
        cpu.run();
        assert_eq!(cpu.register_a, 0x05);
        assert_eq!(cpu.status, 0x0000_0000);
    }

    #[rstest]
    fn lda_indirect_y(mut cpu: CPU) {
        cpu.load(vec![0xb1, 0x02, 0x00]);
        cpu.reset();
        cpu.register_y = 0x01;
        cpu.mem_write_u16(0x02, 0x70e4);
        cpu.mem_write(0x70e5, 0x05);
        cpu.run();
        assert_eq!(cpu.register_a, 0x05);
        assert_eq!(cpu.status, 0x0000_0000);
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
        assert_eq!(cpu.status, 0x0000_0000);
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
        assert_eq!(cpu.status, 0x0000_0000);
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
    #[case(vec![0x85, 0x10, 0x00])] // sta $10
    #[case(vec![
        0xa2, 0x02, // ldx #$02
        0x95, 0x0e, // sta $0e
        0x00
    ])]
    fn sta_zero_page(mut cpu: CPU, #[case] program: Vec<u8>) {
        cpu.load(program);
        cpu.reset();
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
        cpu.reset();
        cpu.register_a = 5;
        cpu.run();
        assert_eq!(cpu.mem_read_u16(0x70e4), 5);
    }

    #[rstest]
    fn sta_indirect_x(mut cpu: CPU) {
        cpu.load(vec![0x81, 0x01, 0x00]);
        cpu.reset();
        cpu.register_a = 5;
        cpu.register_x = 0x01;
        cpu.mem_write_u16(0x0002, 0x70e4);
        cpu.run();

        assert_eq!(cpu.mem_read_u16(0x70e4), 5);
        assert_eq!(cpu.status, 0x0000_0000);
    }

    #[rstest]
    fn sta_indirect_y(mut cpu: CPU) {
        cpu.load(vec![0x91, 0x02, 0x00]);
        cpu.reset();
        cpu.register_a = 5;
        cpu.register_y = 0x01;
        cpu.mem_write_u16(0x02, 0x70e3);
        cpu.run();

        assert_eq!(cpu.mem_read_u16(0x70e4), 5);
        assert_eq!(cpu.status, 0x0000_0000);
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
        cpu.reset();
        cpu.register_x = 5;
        cpu.run();
        assert_eq!(cpu.mem_read(0x10), 5);
    }

    #[rstest]
    fn stx_absolute(mut cpu: CPU) {
        cpu.load(vec![0x8e, 0xe4, 0x70, 0x00]); // stx $70e4
        cpu.reset();
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
        cpu.reset();
        cpu.register_y = 5;
        cpu.run();
        assert_eq!(cpu.mem_read(0x10), 5);
    }

    #[rstest]
    fn sty_absolute(mut cpu: CPU) {
        cpu.load(vec![0x8c, 0xe4, 0x70, 0x00]); // sty $70e4
        cpu.reset();
        cpu.register_y = 5;
        cpu.run();
        assert_eq!(cpu.mem_read_u16(0x70e4), 5);
    }

    #[rstest]
    fn tax(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa9, 0x05, 0xaa, 0x00]);
        assert_eq!(cpu.register_x, 0x05);
        assert_eq!(cpu.status, 0x0000_0000);
    }

    #[rstest]
    fn tax_zero_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa9, 0x00, 0xaa, 0x00]);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn tax_negative_flag(mut cpu: CPU) {
        cpu.load_and_run(vec![0xa9, 0xf0, 0xaa, 0x00]);
        assert_eq!(cpu.register_x, 0xf0);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[rstest]
    fn inx(mut cpu: CPU) {
        cpu.load(vec![0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 10;
        cpu.run();
        assert_eq!(cpu.register_x, 11);
        assert_eq!(cpu.status, 0x0000_0000);
    }

    #[rstest]
    fn inx_zero_flag(mut cpu: CPU) {
        cpu.load(vec![0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 0xff;
        cpu.run();
        assert_eq!(cpu.register_x, 0);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[rstest]
    fn inx_negative_flag(mut cpu: CPU) {
        cpu.load(vec![0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 0xfe;
        cpu.run();
        assert_eq!(cpu.register_x, 0xff);
        assert_eq!(cpu.status, 0b1000_0000);
    }
}
