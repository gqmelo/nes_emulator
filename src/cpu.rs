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
                0xAD => {
                    self.lda(&AddressingMode::Absolute);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::Absolute);
                }
                0x85 => {
                    self.sda(&AddressingMode::ZeroPage);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::ZeroPage);
                }
                0x95 => {
                    self.sda(&AddressingMode::ZeroPageX);
                    self.program_counter +=
                        self.get_program_counter_increment(&AddressingMode::ZeroPageX);
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
                _ => todo!(),
            }
        }
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn sda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
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
                    .mem_read_u16(self.program_counter)
                    .wrapping_add(self.register_x.into());
                self.mem_read_u16(ptr)
            }
            AddressingMode::IndirectY => {
                let ptr = self
                    .mem_read_u16(self.program_counter)
                    .wrapping_add(self.register_y.into());
                self.mem_read_u16(ptr)
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

    #[test]
    fn lda_immediate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 0x05);
        assert_eq!(cpu.status, 0x0000_0000);
    }

    #[test]
    fn lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[test]
    fn lda_negative_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xf0, 0x00]);
        assert_eq!(cpu.register_a, 0xf0);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[test]
    fn lda_zero_page() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 99);
        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);
        assert_eq!(cpu.register_a, 99);
        assert_eq!(cpu.status, 0x0000_0000);
    }

    #[test]
    fn lda_absolute() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x70e4, 99);
        cpu.load_and_run(vec![0xad, 0xe4, 0x70, 0x00]);
        assert_eq!(cpu.register_a, 99);
        assert_eq!(cpu.status, 0x0000_0000);
    }

    #[test]
    fn sda_zero_page() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x85, 0x10, 0x00]);
        cpu.reset();
        cpu.register_a = 5;
        cpu.run();
        assert_eq!(cpu.mem_read(0x10), 5);
    }

    #[test]
    fn sda_zero_page_x() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x95, 0x10, 0x00]);
        cpu.reset();
        cpu.register_a = 5;
        cpu.register_x = 2;
        cpu.run();
        assert_eq!(cpu.mem_read(0x12), 5);
    }

    #[test]
    fn tax() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0xaa, 0x00]);
        assert_eq!(cpu.register_x, 0x05);
        assert_eq!(cpu.status, 0x0000_0000);
    }

    #[test]
    fn tax_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0xaa, 0x00]);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[test]
    fn tax_negative_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xf0, 0xaa, 0x00]);
        assert_eq!(cpu.register_x, 0xf0);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[test]
    fn inx() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 10;
        cpu.run();
        assert_eq!(cpu.register_x, 11);
        assert_eq!(cpu.status, 0x0000_0000);
    }

    #[test]
    fn inx_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 0xff;
        cpu.run();
        assert_eq!(cpu.register_x, 0);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[test]
    fn inx_negative_flag() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 0xfe;
        cpu.run();
        assert_eq!(cpu.register_x, 0xff);
        assert_eq!(cpu.status, 0b1000_0000);
    }
}
