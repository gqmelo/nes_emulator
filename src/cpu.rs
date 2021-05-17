pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub status: u8,
    pub program_counter: u8,
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            register_x: 0,
            status: 0,
            program_counter: 0,
        }
    }

    pub fn interpret(&mut self, program: Vec<u8>) {
        loop {
            let opcode = program[self.program_counter as usize];
            self.program_counter += 1;

            match opcode {
                0xA9 => {
                    let param = program[self.program_counter as usize];
                    self.program_counter += 1;
                    self.register_a = param;
                    self.update_zero_and_negative_flags(self.register_a);
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
        cpu.interpret(vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 0x05);
        assert_eq!(cpu.status, 0x0000_0000);
    }

    #[test]
    fn lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0x00, 0x00]);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[test]
    fn lda_negative_flag() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0xf0, 0x00]);
        assert_eq!(cpu.register_a, 0xf0);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[test]
    fn tax() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0x05, 0xaa, 0x00]);
        assert_eq!(cpu.register_x, 0x05);
        assert_eq!(cpu.status, 0x0000_0000);
    }

    #[test]
    fn tax_zero_flag() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0x00, 0xaa, 0x00]);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[test]
    fn tax_negative_flag() {
        let mut cpu = CPU::new();
        cpu.interpret(vec![0xa9, 0xf0, 0xaa, 0x00]);
        assert_eq!(cpu.register_x, 0xf0);
        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[test]
    fn inx() {
        let mut cpu = CPU::new();
        cpu.register_x = 10;
        cpu.interpret(vec![0xe8, 0x00]);
        assert_eq!(cpu.register_x, 11);
        assert_eq!(cpu.status, 0x0000_0000);
    }

    #[test]
    fn inx_zero_flag() {
        let mut cpu = CPU::new();
        cpu.register_x = 0xff;
        cpu.interpret(vec![0xe8, 0x00]);
        assert_eq!(cpu.register_x, 0);
        assert_eq!(cpu.status, 0b0000_0010);
    }

    #[test]
    fn inx_negative_flag() {
        let mut cpu = CPU::new();
        cpu.register_x = 0xfe;
        cpu.interpret(vec![0xe8, 0x00]);
        assert_eq!(cpu.register_x, 0xff);
        assert_eq!(cpu.status, 0b1000_0000);
    }
}
