const RAM_START: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;

pub trait Mem {
    fn mem_read(&self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, data: u8);
    fn mem_read_u16(&self, addr: u16) -> u16;
    fn mem_write_u16(&mut self, addr: u16, data: u16);
}

pub struct Bus {
    cpu_vram: [u8; 2048],
}

impl Bus {
    pub fn new() -> Self {
        Bus {
            cpu_vram: [0; 2048],
        }
    }
}

impl Mem for Bus {
    fn mem_read(&self, addr: u16) -> u8 {
        match addr {
            RAM_START..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0000_0111_1111_1111;
                self.cpu_vram[mirror_down_addr as usize]
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr & 0b0000_0100_0000_0111;
                todo!("PPU is not supported yet");
            }
            _ => {
                println!("Ignoring memory read access at {:#04x}", addr);
                0
            }
        }
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        match addr {
            RAM_START..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0000_0111_1111_1111;
                self.cpu_vram[mirror_down_addr as usize] = data
            }
            PPU_REGISTERS..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr & 0b0000_0100_0000_0111;
                todo!("PPU is not supported yet");
            }
            _ => {
                println!("Ignoring memory write access at {:#04x}", addr);
            }
        }
    }

    fn mem_read_u16(&self, addr: u16) -> u16 {
        u16::from_le_bytes([self.mem_read(addr as u16), self.mem_read((addr + 1) as u16)])
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        self.mem_write(addr, (data & 0xff) as u8);
        self.mem_write(addr + 1, (data >> 8) as u8);
    }
}
