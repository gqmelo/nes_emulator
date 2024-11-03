use crate::{cartridge::Rom, ppu::PPU};

const RAM_START: u16 = 0x0000;
const RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;
const PRG_ROM_START: u16 = 0x8000;
const PRG_ROM_END: u16 = 0xFFFF;

pub trait Mem {
    fn mem_read(&mut self, addr: u16) -> u8;
    fn mem_write(&mut self, addr: u16, data: u8);
    fn mem_read_u16(&mut self, addr: u16) -> u16;
    fn mem_write_u16(&mut self, addr: u16, data: u16);
}

pub struct Bus {
    cpu_vram: [u8; 2048],
    prg_rom: Vec<u8>,
    ppu: PPU,
}

impl Bus {
    pub fn new(rom: Rom) -> Self {
        Bus {
            cpu_vram: [0; 2048],
            prg_rom: rom.prg_rom,
            ppu: PPU::new(rom.chr_rom, rom.screen_mirroring),
        }
    }
}

impl Mem for Bus {
    fn mem_read(&mut self, addr: u16) -> u8 {
        match addr {
            RAM_START..=RAM_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0000_0111_1111_1111;
                self.cpu_vram[mirror_down_addr as usize]
            }
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 | 0x4014 => {
                panic!("Attempt to read from write-only PPU address {:#04x}", addr)
            }
            0x2002 => self.ppu.read_status(),
            0x2004 => self.ppu.read_oam_data(),
            0x2007 => self.ppu.read_data(),
            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let _mirror_down_addr = addr & 0b0000_0100_0000_0111;
                self.mem_read(_mirror_down_addr)
            }
            PRG_ROM_START..=PRG_ROM_END => {
                let mut prg_rom_addr = (addr - 0x8000) as usize;
                if self.prg_rom.len() == 0x4000 && prg_rom_addr >= 0x4000 {
                    prg_rom_addr -= 0x4000;
                }
                self.prg_rom[prg_rom_addr]
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
            0x2000 => {
                self.ppu.write_to_ctrl(data);
            }
            0x2001 => {
                self.ppu.write_to_mask(data);
            }
            0x2002 => panic!("attempt to write to PPU status register"),

            0x2003 => {
                self.ppu.write_to_oam_addr(data);
            }
            0x2004 => {
                self.ppu.write_to_oam_data(data);
            }
            0x2005 => {
                self.ppu.write_to_scroll(data);
            }
            0x2006 => {
                self.ppu.write_to_ppu_addr(data);
            }
            0x2007 => {
                self.ppu.write_to_data(data);
            }
            0x2008..=PPU_REGISTERS_MIRRORS_END => {
                let mirror_down_addr = addr & 0b0000_0100_0000_0111;
                self.mem_write(mirror_down_addr, data);
            }
            0x8000..=0xFFFF => panic!("Attempt to write to Cartridge ROM space: {:x}", addr),
            _ => {
                println!("Ignoring memory write access at {:#04x}", addr);
            }
        }
    }

    fn mem_read_u16(&mut self, addr: u16) -> u16 {
        u16::from_le_bytes([self.mem_read(addr as u16), self.mem_read((addr + 1) as u16)])
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        self.mem_write(addr, (data & 0xff) as u8);
        self.mem_write(addr + 1, (data >> 8) as u8);
    }
}
