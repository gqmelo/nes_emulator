use bitflags::bitflags;

use crate::cartridge::Mirroring;

pub struct PPU {
    chr_rom: Vec<u8>,
    mirroring: Mirroring,
    palette_table: [u8; 32],
    vram: [u8; 2048],
    oam_addr: u8,
    oam_data: [u8; 256],
    internal_data_buf: u8,
    addr: AddrRegister,
    ctrl: ControlRegister,
    mask: MaskRegister,
    status: StatusRegister,
    scroll: ScrollRegister,
}

impl PPU {
    pub fn new(chr_rom: Vec<u8>, mirroring: Mirroring) -> Self {
        PPU {
            chr_rom,
            mirroring,
            palette_table: [0; 32],
            vram: [0; 2048],
            oam_addr: 0,
            oam_data: [0; 256],
            internal_data_buf: 0,
            addr: AddrRegister::new(),
            ctrl: ControlRegister::new(),
            mask: MaskRegister::new(),
            status: StatusRegister::new(),
            scroll: ScrollRegister::new(),
        }
    }

    pub fn write_to_ppu_addr(&mut self, value: u8) {
        self.addr.update(value);
    }

    pub fn write_to_ctrl(&mut self, value: u8) {
        self.ctrl.update(value);
    }

    pub fn write_to_mask(&mut self, value: u8) {
        self.mask.update(value);
    }

    pub fn read_status(&mut self) -> u8 {
        let data = self.status.snapshot();
        self.status.reset_vblank_status();
        self.addr.reset_latch();
        self.scroll.reset_latch();
        data
    }

    pub fn write_to_oam_addr(&mut self, value: u8) {
        self.oam_addr = value;
    }

    pub fn write_to_oam_data(&mut self, value: u8) {
        self.oam_data[self.oam_addr as usize] = value;
        self.oam_addr = self.oam_addr.wrapping_add(1);
    }

    pub fn read_oam_data(&self) -> u8 {
        self.oam_data[self.oam_addr as usize]
    }

    pub fn write_oam_dma(&mut self, data: &[u8; 256]) {
        for x in data.iter() {
            self.oam_data[self.oam_addr as usize] = *x;
            self.oam_addr = self.oam_addr.wrapping_add(1);
        }
    }

    pub fn write_to_scroll(&mut self, value: u8) {
        self.scroll.write(value);
    }

    fn increment_vram_addr(&mut self) {
        self.addr.increment(self.ctrl.vram_addr_increment());
    }

    pub fn read_data(&mut self) -> u8 {
        let addr = self.addr.get();
        self.increment_vram_addr();

        match addr {
            0..=0x1fff => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.chr_rom[addr as usize];
                result
            }
            0x2000..=0x2fff => {
                let result = self.internal_data_buf;
                self.internal_data_buf = self.vram[self.mirror_vram_addr(addr) as usize];
                result
            }
            0x3000..=0x3eff => panic!(
                "addr space 0x3000..0x3eff is not expected to be used, requested = {} ",
                addr
            ),
            0x3f00..=0x3fff => self.palette_table[(addr - 0x3f00) as usize],
            _ => panic!("unexpected access to mirrored space {}", addr),
        }
    }

    pub fn write_to_data(&mut self, value: u8) {
        let addr = self.addr.get();
        match addr {
            0..=0x1fff => println!("attempt to write to chr rom space {}", addr),
            0x2000..=0x2fff => {
                self.vram[self.mirror_vram_addr(addr) as usize] = value;
            }
            0x3000..=0x3eff => unimplemented!("addr {} shouldn't be used in reallity", addr),

            //Addresses $3F10/$3F14/$3F18/$3F1C are mirrors of $3F00/$3F04/$3F08/$3F0C
            0x3f10 | 0x3f14 | 0x3f18 | 0x3f1c => {
                let add_mirror = addr - 0x10;
                self.palette_table[(add_mirror - 0x3f00) as usize] = value;
            }
            0x3f00..=0x3fff => {
                self.palette_table[(addr - 0x3f00) as usize] = value;
            }
            _ => panic!("unexpected access to mirrored space {}", addr),
        }
        self.increment_vram_addr();
    }

    fn mirror_vram_addr(&self, addr: u16) -> u16 {
        let mirrored_vram = addr & 0b0010_1111_1111_1111;
        let vram_index = mirrored_vram - 0x2000;
        let name_table = vram_index / 0x400;
        match (&self.mirroring, name_table) {
            (Mirroring::Vertical, 2) | (Mirroring::Vertical, 3) => vram_index - 0x800,
            (Mirroring::Horizontal, 2) => vram_index - 0x400,
            (Mirroring::Horizontal, 1) => vram_index - 0x400,
            (Mirroring::Horizontal, 3) => vram_index - 0x800,
            _ => vram_index,
        }
    }
}

pub struct AddrRegister {
    value: (u8, u8), // high byte first, lo byte second
    hi_ptr: bool,
}

impl AddrRegister {
    pub fn new() -> Self {
        AddrRegister {
            value: (0, 0),
            hi_ptr: true,
        }
    }

    pub fn set(&mut self, data: u16) {
        let bytes = data.to_be_bytes();
        self.value.0 = bytes[0];
        self.value.1 = bytes[1];
    }

    pub fn update(&mut self, data: u8) {
        if self.hi_ptr {
            self.value.0 = data;
        } else {
            self.value.1 = data;
        }

        // mirror down addr above 0x3fff
        let current_value = self.get();
        if current_value > 0x3fff {
            self.set(current_value & 0b0011_1111_1111_1111)
        }
        self.hi_ptr = !self.hi_ptr;
    }

    pub fn increment(&mut self, inc: u8) {
        self.set(self.get().wrapping_add(inc.into()));

        // mirror down addr above 0x3fff
        let current_value = self.get();
        if current_value > 0x3fff {
            self.set(current_value & 0b0011_1111_1111_1111)
        }
    }

    pub fn reset_latch(&mut self) {
        self.hi_ptr = true;
    }

    pub fn get(&self) -> u16 {
        u16::from_be_bytes([self.value.0, self.value.1])
    }
}

bitflags! {
    pub struct ControlRegister: u8 {
        const NAMETABLE1 =              0b0000_0001;
        const NAMETABLE2 =              0b0000_0010;
        const VRAM_ADD_INCREMENT =      0b0000_0100;
        const SPRITE_PATTERN_ADDR =     0b0000_1000;
        const BACKGROUND_PATTERN_ADDR = 0b0001_0000;
        const SPRITE_SIZE =             0b0010_0000;
        const MASTER_SLAVE_SELECT =     0b0100_0000;
        const GENERATE_NMI =            0b1000_0000;
    }
}

impl ControlRegister {
    pub fn new() -> Self {
        ControlRegister::from_bits_truncate(0b0000_0000)
    }

    pub fn vram_addr_increment(&self) -> u8 {
        if !self.contains(ControlRegister::VRAM_ADD_INCREMENT) {
            1
        } else {
            32
        }
    }

    pub fn update(&mut self, data: u8) {
        self.remove(ControlRegister::from_bits_truncate(0b1111_1111));
        self.insert(ControlRegister::from_bits_truncate(data));
    }
}

pub struct ScrollRegister {
    pub scroll_x: u8,
    pub scroll_y: u8,
    pub latch: bool,
}

impl ScrollRegister {
    pub fn new() -> Self {
        ScrollRegister {
            scroll_x: 0,
            scroll_y: 0,
            latch: false,
        }
    }

    pub fn write(&mut self, data: u8) {
        if !self.latch {
            self.scroll_x = data;
        } else {
            self.scroll_y = data;
        }
        self.latch = !self.latch;
    }

    pub fn reset_latch(&mut self) {
        self.latch = false;
    }
}

bitflags! {

    // 7  bit  0
    // ---- ----
    // VSO. ....
    // |||| ||||
    // |||+-++++- Least significant bits previously written into a PPU register
    // |||        (due to register not being updated for this address)
    // ||+------- Sprite overflow. The intent was for this flag to be set
    // ||         whenever more than eight sprites appear on a scanline, but a
    // ||         hardware bug causes the actual behavior to be more complicated
    // ||         and generate false positives as well as false negatives; see
    // ||         PPU sprite evaluation. This flag is set during sprite
    // ||         evaluation and cleared at dot 1 (the second dot) of the
    // ||         pre-render line.
    // |+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
    // |          a nonzero background pixel; cleared at dot 1 of the pre-render
    // |          line.  Used for raster timing.
    // +--------- Vertical blank has started (0: not in vblank; 1: in vblank).
    //            Set at dot 1 of line 241 (the line *after* the post-render
    //            line); cleared after reading $2002 and at dot 1 of the
    //            pre-render line.
    pub struct StatusRegister: u8 {
        const NOTUSED          = 0b00000001;
        const NOTUSED2         = 0b00000010;
        const NOTUSED3         = 0b00000100;
        const NOTUSED4         = 0b00001000;
        const NOTUSED5         = 0b00010000;
        const SPRITE_OVERFLOW  = 0b00100000;
        const SPRITE_ZERO_HIT  = 0b01000000;
        const VBLANK_STARTED   = 0b10000000;
    }
}

impl StatusRegister {
    pub fn new() -> Self {
        StatusRegister::from_bits_truncate(0b00000000)
    }

    pub fn set_vblank_status(&mut self, status: bool) {
        self.set(StatusRegister::VBLANK_STARTED, status);
    }

    pub fn set_sprite_zero_hit(&mut self, status: bool) {
        self.set(StatusRegister::SPRITE_ZERO_HIT, status);
    }

    pub fn set_sprite_overflow(&mut self, status: bool) {
        self.set(StatusRegister::SPRITE_OVERFLOW, status);
    }

    pub fn reset_vblank_status(&mut self) {
        self.remove(StatusRegister::VBLANK_STARTED);
    }

    pub fn is_in_vblank(&self) -> bool {
        self.contains(StatusRegister::VBLANK_STARTED)
    }

    pub fn snapshot(&self) -> u8 {
        self.bits()
    }
}

bitflags! {

    // 7  bit  0
    // ---- ----
    // BGRs bMmG
    // |||| ||||
    // |||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)
    // |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
    // |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
    // |||| +---- 1: Show background
    // |||+------ 1: Show sprites
    // ||+------- Emphasize red
    // |+-------- Emphasize green
    // +--------- Emphasize blue
    pub struct MaskRegister: u8 {
        const GREYSCALE               = 0b00000001;
        const LEFTMOST_8PXL_BACKGROUND  = 0b00000010;
        const LEFTMOST_8PXL_SPRITE      = 0b00000100;
        const SHOW_BACKGROUND         = 0b00001000;
        const SHOW_SPRITES            = 0b00010000;
        const EMPHASISE_RED           = 0b00100000;
        const EMPHASISE_GREEN         = 0b01000000;
        const EMPHASISE_BLUE          = 0b10000000;
    }
}

pub enum Color {
    Red,
    Green,
    Blue,
}

impl MaskRegister {
    pub fn new() -> Self {
        MaskRegister::from_bits_truncate(0b00000000)
    }

    pub fn is_grayscale(&self) -> bool {
        self.contains(MaskRegister::GREYSCALE)
    }

    pub fn leftmost_8pxl_background(&self) -> bool {
        self.contains(MaskRegister::LEFTMOST_8PXL_BACKGROUND)
    }

    pub fn leftmost_8pxl_sprite(&self) -> bool {
        self.contains(MaskRegister::LEFTMOST_8PXL_SPRITE)
    }

    pub fn show_background(&self) -> bool {
        self.contains(MaskRegister::SHOW_BACKGROUND)
    }

    pub fn show_sprites(&self) -> bool {
        self.contains(MaskRegister::SHOW_SPRITES)
    }

    pub fn emphasize(&self) -> Vec<Color> {
        let mut result = Vec::<Color>::new();
        if self.contains(MaskRegister::EMPHASISE_RED) {
            result.push(Color::Red);
        }
        if self.contains(MaskRegister::EMPHASISE_BLUE) {
            result.push(Color::Blue);
        }
        if self.contains(MaskRegister::EMPHASISE_GREEN) {
            result.push(Color::Green);
        }

        result
    }

    pub fn update(&mut self, data: u8) {
        self.remove(MaskRegister::from_bits_truncate(0b1111_1111));
        self.insert(MaskRegister::from_bits_truncate(data));
    }
}
