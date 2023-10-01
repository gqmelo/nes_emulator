use std::env;
use std::fs::File;
use std::io::Read;

use nes_emulator::bus::Bus;
use nes_emulator::cartridge::Rom;
use nes_emulator::cpu::CPU;

fn main() {
    let current_exe = env::current_exe().unwrap();
    let exe_dir = current_exe.parent().unwrap();
    let rom_file = exe_dir.join("../../roms/nestest.nes");

    let mut file = File::open(rom_file.clone()).unwrap();
    let mut raw = Vec::new();
    let mut cpu = match file.read_to_end(&mut raw) {
        Ok(_) => {
            let rom = Rom::new(&raw);
            let bus = Bus::new(rom.unwrap());
            CPU::new(bus, 0xC000)
        }
        Err(_) => {
            panic!("Could not read file {}", rom_file.display())
        }
    };

    cpu.run_with_callback(move |cpu| {
        println!("{}", cpu.trace());
    });
}
