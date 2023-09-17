const NES_TAG: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];
const PRG_ROM_PAGE_SIZE: usize = 16 * 1024;
const CHR_ROM_PAGE_SIZE: usize = 8 * 1024;
const TRAINER_SIZE: usize = 512;

#[derive(Debug, PartialEq)]
pub enum Mirroring {
    Vertical,
    Horizontal,
    FourScreen,
}

#[derive(Debug)]
pub struct Rom {
    pub pgr_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub mapper: u8,
    pub screen_mirroring: Mirroring,
}

impl Rom {
    pub fn new(raw: &Vec<u8>) -> Result<Rom, String> {
        if raw.len() < 16 {
            return Err(
                "File size is not valid, it is smaller than the fixed header size.".to_string(),
            );
        }

        if raw[0..4] != NES_TAG {
            return Err("File is not in iNES file format".to_string());
        }

        let has_trainer = (raw[6] & 0b100) != 0;
        let version = (raw[7] >> 2) & 0b11;
        if version != 0 {
            return Err("iNES version unsupported. Only iNES 1.0 is supported.".to_string());
        }

        let pgr_rom_size = raw[4] as usize * PRG_ROM_PAGE_SIZE;
        let chr_rom_size = raw[5] as usize * CHR_ROM_PAGE_SIZE;

        let pgr_rom_start = 16 + if has_trainer { TRAINER_SIZE } else { 0 };
        let pgr_rom_end = pgr_rom_start + pgr_rom_size;
        let chr_rom_start = pgr_rom_end;
        let chr_rom_end = chr_rom_start + chr_rom_size;

        let is_four_screen = (raw[6] & 0b1000) != 0;
        let is_vertical_mirroring = (raw[6] & 0x1) != 0;
        let screen_mirroring = match (is_four_screen, is_vertical_mirroring) {
            (true, _) => Mirroring::FourScreen,
            (false, false) => Mirroring::Horizontal,
            (false, true) => Mirroring::Vertical,
        };

        Ok(Rom {
            pgr_rom: raw[pgr_rom_start..pgr_rom_end].to_vec(),
            chr_rom: raw[chr_rom_start..chr_rom_end].to_vec(),
            mapper: 0,
            screen_mirroring: screen_mirroring,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use rstest::*;

    #[rstest]
    fn invalid_file_size() {
        let raw = vec![0x12];
        let rom = Rom::new(&raw);
        assert!(rom.is_err());
        let error = rom.unwrap_err();
        assert!(
            error.to_lowercase().contains("file size is not valid"),
            "error message not as expected '{}'",
            error
        );
    }

    #[rstest]
    fn invalid_tag() {
        let raw = vec![
            0x4E, 0x45, 0x32, 0x1A, 0x02, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00,
        ];
        let rom = Rom::new(&raw);
        assert!(rom.is_err());
        let error = rom.unwrap_err();
        assert!(
            error.to_lowercase().contains("file format"),
            "error message not as expected '{}'",
            error
        );
    }

    #[rstest]
    fn unsupported_version() {
        let mut raw = vec![
            0x4E,
            0x45,
            0x53,
            0x1A,
            0x01,
            0x00,
            0x00,
            0b0000_1000, // iNES 2.0
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
            0x00,
        ];
        raw.extend(std::iter::repeat(0x78).take(1 * PRG_ROM_PAGE_SIZE));
        raw.extend(std::iter::repeat(0x94).take(1 * CHR_ROM_PAGE_SIZE));
        let rom = Rom::new(&raw);
        assert!(rom.is_err());
        let error = rom.unwrap_err();
        assert!(
            error.to_lowercase().contains("unsupported"),
            "error message not as expected '{}'",
            error
        );
    }

    #[rstest]
    fn valid_rom_without_trainer() {
        let mut raw = vec![
            0x4E, 0x45, 0x53, 0x1A, 0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00,
        ];
        raw.extend(std::iter::repeat(0x78).take(1 * PRG_ROM_PAGE_SIZE));
        raw.extend(std::iter::repeat(0x94).take(1 * CHR_ROM_PAGE_SIZE));

        let rom = Rom::new(&raw);

        assert!(rom.is_ok());

        let rom = rom.unwrap();
        assert_eq!(rom.pgr_rom.len(), PRG_ROM_PAGE_SIZE);
        for byte in rom.pgr_rom.iter() {
            assert_eq!(byte, &0x78);
        }
        for byte in rom.chr_rom.iter() {
            assert_eq!(byte, &0x94);
        }
    }

    #[rstest]
    fn valid_rom_with_trainer() {
        let mut raw = vec![
            0x4E, 0x45, 0x53, 0x1A, 0x01, 0x01, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00,
        ];
        raw.extend(std::iter::repeat(0x66).take(TRAINER_SIZE));
        raw.extend(std::iter::repeat(0x78).take(1 * PRG_ROM_PAGE_SIZE));
        raw.extend(std::iter::repeat(0x94).take(1 * CHR_ROM_PAGE_SIZE));

        let rom = Rom::new(&raw);

        assert!(rom.is_ok());

        let rom = rom.unwrap();
        assert_eq!(rom.pgr_rom.len(), PRG_ROM_PAGE_SIZE);
        for byte in rom.pgr_rom.iter() {
            assert_eq!(byte, &0x78);
        }
        for byte in rom.chr_rom.iter() {
            assert_eq!(byte, &0x94);
        }
    }

    #[rstest]
    fn valid_rom_horizontal_mirroring() {
        let raw = vec![
            0x4E, 0x45, 0x53, 0x1A, 0x00, 0x00, 0x00, // horizontal mirroring
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        let rom = Rom::new(&raw);

        assert!(rom.is_ok());

        let rom = rom.unwrap();
        assert_eq!(rom.screen_mirroring, Mirroring::Horizontal);
    }

    #[rstest]
    fn valid_rom_vertical_mirroring() {
        let raw = vec![
            0x4E, 0x45, 0x53, 0x1A, 0x00, 0x00, 0b0001, // vertical mirroring
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        let rom = Rom::new(&raw);

        assert!(rom.is_ok());

        let rom = rom.unwrap();
        assert_eq!(rom.screen_mirroring, Mirroring::Vertical);
    }

    #[rstest]
    fn valid_rom_four_screen_mirroring() {
        let raw = vec![
            0x4E, 0x45, 0x53, 0x1A, 0x00, 0x00, 0b1001, // four screen mirroring
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        ];

        let rom = Rom::new(&raw);

        assert!(rom.is_ok());

        let rom = rom.unwrap();
        assert_eq!(rom.screen_mirroring, Mirroring::FourScreen);
    }
}
