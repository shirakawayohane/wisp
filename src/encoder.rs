#[test]
pub fn test_write_f32() {
    assert_eq!(write_f32(3.14), [0xc3, 0xf5, 0x48, 0x40]);
}

pub fn write_f32(value: f32) -> [u8; 4] {
    value.to_le_bytes()
}

#[test]
pub fn test_write_u32() {
    assert_eq!(write_u32(123456), &[0xC0, 0xC4, 0x07]);
}

pub fn write_u32(val: u32) -> Vec<u8> {
    let mut n = val;
    let mut buf = [0_u8; 4];
    let mut written = 0;
    loop {
        let mut byte: u8 = (n & 0x7f) as u8;
        n >>= 7;
        if n != 0 {
            byte |= 0x80;
        }
        buf[written] = byte;
        written += 1;
        if n == 0 {
            return Vec::from(&buf[..written]);
        }
    }
}

#[test]
pub fn test_write_name() {
    assert_eq!(
        encode_name("addTwo"),
        &[0x06, 0x61, 0x64, 0x64, 0x54, 0x77, 0x6f]
    );
}

pub fn encode_name(name: &str) -> Vec<u8> {
    let bytes = name.as_bytes();
    [&write_u32(bytes.len() as u32), bytes].concat()
}
