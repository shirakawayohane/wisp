use std::io::Write;

use anyhow::Result;

#[test]
pub fn test_write_f32() {
    let mut buf = Vec::new();
    write_f32(&mut buf, 3.14);
    assert_eq!(buf, [0xc3, 0xf5, 0x48, 0x40]);
}

pub fn write_f32(w: &mut impl Write, value: f32) -> Result<()> {
    w.write(&value.to_le_bytes())?;
    Ok(())
}

#[test]
pub fn test_write_u32() {
    let mut buf = Vec::new();
    write_u32(&mut buf, 123456).unwrap();
    assert_eq!(buf, &[0xC0, 0xC4, 0x07]);
}

pub fn write_u32(w: &mut impl Write, val: u32) -> Result<()> {
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
            w.write(&buf[..written])?;
            break;
        }
    }
    Ok(())
}

#[test]
pub fn test_write_name() {
    let mut buf = Vec::new();
    encode_name(&mut buf, "addTwo").unwrap();
    assert_eq!(
        buf,
        &[0x06, 0x61, 0x64, 0x64, 0x54, 0x77, 0x6f]
    );
}

pub fn encode_name(w: &mut impl Write, name: &str) -> Result<()> {
    let bytes = name.as_bytes();
    write_u32(&mut w, bytes.len() as u32)?;
    w.write(bytes);
    Ok(())
}
