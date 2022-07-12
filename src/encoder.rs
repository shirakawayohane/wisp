use std::io::Write;

pub fn encode_leb128<N: Into<u64>>(writer: &mut impl Write, n: N) -> Result<usize, std::io::Error> {
    leb128::write::unsigned(writer, n.into())
}
pub fn encode_s_leb128<N: Into<i64>>(writer: &mut impl Write, n: N) -> Result<usize, std::io::Error> {
    leb128::write::signed(writer, n.into())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_signed() {
        let mut buf = Vec::new();
        encode_leb128(&mut buf, 0 as u8).unwrap();
        assert_eq!(buf, vec![0x00]);

        buf.clear();
        encode_leb128(&mut buf, 1 as u32).unwrap();
        assert_eq!(buf, vec![0x01]);

        buf.clear();
        encode_leb128(&mut buf, 63 as u64).unwrap();
        assert_eq!(buf, vec![0x3f]);

        buf.clear();
        encode_s_leb128(&mut buf, 64 as u8).unwrap();
        assert_eq!(buf, vec![0xc0, 0x00 ]);
        
        buf.clear();
        encode_s_leb128(&mut buf, 8191 as u32).unwrap();
        assert_eq!(buf, vec![0xff, 0x3f]);
        
        buf.clear();
        encode_s_leb128(&mut buf, 8192 as u32).unwrap();
        assert_eq!(buf, vec![0x80, 0xc0, 0x00]);
    }
    #[test]
    fn test_unsigned() {
        let mut buf = Vec::new();
        encode_leb128(&mut buf, 0 as u8).unwrap();
        assert_eq!(buf, vec![0x00]);

        buf.clear();
        encode_leb128(&mut buf, 1 as u32).unwrap();
        assert_eq!(buf, vec![0x01]);

        buf.clear();
        encode_leb128(&mut buf, 63 as u64).unwrap();
        assert_eq!(buf, vec![0x3f]);

        buf.clear();
        encode_leb128(&mut buf, 64 as u8).unwrap();
        assert_eq!(buf, vec![0x40]);


        buf.clear();
        encode_leb128(&mut buf, 8191 as u32).unwrap();
        assert_eq!(buf, vec![0xff, 0x3f]);

        buf.clear();
        encode_leb128(&mut buf, 8192 as u32).unwrap();
        assert_eq!(buf, vec![0x80, 0x40]);
    }
}