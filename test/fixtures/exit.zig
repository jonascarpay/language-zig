// leading comment

pub fn main() u8 {
    const local: type = struct {
        fn foob() u8 {
            return 11 * 13;
        }
    };
    return local.foob();
}
