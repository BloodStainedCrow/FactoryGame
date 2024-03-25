use proptest::prelude::*;

proptest! {}

#[test]
#[should_panic = "InvalidDigit"]
fn bad_string_placeholder() {
    "twenty".parse::<i32>().expect("Expect to panic");
}
