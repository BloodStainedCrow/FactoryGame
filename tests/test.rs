use proptest::prelude::*;

proptest! {
    #![proptest_config(ProptestConfig::with_cases(1000))]
    #[test]
    fn test_addition_placeholder(a in 0..10, b in 0..10) {
        println!("a:{a}");
        println!("b:{b}");
        prop_assert!(a + b <= 18);
    }
}

#[test]
#[should_panic = "InvalidDigit"]
fn bad_string_placeholder() {
    "twenty".parse::<i32>().expect("Expect to panic");
}
