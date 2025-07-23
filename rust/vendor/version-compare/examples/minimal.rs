//! The most minimal usage example of the version-compare library.
//!
//! This example compares two given version numbers, and matches the comparison result.
//!
//! You can run this example file by using the command `cargo run --example minimal`.

extern crate version_compare;

use version_compare::{CompOp, VersionCompare};

fn main() {
    // Define some version numbers
    let a = "1.3";
    let b = "1.2.4";

    // Match
    match VersionCompare::compare(&a, &b).unwrap() {
        CompOp::Lt => println!("Version a is less than b"),
        CompOp::Eq => println!("Version a is equal to b"),
        CompOp::Gt => println!("Version a is greater than b"),
        _ => unreachable!(),
    }
}
