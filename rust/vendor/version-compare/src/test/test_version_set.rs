use crate::comp_op::CompOp;

/// Struct containing two version numbers, and the comparison operator.
/// Such a set can be used for testing.
///
/// # Arguments
///
/// - `0`: The main version.
/// - `1`: The other version.
/// - `2`: The comparison operator.
pub struct TestVersionSet(pub &'static str, pub &'static str, pub CompOp);

/// List of version sets for dynamic tests
pub const TEST_VERSION_SETS: &'static [TestVersionSet] = &[
    TestVersionSet("1", "1", CompOp::Eq),
    TestVersionSet("1.0.0.0", "1", CompOp::Eq),
    TestVersionSet("1", "1.0.0.0", CompOp::Eq),
    TestVersionSet("0", "0", CompOp::Eq),
    TestVersionSet("0.0.0", "0", CompOp::Eq),
    TestVersionSet("0", "0.0.0", CompOp::Eq),
    TestVersionSet("", "", CompOp::Eq),
    TestVersionSet("", "0.0", CompOp::Eq),
    TestVersionSet("0.0", "", CompOp::Eq),
    TestVersionSet("", "0.1", CompOp::Lt),
    TestVersionSet("0.1", "", CompOp::Gt),
    TestVersionSet("1.2.3", "1.2.3", CompOp::Eq),
    TestVersionSet("1.2.3", "1.2.4", CompOp::Lt),
    TestVersionSet("1.0.0.1", "1.0.0.0", CompOp::Gt),
    TestVersionSet("1.0.0.0", "1.0.0.1", CompOp::Lt),
    TestVersionSet("1.2.3.4", "1.2", CompOp::Gt),
    TestVersionSet("1.2", "1.2.3.4", CompOp::Lt),
    TestVersionSet("1.2.3.4", "2", CompOp::Lt),
    TestVersionSet("2", "1.2.3.4", CompOp::Gt),
    TestVersionSet("123", "123", CompOp::Eq),
    TestVersionSet("123", "1.2.3", CompOp::Gt),
    TestVersionSet("1.2.3", "123", CompOp::Lt),
    TestVersionSet("1.1.2", "1.1.30-dev", CompOp::Lt),
    TestVersionSet("1.2.3", "1.2.3.alpha", CompOp::Gt),
    TestVersionSet("1.2.3", "1.2.3-dev", CompOp::Gt),
    TestVersionSet("1.2.3.dev", "1.2.3.alpha", CompOp::Eq),
    TestVersionSet("1.2.3-dev", "1.2.3-alpha", CompOp::Eq),
    TestVersionSet("1.2.3.dev.1", "1.2.3.alpha", CompOp::Gt),
    TestVersionSet("1.2.3-dev-1", "1.2.3-alpha", CompOp::Gt),
    TestVersionSet("version-compare 3.2.0 / build 0932", "3.2.5", CompOp::Lt),
    TestVersionSet("version-compare 3.2.0 / build 0932", "3.1.1", CompOp::Gt),
    TestVersionSet(
        "version-compare 1.4.1 / build 0043",
        "version-compare 1.4.1 / build 0043",
        CompOp::Eq,
    ),
    TestVersionSet(
        "version-compare 1.4.1 / build 0042",
        "version-compare 1.4.1 / build 0043",
        CompOp::Lt,
    ),
    // TODO: inspect these cases
    TestVersionSet("snapshot.1.2.3", "1.2.3.alpha", CompOp::Lt),
    TestVersionSet("snapshot-1.2.3", "1.2.3-alpha", CompOp::Lt),
];

/// List of invalid version sets for dynamic tests
pub const TEST_VERSION_SETS_ERROR: &'static [TestVersionSet] = &[
    TestVersionSet("1.2.3", "1.2.3", CompOp::Lt),
    TestVersionSet("1.2", "1.2.0.0", CompOp::Ne),
    TestVersionSet("1.2.3.dev", "dev", CompOp::Eq),
    TestVersionSet("snapshot", "1", CompOp::Lt),
];
