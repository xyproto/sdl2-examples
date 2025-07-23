/// Struct containing a version number with some meta data.
/// Such a set can be used for testing.
///
/// # Arguments
///
/// - `0`: The version string.
/// - `1`: Number of version parts.
pub struct TestVersion(pub &'static str, pub usize);

/// List of version numbers with metadata for dynamic tests
pub const TEST_VERSIONS: &'static [TestVersion] = &[
    TestVersion("1", 1),
    TestVersion("1.2", 2),
    TestVersion("1.2.3.4", 4),
    TestVersion("1.2.3.4.5.6.7.8", 8),
    TestVersion("0", 1),
    TestVersion("0.0.0", 3),
    TestVersion("1.0.0", 3),
    TestVersion("0.0.1", 3),
    TestVersion("", 0),
    TestVersion(".", 0),
    TestVersion("...", 0),
    TestVersion("1.2.dev", 3),
    TestVersion("1.2-dev", 3),
    TestVersion("1.2.alpha.4", 4),
    TestVersion("1.2-alpha-4", 4),
    TestVersion("snapshot.1.2", 3),
    TestVersion("snapshot-1.2", 3),
    // TODO: inspect and fix this case
    // TestVersion("version-compare 2.1.8.1 / build 209", 4),
];

/// List of version numbers that contain errors with metadata for dynamic tests
pub const TEST_VERSIONS_ERROR: &'static [TestVersion] = &[
    TestVersion("abc", 1),
    TestVersion("alpha.dev.snapshot", 3),
    TestVersion("test. .snapshot", 3),
    // TODO: broken case, decide what to do here
    // TestVersion("$", 1),
];
