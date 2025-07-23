//! A Rust library to easily compare version numbers in any format,
//! and test them against various comparison operators.
//!
//! Comparing version numbers is hard. Especially when version numbers get really complex,
//! or when their formatting differs.
//!
//! This library helps you to easily compare any kind of version number with minimal code.
//! Two version numbers can be compared to each other, to get a relevant comparison operator (<, ==, >),
//! or version numbers can be tested against any given comparison operator.
//!
//! Along with version comparison, the library also features other useful tools.
//! For example: version numbers can be parsed to inspect a version number by it's bare numeric or text based parts.
//!
//! Inspired by PHPs [version_compare()](http://php.net/manual/en/function.version-compare.php).
//!
//! ### Version formats
//! A list of version number examples that are parsed successfully:
//!
//! - `1`
//! - `3.10.4.1`
//! - `1.2.alpha`
//! - `1.2.dev.4`
//! - ` ` _(empty)_
//! - ` .   -32 . 1` _(undefined formats)_
//! - `MyApp 3.2.0 / build 0932` _(complex formats, not fully functional yet)_
//! - _Many more and support for custom formats to come..._
//!
//! ### Semver
//! Version number formats like [_semver_](http://semver.org/) try to make version numbers consistent and manageable,
//! there are too many projects however that don't follow such format.
//!
//! Version-compare makes working with them easy and supports semver formats out of the box with zero configuration.
//!
//! ## Features
//! * Compare two version numbers, get: `<`, `==` or `>`.
//! * Compare two version numbers against any comparison operator, get `true` or `false`.
//! * Parse complex version numbers.
//! * Static, single-statement methods available.
//!
//! The following features will be added in a later version:
//!
//! * Support for text parts in version strings.
//! * Version manifest, to specify detailed version number constraints.
//! * Version ranges, and tests against them.
//! * Support for operators in version strings, [npm-style](https://docs.npmjs.com/misc/semver), and tests against them.
//! * Batch comparisons.
//!
//! ## Examples
//! Check out the [examples](https://github.com/timvisee/version-compare/tree/master/examples) directory for all available examples.
//!
//!
//! _[View complete README](https://github.com/timvisee/version-compare/blob/master/README.md)_

pub mod comp_op;
pub mod version;
pub mod version_compare;
pub mod version_manifest;
pub mod version_part;

#[cfg(test)]
mod test;

// Reexports
pub use crate::comp_op::CompOp;
pub use crate::version::Version;
pub use crate::version_compare::VersionCompare;
pub use crate::version_manifest::VersionManifest;
pub use crate::version_part::VersionPart;
