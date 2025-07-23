[![Build status on Travis CI][travis-master-badge]][travis-link]
[![Built status on AppVeyor][appveyor-master-badge]][appveyor-master-link]
[![Crate version][crate-version-badge]][crate-link]
[![Documentation][docs-badge]][docs]
[![Download statistics][crate-download-badge]][crate-link]
[![Coverage status][coverage-badge]][coverage-link]
[![Dependencies][dependency-badge]][dependency-link]
[![License][crate-license-badge]][crate-link]

[crate-version-badge]: https://img.shields.io/crates/v/version-compare.svg
[crate-download-badge]: https://img.shields.io/crates/d/version-compare.svg
[crate-license-badge]: https://img.shields.io/crates/l/version-compare.svg
[crate-link]: https://crates.io/crates/version-compare
[coverage-badge]: https://coveralls.io/repos/github/timvisee/version-compare/badge.svg?branch=master
[coverage-link]: https://coveralls.io/github/timvisee/version-compare?branch=master
[dependency-badge]: https://img.shields.io/badge/dependencies-none!-green.svg
[dependency-link]: https://libraries.io/github/timvisee/version-compare
[docs]: https://docs.rs/version-compare
[docs-badge]: https://docs.rs/version-compare/badge.svg

# Rust library: version-compare
> A Rust library to easily compare version numbers in any format, and test them against various comparison operators.

Comparing version numbers is hard. Especially when version numbers get really complex,
or when their formatting differs. 

This library helps you to easily compare any kind of version number with minimal code.
Two version numbers can be compared to each other, to get a relevant comparison operator (`<`, `==`, `>`),
or version numbers can be tested against any given comparison operator.

Along with version comparison, the library also features other useful tools.  
For example: version numbers can be parsed to inspect a version number by it's bare numeric or text based parts.

Inspired by PHPs [version_compare()](http://php.net/manual/en/function.version-compare.php).

**Note:** This library is still a work in progress.
See the list below for a list of currently available and future features.

### Version formats
A list of version number examples that are parsed successfully:

- `1`
- `3.10.4.1`
- `1.2.alpha`
- `1.2.dev.4`
- ` ` _(empty)_
- ` .   -32 . 1` _(undefined formats)_
- `MyApp 3.2.0 / build 0932` _(complex formats, not fully functional yet)_
- _Many more and support for custom formats to come..._

### Semver
Version number formats like [_semver_](http://semver.org/) try to make version numbers consistent and manageable,
there are too many projects however that don't follow such format.

Version-compare makes working with them easy and supports semver formats out of the box with zero configuration.

## Features
* Compare two version numbers, get: `<`, `==` or `>`.
* Compare two version numbers against any comparison operator, get: `true` or `false`.
* Parse complex and undefined version number formats.
* Static, single-statement methods available.

The following features will be added in a later version:

* Support for text parts in version strings.
* Version manifest, to specify detailed version number constraints.
* Version ranges, and tests against them.
* Support for operators in version strings, [npm-style](https://docs.npmjs.com/misc/semver), and tests against them.
* Batch comparisons.

## Example
This library is very easy to use. Here's a basic usage example:

Cargo.toml:
```toml
[dependencies]
version-compare = "0.0.10"
```

[example.rs:](examples/example.rs)
```rust
extern crate version_compare;

use version_compare::{CompOp, Version, VersionCompare};

fn main() {
    // Define some version numbers
    let a = "1.2";
    let b = "1.5.1";

    // The following comparison operators are used:
    // - CompOp::Eq -> Equal
    // - CompOp::Ne -> Not equal
    // - CompOp::Lt -> Less than
    // - CompOp::Le -> Less than or equal
    // - CompOp::Ge -> Greater than or equal
    // - CompOp::Gt -> Greater than

    // Easily compare version strings
    assert_eq!(VersionCompare::compare(&a, &b).unwrap(), CompOp::Lt);
    assert_eq!(VersionCompare::compare_to(&a, &b, &CompOp::Le).unwrap(), true);
    assert_eq!(VersionCompare::compare_to(&a, &b, &CompOp::Gt).unwrap(), false);

    // Version string parsing
    let a_ver = Version::from(a).unwrap();
    let b_ver = Version::from(b).unwrap();

    // Directly compare parsed versions
    assert_eq!(a_ver < b_ver, true);
    assert_eq!(a_ver <= b_ver, true);
    assert_eq!(a_ver > b_ver, false);
    assert_eq!(a_ver != b_ver, true);
    assert_eq!(a_ver.compare(&b_ver), CompOp::Lt);
    assert_eq!(b_ver.compare(&a_ver), CompOp::Gt);
    assert_eq!(a_ver.compare_to(&b_ver, &CompOp::Lt), true);

    // Match
    match a_ver.compare(&b_ver) {
        CompOp::Lt => println!("Version a is less than b"),
        CompOp::Eq => println!("Version a is equal to b"),
        CompOp::Gt => println!("Version a is greater than b"),
        _ => unreachable!()
    }
}
```

Check out the [examples](examples) directory for more complete examples.

## Builds
This library is automatically build and tested for each commit using CI services.

| Service   | Platforms    | Branch      | Build Status                                                   |                                     |
| --------: | :----------- | :---------- | :------------------------------------------------------------: | :---------------------------------- |
| Travis CI | Linux, macOS | master      | [![Build status][travis-master-badge]][travis-link]            | [View Status][travis-link]          |
| Travis CI | Linux, macOS | last commit | [![Build status][travis-last-badge]][travis-link]              | [View Status][travis-link]          |
| AppVeyor  | Windows      | master      | [![Build status][appveyor-master-badge]][appveyor-master-link] | [View Status][appveyor-master-link] |
| AppVeyor  | Windows      | last commit | [![Build status][appveyor-last-badge]][appveyor-last-link]     | [View Status][appveyor-last-link]   |

[travis-master-badge]:   https://travis-ci.org/timvisee/version-compare.svg?branch=master
[travis-last-badge]:     https://travis-ci.org/timvisee/version-compare.svg
[travis-link]:           https://travis-ci.org/timvisee/version-compare
[appveyor-master-badge]: https://ci.appveyor.com/api/projects/status/nikhmuoonooo05a6/branch/master?svg=true
[appveyor-last-badge]:   https://ci.appveyor.com/api/projects/status/nikhmuoonooo05a6?svg=true
[appveyor-master-link]:  https://ci.appveyor.com/project/timvisee/version-compare/branch/master
[appveyor-last-link]:    https://ci.appveyor.com/project/timvisee/version-compare

## License
This project is released under the MIT license. Check out the [LICENSE](LICENSE) file for more information.
