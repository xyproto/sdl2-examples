//! Version module, which provides the `Version` struct as parsed version representation.
//!
//! Version numbers in the form of a string are parsed to a `Version` first, before any comparison
//! is made. This struct provides many methods and features for easy comparison, probing and other
//! things.

use std::cmp::Ordering;
use std::fmt;
use std::iter::Peekable;
use std::slice::Iter;

use crate::comp_op::CompOp;
use crate::version_manifest::VersionManifest;
use crate::version_part::VersionPart;

/// Version struct, which is a representation for a parsed version string.
///
/// A version in string format can be parsed using methods like `Version::from("1.2.3");`.
/// These methods return a `Result` holding the parsed version or an error on failure.
///
/// The original version string is stored in the struct, and can be accessed using the
/// `version.as_str()` method. Note, that when the version wasn't parsed from a string
/// representation, the returned value is generated.
///
/// The struct provides many methods for comparison and probing.
pub struct Version<'a> {
    version: &'a str,
    parts: Vec<VersionPart<'a>>,
    manifest: Option<&'a VersionManifest>,
}

impl<'a> Version<'a> {
    /// Create a `Version` instance from a version string.
    ///
    /// The version string should be passed to the `version` parameter.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::{CompOp, Version};
    ///
    /// let ver = Version::from("1.2.3").unwrap();
    ///
    /// assert_eq!(ver.compare(&Version::from("1.2.3").unwrap()), CompOp::Eq);
    /// ```
    pub fn from(version: &'a str) -> Option<Self> {
        // Split the version string
        let parts = Self::split_version_str(version, None);

        // Return nothing if the parts are none
        if parts.is_none() {
            return None;
        }

        // Create and return the object
        Some(Version {
            version: version,
            parts: parts.unwrap(),
            manifest: None,
        })
    }

    /// Create a `Version` instance from a version string with the given `manifest`.
    ///
    /// The version string should be passed to the `version` parameter.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::{CompOp, Version, VersionManifest};
    ///
    /// let manifest = VersionManifest::new();
    /// let ver = Version::from_manifest("1.2.3", &manifest).unwrap();
    ///
    /// assert_eq!(ver.compare(&Version::from("1.2.3").unwrap()), CompOp::Eq);
    /// ```
    pub fn from_manifest(version: &'a str, manifest: &'a VersionManifest) -> Option<Self> {
        // Split the version string
        let parts = Self::split_version_str(version, Some(&manifest));

        // Return nothing if the parts are none
        if parts.is_none() {
            return None;
        }

        // Create and return the object
        Some(Version {
            version: version,
            parts: parts.unwrap(),
            manifest: Some(&manifest),
        })
    }

    /// Get the version manifest, if available.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::Version;
    ///
    /// let version = Version::from("1.2.3").unwrap();
    ///
    /// if version.has_manifest() {
    ///     println!(
    ///         "Maximum version part depth is {} for this version",
    ///         version.manifest().unwrap().max_depth_number()
    ///     );
    /// } else {
    ///     println!("Version has no manifest");
    /// }
    /// ```
    pub fn manifest(&self) -> Option<&VersionManifest> {
        self.manifest
    }

    /// Check whether this version has a manifest.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::Version;
    ///
    /// let version = Version::from("1.2.3").unwrap();
    ///
    /// if version.has_manifest() {
    ///     println!("This version does have a manifest");
    /// } else {
    ///     println!("This version does not have a manifest");
    /// }
    /// ```
    pub fn has_manifest(&self) -> bool {
        self.manifest().is_some()
    }

    /// Set the version manifest.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::{Version, VersionManifest};
    ///
    /// let manifest = VersionManifest::new();
    /// let mut version = Version::from("1.2.3").unwrap();
    ///
    /// version.set_manifest(Some(&manifest));
    /// ```
    pub fn set_manifest(&mut self, manifest: Option<&'a VersionManifest>) {
        self.manifest = manifest;

        // TODO: Re-parse the version string, because the manifest might have changed.
    }

    /// Split the given version string, in it's version parts.
    /// TODO: Move this method to some sort of helper class, maybe as part of `VersionPart`.
    fn split_version_str(
        version: &'a str,
        manifest: Option<&'a VersionManifest>,
    ) -> Option<Vec<VersionPart<'a>>> {
        // Split the version string, and create a vector to put the parts in
        // TODO: split at specific separators instead
        let split = version.split(|c| !char::is_alphanumeric(c));
        let mut parts = Vec::new();

        // Get the manifest to follow
        let mut used_manifest = &VersionManifest::new();
        if manifest.is_some() {
            used_manifest = manifest.unwrap();
        }

        // Flag to determine whether this version number contains any number part
        let mut has_number = false;

        // Loop over the parts, and parse them
        for part in split {
            // We may not go over the maximum depth
            if used_manifest.max_depth().is_some()
                && parts.len() >= used_manifest.max_depth_number()
            {
                break;
            }

            // Skip empty parts
            if part.is_empty() {
                continue;
            }

            // Try to parse the value as an number
            match part.parse::<i32>() {
                Ok(number) => {
                    // Push the number part to the vector, and set the has number flag
                    parts.push(VersionPart::Number(number));
                    has_number = true;
                }
                Err(_) => {
                    // Ignore text parts if specified
                    if used_manifest.ignore_text() {
                        continue;
                    }

                    // Push the text part to the vector
                    parts.push(VersionPart::Text(part))
                }
            }
        }

        // The version must contain a number part, if any part was parsed
        if !has_number && !parts.is_empty() {
            return None;
        }

        // Return the list of parts
        Some(parts)
    }

    /// Get the original version string.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::Version;
    ///
    /// let ver = Version::from("1.2.3").unwrap();
    ///
    /// assert_eq!(ver.as_str(), "1.2.3");
    /// ```
    pub fn as_str(&self) -> &str {
        &self.version
    }

    /// Get a specific version part by it's `index`.
    /// An error is returned if the given index is out of bound.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::{Version, VersionPart};
    ///
    /// let ver = Version::from("1.2.3").unwrap();
    ///
    /// assert_eq!(ver.part(0), Ok(&VersionPart::Number(1)));
    /// assert_eq!(ver.part(1), Ok(&VersionPart::Number(2)));
    /// assert_eq!(ver.part(2), Ok(&VersionPart::Number(3)));
    /// ```
    pub fn part(&self, index: usize) -> Result<&VersionPart<'a>, ()> {
        // Make sure the index is in-bound
        if index >= self.parts.len() {
            return Err(());
        }

        // Return the requested part
        Ok(&self.parts[index])
    }

    /// Get a vector of all version parts.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::{Version, VersionPart};
    ///
    /// let ver = Version::from("1.2.3").unwrap();
    ///
    /// assert_eq!(ver.parts(), &vec![
    ///     VersionPart::Number(1),
    ///     VersionPart::Number(2),
    ///     VersionPart::Number(3)
    /// ]);
    /// ```
    pub fn parts(&self) -> &Vec<VersionPart<'a>> {
        &self.parts
    }

    /// Get the number of parts in this version string.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::Version;
    ///
    /// let ver_a = Version::from("1.2.3").unwrap();
    /// let ver_b = Version::from("1.2.3.4").unwrap();
    ///
    /// assert_eq!(ver_a.part_count(), 3);
    /// assert_eq!(ver_b.part_count(), 4);
    /// ```
    pub fn part_count(&self) -> usize {
        self.parts.len()
    }

    /// Compare this version to the given `other` version.
    ///
    /// This method returns one of the following comparison operators:
    ///
    /// * `Lt`
    /// * `Eq`
    /// * `Gt`
    ///
    /// Other comparison operators can be used when comparing, but aren't returned by this method.
    ///
    /// # Examples:
    ///
    /// ```
    /// use version_compare::{CompOp, Version};
    ///
    /// assert_eq!(Version::from("1.2").unwrap().compare(&Version::from("1.3.2").unwrap()), CompOp::Lt);
    /// assert_eq!(Version::from("1.9").unwrap().compare(&Version::from("1.9").unwrap()), CompOp::Eq);
    /// assert_eq!(Version::from("0.3.0.0").unwrap().compare(&Version::from("0.3").unwrap()), CompOp::Eq);
    /// assert_eq!(Version::from("2").unwrap().compare(&Version::from("1.7.3").unwrap()), CompOp::Gt);
    /// ```
    pub fn compare(&self, other: &'a Version) -> CompOp {
        // Compare the versions with their peekable iterators
        Self::compare_iter(self.parts.iter().peekable(), other.parts.iter().peekable())
    }

    /// Compare this version to the given `other` version,
    /// and check whether the given comparison operator is valid.
    ///
    /// All comparison operators can be used.
    ///
    /// # Examples:
    ///
    /// ```
    /// use version_compare::{CompOp, Version};
    ///
    /// assert!(Version::from("1.2").unwrap().compare_to(&Version::from("1.3.2").unwrap(), &CompOp::Lt));
    /// assert!(Version::from("1.2").unwrap().compare_to(&Version::from("1.3.2").unwrap(), &CompOp::Le));
    /// assert!(Version::from("1.2").unwrap().compare_to(&Version::from("1.2").unwrap(), &CompOp::Eq));
    /// assert!(Version::from("1.2").unwrap().compare_to(&Version::from("1.2").unwrap(), &CompOp::Le));
    /// ```
    pub fn compare_to(&self, other: &Version, operator: &CompOp) -> bool {
        // Get the comparison result
        let result = self.compare(&other);

        // Match the result against the given operator
        match result {
            CompOp::Eq => match operator {
                &CompOp::Eq | &CompOp::Le | &CompOp::Ge => true,
                _ => false,
            },
            CompOp::Lt => match operator {
                &CompOp::Ne | &CompOp::Lt | &CompOp::Le => true,
                _ => false,
            },
            CompOp::Gt => match operator {
                &CompOp::Ne | &CompOp::Gt | &CompOp::Ge => true,
                _ => false,
            },
            _ => unreachable!(),
        }
    }

    /// Compare two version numbers based on the iterators of their version parts.
    ///
    /// This method returns one of the following comparison operators:
    ///
    /// * `Lt`
    /// * `Eq`
    /// * `Gt`
    ///
    /// Other comparison operators can be used when comparing, but aren't returned by this method.
    fn compare_iter(
        mut iter: Peekable<Iter<VersionPart<'a>>>,
        mut other_iter: Peekable<Iter<VersionPart<'a>>>,
    ) -> CompOp {
        // Iterate through the parts of this version
        let mut other_part: Option<&VersionPart>;

        // Iterate over the iterator, without consuming it
        loop {
            match iter.next() {
                Some(part) => {
                    // Get the part for the other version
                    other_part = other_iter.next();

                    // If there are no parts left in the other version, try to determine the result
                    if other_part.is_none() {
                        // In the main version: if the current part is zero, continue to the next one
                        match part {
                            &VersionPart::Number(num) => {
                                if num == 0 {
                                    continue;
                                }
                            }
                            &VersionPart::Text(_) => return CompOp::Lt,
                        }

                        // The main version is greater
                        return CompOp::Gt;
                    }

                    // Match both part as numbers to destruct their numerical values
                    match part {
                        &VersionPart::Number(num) => match other_part.unwrap() {
                            &VersionPart::Number(other_num) => {
                                // Compare the numbers
                                match num {
                                    n if n < other_num => return CompOp::Lt,
                                    n if n > other_num => return CompOp::Gt,
                                    _ => continue,
                                }
                            }
                            _ => {}
                        },
                        _ => {}
                    }
                }
                None => break,
            }
        }

        // Check whether we should iterate over the other iterator, if it has any items left
        match other_iter.peek() {
            // Compare based on the other iterator
            Some(_) => Self::compare_iter(other_iter, iter).as_flipped(),

            // Nothing more to iterate over, the versions should be equal
            None => CompOp::Eq,
        }
    }
}

impl<'a> fmt::Display for Version<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.version)
    }
}

// Show just the version component parts as debug output
impl<'a> fmt::Debug for Version<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:#?}", self.parts)
        } else {
            write!(f, "{:?}", self.parts)
        }
    }
}

/// Implement the partial ordering trait for the version struct, to easily allow version comparison.
impl<'a> PartialOrd for Version<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.compare(other).ord().unwrap())
    }
}

/// Implement the partial equality trait for the version struct, to easily allow version comparison.
impl<'a> PartialEq for Version<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.compare_to(other, &CompOp::Eq)
    }
}

#[cfg_attr(tarpaulin, skip)]
#[cfg(test)]
mod tests {
    use std::cmp;

    use crate::comp_op::CompOp;
    use crate::test::test_version::{TEST_VERSIONS, TEST_VERSIONS_ERROR};
    use crate::test::test_version_set::TEST_VERSION_SETS;
    use crate::version_manifest::VersionManifest;
    use crate::version_part::VersionPart;

    use super::Version;

    #[test]
    // TODO: This doesn't really test whether this method fully works
    fn from() {
        // Test whether parsing works for each test version
        for version in TEST_VERSIONS {
            assert!(Version::from(&version.0).is_some());
        }

        // Test whether parsing works for each test invalid version
        for version in TEST_VERSIONS_ERROR {
            assert!(Version::from(&version.0).is_none());
        }
    }

    #[test]
    // TODO: This doesn't really test whether this method fully works
    fn from_manifest() {
        // Create a manifest
        let manifest = VersionManifest::new();

        // Test whether parsing works for each test version
        for version in TEST_VERSIONS {
            assert_eq!(
                Version::from_manifest(&version.0, &manifest)
                    .unwrap()
                    .manifest,
                Some(&manifest)
            );
        }

        // Test whether parsing works for each test invalid version
        for version in TEST_VERSIONS_ERROR {
            assert!(Version::from_manifest(&version.0, &manifest).is_none());
        }
    }

    #[test]
    fn manifest() {
        let manifest = VersionManifest::new();
        let mut version = Version::from("1.2.3").unwrap();

        version.manifest = Some(&manifest);
        assert_eq!(version.manifest(), Some(&manifest));

        version.manifest = None;
        assert_eq!(version.manifest(), None);
    }

    #[test]
    fn has_manifest() {
        let manifest = VersionManifest::new();
        let mut version = Version::from("1.2.3").unwrap();

        version.manifest = Some(&manifest);
        assert!(version.has_manifest());

        version.manifest = None;
        assert!(!version.has_manifest());
    }

    #[test]
    fn set_manifest() {
        let manifest = VersionManifest::new();
        let mut version = Version::from("1.2.3").unwrap();

        version.set_manifest(Some(&manifest));
        assert_eq!(version.manifest, Some(&manifest));

        version.set_manifest(None);
        assert_eq!(version.manifest, None);
    }

    #[test]
    fn as_str() {
        // Test for each test version
        for version in TEST_VERSIONS {
            // The input version string must be the same as the returned string
            assert_eq!(Version::from(&version.0).unwrap().as_str(), version.0);
        }
    }

    #[test]
    fn part() {
        // Test for each test version
        for version in TEST_VERSIONS {
            // Create a version object
            let ver = Version::from(&version.0).unwrap();

            // Loop through each part
            for i in 0..version.1 {
                assert_eq!(ver.part(i), Ok(&ver.parts[i]));
            }

            // A value outside the range must return an error
            assert!(ver.part(version.1).is_err());
        }
    }

    #[test]
    fn parts() {
        // Test for each test version
        for version in TEST_VERSIONS {
            // The number of parts must match
            assert_eq!(Version::from(&version.0).unwrap().parts().len(), version.1);
        }
    }

    #[test]
    fn parts_max_depth() {
        // Create a manifest
        let mut manifest = VersionManifest::new();

        // Loop through a range of numbers
        for depth in 0..5 {
            // Set the maximum depth
            manifest.set_max_depth_number(depth);

            // Test for each test version with the manifest
            for version in TEST_VERSIONS {
                // Create a version object, and count it's parts
                let ver = Version::from_manifest(&version.0, &manifest);

                // Some versions might be none, because not all of the start with a number when the
                // maximum depth is 1. A version string with only text isn't allowed,
                // resulting in none.
                if ver.is_none() {
                    continue;
                }

                // Get the part count
                let count = ver.unwrap().parts().len();

                // The number of parts must match
                if depth == 0 {
                    assert_eq!(count, version.1);
                } else {
                    assert_eq!(count, cmp::min(version.1, depth));
                }
            }
        }
    }

    #[test]
    fn parts_ignore_text() {
        // Create a manifest
        let mut manifest = VersionManifest::new();

        // Try this for true and false
        for ignore in vec![true, false] {
            // Set to ignore text
            manifest.set_ignore_text(ignore);

            // Keep track whether any version passed with text
            let mut had_text = false;

            // Test each test version
            for version in TEST_VERSIONS {
                // Create a version instance, and get it's parts
                let ver = Version::from_manifest(&version.0, &manifest).unwrap();

                // Loop through all version parts
                for part in ver.parts() {
                    match part {
                        &VersionPart::Text(_) => {
                            // Set the flag
                            had_text = true;

                            // Break the loop if we already reached text when not ignored
                            if !ignore {
                                break;
                            }
                        }
                        _ => {}
                    }
                }
            }

            // Assert had text
            assert_eq!(had_text, !ignore);
        }
    }

    #[test]
    fn part_count() {
        // Test for each test version
        for version in TEST_VERSIONS {
            // The number of parts must match the metadata
            assert_eq!(Version::from(&version.0).unwrap().part_count(), version.1);
        }
    }

    #[test]
    fn compare() {
        // Compare each version in the version set
        for entry in TEST_VERSION_SETS {
            // Get both versions
            let version_a = Version::from(&entry.0).unwrap();
            let version_b = Version::from(&entry.1).unwrap();

            // Compare them
            assert_eq!(
                version_a.compare(&version_b),
                entry.2.clone(),
                "Testing that {} is {} {}",
                &entry.0,
                &entry.2.sign(),
                &entry.1
            );
        }
    }

    #[test]
    fn compare_to() {
        // Compare each version in the version set
        for entry in TEST_VERSION_SETS {
            // Get both versions
            let version_a = Version::from(&entry.0).unwrap();
            let version_b = Version::from(&entry.1).unwrap();

            // Test
            assert!(version_a.compare_to(&version_b, &entry.2));

            // Make sure the inverse operator is not correct
            assert_eq!(version_a.compare_to(&version_b, &entry.2.invert()), false);
        }

        // Assert an exceptional case, compare to not equal
        assert!(Version::from("1.2")
            .unwrap()
            .compare_to(&Version::from("1.2.3").unwrap(), &CompOp::Ne,));
    }

    #[test]
    fn display() {
        assert_eq!(format!("{}", Version::from("1.2.3").unwrap()), "1.2.3");
    }

    #[test]
    fn debug() {
        assert_eq!(
            format!("{:?}", Version::from("1.2.3").unwrap()),
            "[Number(1), Number(2), Number(3)]",
        );
        assert_eq!(
            format!("{:#?}", Version::from("1.2.3").unwrap()),
            "[\n    Number(\n        1,\n    ),\n    Number(\n        2,\n    ),\n    Number(\n        3,\n    ),\n]",
        );
    }

    #[test]
    fn partial_cmp() {
        // Compare each version in the version set
        for entry in TEST_VERSION_SETS {
            // Get both versions
            let version_a = Version::from(&entry.0).unwrap();
            let version_b = Version::from(&entry.1).unwrap();

            // Compare and assert
            match entry.2 {
                CompOp::Eq => assert!(version_a == version_b),
                CompOp::Lt => assert!(version_a < version_b),
                CompOp::Gt => assert!(version_a > version_b),
                _ => {}
            }
        }
    }

    #[test]
    fn partial_eq() {
        // Compare each version in the version set
        for entry in TEST_VERSION_SETS {
            // Skip entries that are less or equal, or greater or equal
            match entry.2 {
                CompOp::Le | CompOp::Ge => continue,
                _ => {}
            }

            // Get both versions
            let version_a = Version::from(&entry.0).unwrap();
            let version_b = Version::from(&entry.1).unwrap();

            // Determine what the result should be
            let result = match entry.2 {
                CompOp::Eq => true,
                _ => false,
            };

            // Test
            assert_eq!(version_a == version_b, result);
        }

        // Assert an exceptional case, compare to not equal
        assert!(Version::from("1.2").unwrap() != Version::from("1.2.3").unwrap());
    }
}
