//! Version part module.
//!
//! A module that provides the `VersionPart` enum, with the specification of all available version
//! parts. Each version string is broken down into these version parts when being parsed to a
//! `Version`.

use std::fmt;

/// Enum of version string parts.
///
/// Each version string is broken down into these version parts when being parsed to a `Version`.
#[derive(Debug, PartialEq)]
pub enum VersionPart<'a> {
    /// Numeric part, most common in version strings.
    /// Holds the numerical value.
    Number(i32),

    /// A text part.
    /// These parts usually hold text with an yet unknown definition.
    /// Holds the string slice.
    Text(&'a str),
}

impl<'a> fmt::Display for VersionPart<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VersionPart::Number(n) => write!(f, "{}", n),
            VersionPart::Text(t) => write!(f, "{}", t),
        }
    }
}

#[cfg_attr(tarpaulin, skip)]
#[cfg(test)]
mod tests {
    use crate::version_part::VersionPart;

    #[test]
    fn display() {
        assert_eq!(format!("{}", VersionPart::Number(123)), "123");
        assert_eq!(format!("{}", VersionPart::Text("123")), "123");
    }
}
