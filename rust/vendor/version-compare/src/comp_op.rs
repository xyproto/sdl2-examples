//! Module with all supported comparison operators.
//!
//! This module provides an enum with all comparison operators that can be used with this library.
//! The enum provides various useful helper functions to inverse or flip an operator.
//!
//! Methods like `CompOp::from_sign(">");` can be used to get a comparison operator by it's logical
//! sign from a string.

use std::cmp::Ordering;

/// Enum of supported comparison operators.
#[derive(Debug, Clone, PartialEq)]
pub enum CompOp {
    /// Equal (`==`, `=`).
    /// When version `A` is equal to `B`.
    Eq,

    /// Not equal (`!=`, `!`, `<>`).
    /// When version `A` is not equal to `B`.
    Ne,

    /// Less than (`<`).
    /// When version `A` is less than `B` but not equal.
    Lt,

    /// Less or equal (`<=`).
    /// When version `A` is less than or equal to `B`.
    Le,

    /// Greater or equal (`>=`).
    /// When version `A` is greater than or equal to `B`.
    Ge,

    /// Greater than (`>`).
    /// When version `A` is greater than `B` but not equal.
    Gt,
}

impl CompOp {
    /// Get a comparison operator by it's sign.
    /// Whitespaces are stripped from the sign string.
    /// An error is returned if the sign isn't recognized.
    ///
    /// The following signs are supported:
    ///
    /// * `==` _or_ `=` -> `Eq`
    /// * `!=` _or_ `!` _or_ `<>` -> `Ne`
    /// * `< ` -> `Lt`
    /// * `<=` -> `Le`
    /// * `>=` -> `Ge`
    /// * `> ` -> `Gt`
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::CompOp;
    ///
    /// assert_eq!(CompOp::from_sign("=="), Ok(CompOp::Eq));
    /// assert_eq!(CompOp::from_sign("<"), Ok(CompOp::Lt));
    /// assert_eq!(CompOp::from_sign("  >=   "), Ok(CompOp::Ge));
    /// assert!(CompOp::from_sign("*").is_err());
    /// ```
    pub fn from_sign(sign: &str) -> Result<CompOp, ()> {
        match sign.trim().as_ref() {
            "==" | "=" => Ok(CompOp::Eq),
            "!=" | "!" | "<>" => Ok(CompOp::Ne),
            "<" => Ok(CompOp::Lt),
            "<=" => Ok(CompOp::Le),
            ">=" => Ok(CompOp::Ge),
            ">" => Ok(CompOp::Gt),
            _ => Err(()),
        }
    }

    /// Get a comparison operator by it's name.
    /// Names are case-insensitive, and whitespaces are stripped from the string.
    /// An error is returned if the name isn't recognized.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::CompOp;
    ///
    /// assert_eq!(CompOp::from_name("eq"), Ok(CompOp::Eq));
    /// assert_eq!(CompOp::from_name("lt"), Ok(CompOp::Lt));
    /// assert_eq!(CompOp::from_name("  Ge   "), Ok(CompOp::Ge));
    /// assert!(CompOp::from_name("abc").is_err());
    /// ```
    pub fn from_name(sign: &str) -> Result<CompOp, ()> {
        match sign.trim().to_lowercase().as_ref() {
            "eq" => Ok(CompOp::Eq),
            "ne" => Ok(CompOp::Ne),
            "lt" => Ok(CompOp::Lt),
            "le" => Ok(CompOp::Le),
            "ge" => Ok(CompOp::Ge),
            "gt" => Ok(CompOp::Gt),
            _ => Err(()),
        }
    }

    /// Get the comparison operator from Rusts `Ordering` enum.
    ///
    /// The following comparison operators are returned:
    ///
    /// * `Ordering::Less` -> `Lt`
    /// * `Ordering::Equal` -> `Eq`
    /// * `Ordering::Greater` -> `Gt`
    pub fn from_ord(ord: Ordering) -> CompOp {
        match ord {
            Ordering::Less => CompOp::Lt,
            Ordering::Equal => CompOp::Eq,
            Ordering::Greater => CompOp::Gt,
        }
    }

    /// Get the name of this comparison operator.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::CompOp;
    ///
    /// assert_eq!(CompOp::Eq.name(), "eq");
    /// assert_eq!(CompOp::Lt.name(), "lt");
    /// assert_eq!(CompOp::Ge.name(), "ge");
    /// ```
    pub fn name(&self) -> &str {
        match self {
            &CompOp::Eq => "eq",
            &CompOp::Ne => "ne",
            &CompOp::Lt => "lt",
            &CompOp::Le => "le",
            &CompOp::Ge => "ge",
            &CompOp::Gt => "gt",
        }
    }

    /// Covert to the inverted comparison operator.
    ///
    /// This uses the following bidirectional rules:
    ///
    /// * `Eq` <-> `Ne`
    /// * `Lt` <-> `Ge`
    /// * `Le` <-> `Gt`
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::CompOp;
    ///
    /// assert_eq!(CompOp::Eq.as_inverted(), CompOp::Ne);
    /// assert_eq!(CompOp::Lt.as_inverted(), CompOp::Ge);
    /// assert_eq!(CompOp::Gt.as_inverted(), CompOp::Le);
    /// ```
    pub fn as_inverted(self) -> Self {
        self.invert()
    }

    /// Get the inverted comparison operator.
    ///
    /// This uses the following bidirectional rules:
    ///
    /// * `Eq` <-> `Ne`
    /// * `Lt` <-> `Ge`
    /// * `Le` <-> `Gt`
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::CompOp;
    ///
    /// assert_eq!(CompOp::Eq.invert(), CompOp::Ne);
    /// assert_eq!(CompOp::Lt.invert(), CompOp::Ge);
    /// assert_eq!(CompOp::Gt.invert(), CompOp::Le);
    /// ```
    pub fn invert(&self) -> Self {
        match self {
            &CompOp::Eq => CompOp::Ne,
            &CompOp::Ne => CompOp::Eq,
            &CompOp::Lt => CompOp::Ge,
            &CompOp::Le => CompOp::Gt,
            &CompOp::Ge => CompOp::Lt,
            &CompOp::Gt => CompOp::Le,
        }
    }

    /// Convert to the opposite comparison operator.
    ///
    /// This uses the following bidirectional rules:
    ///
    /// * `Eq` <-> `Ne`
    /// * `Lt` <-> `Gt`
    /// * `Le` <-> `Ge`
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::CompOp;
    ///
    /// assert_eq!(CompOp::Eq.as_opposite(), CompOp::Ne);
    /// assert_eq!(CompOp::Lt.as_opposite(), CompOp::Gt);
    /// assert_eq!(CompOp::Ge.as_opposite(), CompOp::Le);
    /// ```
    pub fn as_opposite(self) -> Self {
        self.opposite()
    }

    /// Get the opposite comparison operator.
    ///
    /// This uses the following bidirectional rules:
    ///
    /// * `Eq` <-> `Ne`
    /// * `Lt` <-> `Gt`
    /// * `Le` <-> `Ge`
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::CompOp;
    ///
    /// assert_eq!(CompOp::Eq.opposite(), CompOp::Ne);
    /// assert_eq!(CompOp::Lt.opposite(), CompOp::Gt);
    /// assert_eq!(CompOp::Ge.opposite(), CompOp::Le);
    /// ```
    pub fn opposite(&self) -> Self {
        match self {
            &CompOp::Eq => CompOp::Ne,
            &CompOp::Ne => CompOp::Eq,
            &CompOp::Lt => CompOp::Gt,
            &CompOp::Le => CompOp::Ge,
            &CompOp::Ge => CompOp::Le,
            &CompOp::Gt => CompOp::Lt,
        }
    }

    /// Convert to the flipped comparison operator.
    ///
    /// This uses the following bidirectional rules:
    ///
    /// * `Lt` <-> `Gt`
    /// * `Le` <-> `Ge`
    /// * Other operators are returned as is.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::CompOp;
    ///
    /// assert_eq!(CompOp::Eq.as_flipped(), CompOp::Eq);
    /// assert_eq!(CompOp::Lt.as_flipped(), CompOp::Gt);
    /// assert_eq!(CompOp::Ge.as_flipped(), CompOp::Le);
    /// ```
    pub fn as_flipped(self) -> Self {
        self.flip()
    }

    /// Get the flipped comparison operator.
    ///
    /// This uses the following bidirectional rules:
    ///
    /// * `Lt` <-> `Gt`
    /// * `Le` <-> `Ge`
    /// * Other operators are returned as is.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::CompOp;
    ///
    /// assert_eq!(CompOp::Eq.flip(), CompOp::Eq);
    /// assert_eq!(CompOp::Lt.flip(), CompOp::Gt);
    /// assert_eq!(CompOp::Ge.flip(), CompOp::Le);
    /// ```
    pub fn flip(&self) -> Self {
        match self {
            &CompOp::Lt => CompOp::Gt,
            &CompOp::Le => CompOp::Ge,
            &CompOp::Ge => CompOp::Le,
            &CompOp::Gt => CompOp::Lt,
            _ => self.clone(),
        }
    }

    /// Get the sign for this comparison operator.
    ///
    /// The following signs are returned:
    ///
    /// * `Eq` -> `==`
    /// * `Ne` -> `!=`
    /// * `Lt` -> `< `
    /// * `Le` -> `<=`
    /// * `Ge` -> `>=`
    /// * `Gt` -> `> `
    ///
    /// Note: Some comparison operators also support other signs,
    /// such as `=` for `Eq` and `!` for `Ne`,
    /// these are never returned by this method however as the table above is used.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::CompOp;
    ///
    /// assert_eq!(CompOp::Eq.sign(), "==");
    /// assert_eq!(CompOp::Lt.sign(), "<");
    /// assert_eq!(CompOp::Ge.flip().sign(), "<=");
    /// ```
    pub fn sign(&self) -> &'static str {
        match self {
            &CompOp::Eq => "==",
            &CompOp::Ne => "!=",
            &CompOp::Lt => "<",
            &CompOp::Le => "<=",
            &CompOp::Ge => ">=",
            &CompOp::Gt => ">",
        }
    }

    /// Get a factor (number) for this comparison operator.
    /// These factors can be useful for quick calculations.
    ///
    /// The following factor numbers are returned:
    ///
    /// * `Eq` or `Ne` -> ` 0 `
    /// * `Lt` or `Le` -> `-1`
    /// * `Gt` or `Ge` -> ` 1`
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::Version;
    ///
    /// let ver_a = Version::from("1.2.3").unwrap();
    /// let ver_b = Version::from("1.3").unwrap();
    ///
    /// assert_eq!(ver_a.compare(&ver_b).factor(), -1);
    /// assert_eq!(10 * ver_b.compare(&ver_a).factor(), 10);
    /// ```
    pub fn factor(&self) -> i8 {
        match self {
            &CompOp::Eq | &CompOp::Ne => 0,
            &CompOp::Lt | &CompOp::Le => -1,
            &CompOp::Gt | &CompOp::Ge => 1,
        }
    }

    /// Get Rust's ordering for this comparison operator.
    ///
    /// The following comparison operators are supported:
    ///
    /// * `Eq` -> `Ordering::Equal`
    /// * `Lt` -> `Ordering::Less`
    /// * `Gt` -> `Ordering::Greater`
    ///
    /// For other comparison operators `None` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::cmp::Ordering;
    /// use version_compare::Version;
    ///
    /// let ver_a = Version::from("1.2.3").unwrap();
    /// let ver_b = Version::from("1.3").unwrap();
    ///
    /// assert_eq!(ver_a.compare(&ver_b).ord().unwrap(), Ordering::Less);
    /// ```
    pub fn ord(&self) -> Option<Ordering> {
        match self {
            &CompOp::Eq => Some(Ordering::Equal),
            &CompOp::Lt => Some(Ordering::Less),
            &CompOp::Gt => Some(Ordering::Greater),
            _ => None,
        }
    }
}

#[cfg_attr(tarpaulin, skip)]
#[cfg(test)]
mod tests {
    use std::cmp::Ordering;

    use super::CompOp;

    #[test]
    fn from_sign() {
        // Normal signs
        assert_eq!(CompOp::from_sign("==").unwrap(), CompOp::Eq);
        assert_eq!(CompOp::from_sign("=").unwrap(), CompOp::Eq);
        assert_eq!(CompOp::from_sign("!=").unwrap(), CompOp::Ne);
        assert_eq!(CompOp::from_sign("!").unwrap(), CompOp::Ne);
        assert_eq!(CompOp::from_sign("<>").unwrap(), CompOp::Ne);
        assert_eq!(CompOp::from_sign("<").unwrap(), CompOp::Lt);
        assert_eq!(CompOp::from_sign("<=").unwrap(), CompOp::Le);
        assert_eq!(CompOp::from_sign(">=").unwrap(), CompOp::Ge);
        assert_eq!(CompOp::from_sign(">").unwrap(), CompOp::Gt);

        // Exceptional cases
        assert_eq!(CompOp::from_sign("  <=  ").unwrap(), CompOp::Le);
        assert!(CompOp::from_sign("*").is_err());
    }

    #[test]
    fn from_name() {
        // Normal names
        assert_eq!(CompOp::from_name("eq").unwrap(), CompOp::Eq);
        assert_eq!(CompOp::from_name("ne").unwrap(), CompOp::Ne);
        assert_eq!(CompOp::from_name("lt").unwrap(), CompOp::Lt);
        assert_eq!(CompOp::from_name("le").unwrap(), CompOp::Le);
        assert_eq!(CompOp::from_name("ge").unwrap(), CompOp::Ge);
        assert_eq!(CompOp::from_name("gt").unwrap(), CompOp::Gt);

        // Exceptional cases
        assert_eq!(CompOp::from_name("  Le  ").unwrap(), CompOp::Le);
        assert!(CompOp::from_name("abc").is_err());
    }

    #[test]
    fn from_ord() {
        assert_eq!(CompOp::from_ord(Ordering::Less), CompOp::Lt);
        assert_eq!(CompOp::from_ord(Ordering::Equal), CompOp::Eq);
        assert_eq!(CompOp::from_ord(Ordering::Greater), CompOp::Gt);
    }

    #[test]
    fn name() {
        assert_eq!(CompOp::Eq.name(), "eq");
        assert_eq!(CompOp::Ne.name(), "ne");
        assert_eq!(CompOp::Lt.name(), "lt");
        assert_eq!(CompOp::Le.name(), "le");
        assert_eq!(CompOp::Ge.name(), "ge");
        assert_eq!(CompOp::Gt.name(), "gt");
    }

    #[test]
    fn as_inverted() {
        assert_eq!(CompOp::Ne.as_inverted(), CompOp::Eq);
        assert_eq!(CompOp::Eq.as_inverted(), CompOp::Ne);
        assert_eq!(CompOp::Ge.as_inverted(), CompOp::Lt);
        assert_eq!(CompOp::Gt.as_inverted(), CompOp::Le);
        assert_eq!(CompOp::Lt.as_inverted(), CompOp::Ge);
        assert_eq!(CompOp::Le.as_inverted(), CompOp::Gt);
    }

    #[test]
    fn invert() {
        assert_eq!(CompOp::Ne.invert(), CompOp::Eq);
        assert_eq!(CompOp::Eq.invert(), CompOp::Ne);
        assert_eq!(CompOp::Ge.invert(), CompOp::Lt);
        assert_eq!(CompOp::Gt.invert(), CompOp::Le);
        assert_eq!(CompOp::Lt.invert(), CompOp::Ge);
        assert_eq!(CompOp::Le.invert(), CompOp::Gt);
    }

    #[test]
    fn as_opposite() {
        assert_eq!(CompOp::Ne.as_opposite(), CompOp::Eq);
        assert_eq!(CompOp::Eq.as_opposite(), CompOp::Ne);
        assert_eq!(CompOp::Gt.as_opposite(), CompOp::Lt);
        assert_eq!(CompOp::Ge.as_opposite(), CompOp::Le);
        assert_eq!(CompOp::Le.as_opposite(), CompOp::Ge);
        assert_eq!(CompOp::Lt.as_opposite(), CompOp::Gt);
    }

    #[test]
    fn opposite() {
        assert_eq!(CompOp::Eq.opposite(), CompOp::Ne);
        assert_eq!(CompOp::Ne.opposite(), CompOp::Eq);
        assert_eq!(CompOp::Lt.opposite(), CompOp::Gt);
        assert_eq!(CompOp::Le.opposite(), CompOp::Ge);
        assert_eq!(CompOp::Ge.opposite(), CompOp::Le);
        assert_eq!(CompOp::Gt.opposite(), CompOp::Lt);
    }

    #[test]
    fn as_flipped() {
        assert_eq!(CompOp::Eq.as_flipped(), CompOp::Eq);
        assert_eq!(CompOp::Ne.as_flipped(), CompOp::Ne);
        assert_eq!(CompOp::Lt.as_flipped(), CompOp::Gt);
        assert_eq!(CompOp::Le.as_flipped(), CompOp::Ge);
        assert_eq!(CompOp::Ge.as_flipped(), CompOp::Le);
        assert_eq!(CompOp::Gt.as_flipped(), CompOp::Lt);
    }

    #[test]
    fn flip() {
        assert_eq!(CompOp::Eq.flip(), CompOp::Eq);
        assert_eq!(CompOp::Ne.flip(), CompOp::Ne);
        assert_eq!(CompOp::Lt.flip(), CompOp::Gt);
        assert_eq!(CompOp::Le.flip(), CompOp::Ge);
        assert_eq!(CompOp::Ge.flip(), CompOp::Le);
        assert_eq!(CompOp::Gt.flip(), CompOp::Lt);
    }

    #[test]
    fn sign() {
        assert_eq!(CompOp::Eq.sign(), "==");
        assert_eq!(CompOp::Ne.sign(), "!=");
        assert_eq!(CompOp::Lt.sign(), "<");
        assert_eq!(CompOp::Le.sign(), "<=");
        assert_eq!(CompOp::Ge.sign(), ">=");
        assert_eq!(CompOp::Gt.sign(), ">");
    }

    #[test]
    fn factor() {
        assert_eq!(CompOp::Eq.factor(), 0);
        assert_eq!(CompOp::Ne.factor(), 0);
        assert_eq!(CompOp::Lt.factor(), -1);
        assert_eq!(CompOp::Le.factor(), -1);
        assert_eq!(CompOp::Ge.factor(), 1);
        assert_eq!(CompOp::Gt.factor(), 1);
    }

    #[test]
    fn ord() {
        assert_eq!(CompOp::Eq.ord(), Some(Ordering::Equal));
        assert_eq!(CompOp::Ne.ord(), None);
        assert_eq!(CompOp::Lt.ord(), Some(Ordering::Less));
        assert_eq!(CompOp::Le.ord(), None);
        assert_eq!(CompOp::Ge.ord(), None);
        assert_eq!(CompOp::Gt.ord(), Some(Ordering::Greater));
    }
}
