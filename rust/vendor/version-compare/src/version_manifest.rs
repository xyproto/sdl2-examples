//! Module for the version manifest.
//!
//! A version manifest can be used to configure and specify how versions are parsed and compared.
//! For example, you can configure the maximum depth of a version number, and set whether text
//! parts are ignored in a version string.

/// Version manifest (configuration).
///
/// A manifest (configuration) that is used respectively when parsing and comparing version strings.
#[derive(Debug, PartialEq)]
pub struct VersionManifest {
    /// The maximum depth of a version number. This specifies the maximum number of parts.
    max_depth: Option<usize>,

    /// True to ignore text parts in version strings.
    ignore_text: bool,
}

/// Version manifest implementation.
impl VersionManifest {
    /// Constructor.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::VersionManifest;
    ///
    /// let mut manifest = VersionManifest::new();
    ///
    /// // Ignore text parts
    /// manifest.set_ignore_text(true);
    /// ```
    pub fn new() -> Self {
        VersionManifest {
            max_depth: None,
            ignore_text: false,
        }
    }

    /// The maximum depth of a version number.
    /// None if no depth is configured.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::VersionManifest;
    ///
    /// let manifest = VersionManifest::new();
    ///
    /// match manifest.max_depth() {
    ///     &Some(depth) => println!("Maximum depth of {}", depth),
    ///     &None => println!("No maximum depth")
    /// }
    /// ```
    pub fn max_depth(&self) -> &Option<usize> {
        &self.max_depth
    }

    /// The maximum depth of a version number as numerical value.
    /// Zero is returned if no depth is configured.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::VersionManifest;
    ///
    /// let manifest = VersionManifest::new();
    ///
    /// println!("Maximum depth of {}", manifest.max_depth_number());
    /// ```
    pub fn max_depth_number(&self) -> usize {
        if self.max_depth.is_some() {
            self.max_depth.unwrap()
        } else {
            0
        }
    }

    /// Set the maximum depth of a version number.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::VersionManifest;
    ///
    /// let mut manifest = VersionManifest::new();
    ///
    /// // Set the maximum depth to 3
    /// manifest.set_max_depth(Some(3));
    ///
    /// // Don't use a maximum depth
    /// manifest.set_max_depth(None);
    /// ```
    pub fn set_max_depth(&mut self, max_depth: Option<usize>) {
        if max_depth.is_some() && max_depth.unwrap() > 0 {
            self.max_depth = max_depth;
        } else {
            self.max_depth = None;
        }
    }

    /// Set the maximum depth of a version number.
    /// Use zero to disable the maximum depth.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::VersionManifest;
    ///
    /// let mut manifest = VersionManifest::new();
    ///
    /// // Set the maximum depth to 3
    /// manifest.set_max_depth_number(3);
    ///
    /// // Don't use a maximum depth
    /// manifest.set_max_depth_number(0);
    /// ```
    pub fn set_max_depth_number(&mut self, max_depth: usize) {
        if max_depth > 0 {
            self.max_depth = Some(max_depth);
        } else {
            self.max_depth = None;
        }
    }

    /// Check whether there's a maximum configured depth.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::VersionManifest;
    ///
    /// let mut manifest = VersionManifest::new();
    ///
    /// assert!(!manifest.has_max_depth());
    ///
    /// manifest.set_max_depth(Some(3));
    /// assert!(manifest.has_max_depth());
    /// ```
    pub fn has_max_depth(&self) -> bool {
        self.max_depth.is_some() && self.max_depth.unwrap() > 0
    }

    /// Check whether to ignore text parts in version numbers.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::VersionManifest;
    ///
    /// let manifest = VersionManifest::new();
    ///
    /// if manifest.ignore_text() {
    ///     println!("Text parts are ignored");
    /// } else {
    ///     println!("Text parts are not ignored");
    /// }
    /// ```
    pub fn ignore_text(&self) -> bool {
        self.ignore_text
    }

    /// Set whether to ignore text parts.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_compare::VersionManifest;
    ///
    /// let mut manifest = VersionManifest::new();
    ///
    /// // Ignore text parts
    /// manifest.set_ignore_text(true);
    ///
    /// // Don't ignore text parts
    /// manifest.set_ignore_text(false);
    /// ```
    pub fn set_ignore_text(&mut self, ignore_text: bool) {
        self.ignore_text = ignore_text;
    }
}

#[cfg_attr(tarpaulin, skip)]
#[cfg(test)]
mod tests {
    use crate::version_manifest::VersionManifest;

    #[test]
    fn max_depth() {
        let mut manifest = VersionManifest::new();

        manifest.max_depth = Some(1);
        assert_eq!(manifest.max_depth(), &Some(1));

        manifest.max_depth = Some(3);
        assert_eq!(manifest.max_depth(), &Some(3));

        manifest.max_depth = None;
        assert_eq!(manifest.max_depth(), &None);
    }

    #[test]
    fn max_depth_number() {
        let mut manifest = VersionManifest::new();

        manifest.max_depth = Some(1);
        assert_eq!(manifest.max_depth_number(), 1);

        manifest.max_depth = Some(3);
        assert_eq!(manifest.max_depth_number(), 3);

        manifest.max_depth = None;
        assert_eq!(manifest.max_depth_number(), 0);
    }

    #[test]
    fn set_max_depth() {
        let mut manifest = VersionManifest::new();

        manifest.set_max_depth(Some(1));
        assert_eq!(manifest.max_depth, Some(1));

        manifest.set_max_depth(Some(3));
        assert_eq!(manifest.max_depth, Some(3));

        manifest.set_max_depth(Some(0));
        assert_eq!(manifest.max_depth, None);

        manifest.set_max_depth(None);
        assert_eq!(manifest.max_depth, None);
    }

    #[test]
    fn set_max_depth_number() {
        let mut manifest = VersionManifest::new();

        manifest.set_max_depth_number(1);
        assert_eq!(manifest.max_depth, Some(1));

        manifest.set_max_depth_number(3);
        assert_eq!(manifest.max_depth, Some(3));

        manifest.set_max_depth_number(0);
        assert_eq!(manifest.max_depth, None);
    }

    #[test]
    fn has_max_depth() {
        let mut manifest = VersionManifest::new();

        manifest.max_depth = Some(1);
        assert!(manifest.has_max_depth());

        manifest.max_depth = Some(3);
        assert!(manifest.has_max_depth());

        manifest.max_depth = None;
        assert!(!manifest.has_max_depth());
    }

    #[test]
    fn ignore_text() {
        let mut manifest = VersionManifest::new();

        manifest.ignore_text = true;
        assert!(manifest.ignore_text());

        manifest.ignore_text = false;
        assert!(!manifest.ignore_text());
    }

    #[test]
    fn set_ignore_text() {
        let mut manifest = VersionManifest::new();

        manifest.set_ignore_text(true);
        assert!(manifest.ignore_text);

        manifest.set_ignore_text(false);
        assert!(!manifest.ignore_text);
    }
}
