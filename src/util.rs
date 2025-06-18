use std::{
    borrow::Cow,
    ffi::OsStr,
    path::{Component, Path, PathBuf, Prefix},
};

use keepcalm::SharedGlobalMut;
use tempfile::TempDir;

static CANONICAL_TEMP_DIR: SharedGlobalMut<PathBuf> = SharedGlobalMut::new_lazy(|| {
    let tmp = if cfg!(target_vendor = "apple") {
        Path::new("/tmp").to_owned()
    } else {
        std::env::temp_dir()
    };
    match tmp.canonicalize() {
        Ok(canonical) => canonical,
        Err(_) => tmp,
    }
});

static CANONICAL_CWD: SharedGlobalMut<Option<PathBuf>> = SharedGlobalMut::new_lazy(|| {
    let cwd = std::env::current_dir().ok()?;
    match cwd.canonicalize() {
        Ok(canonical) => Some(canonical),
        Err(_) => None,
    }
});

static CANONICAL_HOME_DIR: SharedGlobalMut<Option<PathBuf>> =
    SharedGlobalMut::new_lazy(|| dirs::home_dir().map(|home| home.canonicalize().unwrap_or(home)));

#[derive(Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct NicePathBuf {
    path: PathBuf,
}

impl serde::Serialize for NicePathBuf {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.path.display().to_string())
    }
}

impl<'de> serde::Deserialize<'de> for NicePathBuf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Ok(Self::new(&s))
    }
}

impl From<&'_ NicePathBuf> for NicePathBuf {
    fn from(path: &NicePathBuf) -> Self {
        path.clone()
    }
}

impl From<&'_ Path> for NicePathBuf {
    fn from(path: &Path) -> Self {
        NicePathBuf::new(path.to_owned())
    }
}

impl NicePathBuf {
    pub fn new(path: impl AsRef<Path>) -> Self {
        Self {
            path: path.as_ref().to_path_buf(),
        }
    }

    pub fn exists(&self) -> std::io::Result<bool> {
        std::fs::exists(&self.path)
    }

    pub fn join(&self, other: impl AsRef<Path>) -> Self {
        Self {
            path: self.path.join(other.as_ref()),
        }
    }

    pub fn create_dir_all(&self) -> std::io::Result<()> {
        std::fs::create_dir_all(&self.path)
    }

    pub fn remove_dir_all(&self) -> std::io::Result<()> {
        std::fs::remove_dir_all(&self.path)
    }

    pub fn cwd() -> NicePathBuf {
        let cwd = std::env::current_dir().expect("Couldn't get current directory");
        cwd.into()
    }

    /// Returns a string that can be used in the environment to refer to this
    /// path.
    ///
    /// In the case where this path may be accessed via multiple routes, we will
    /// choose the shortest (ie: /tmp on macOS rather than /private/tmp).
    pub fn env_string(&self) -> String {
        let path = &self.path;
        if let Ok(canonical) = path.canonicalize() {
            if cfg!(target_vendor = "apple") {
                if let Ok(tmp) = canonical.strip_prefix(CANONICAL_TEMP_DIR.read()) {
                    format!("/tmp/{}", tmp.display())
                } else {
                    canonical.display().to_string()
                }
            } else {
                canonical.display().to_string()
            }
        } else {
            path.display().to_string()
        }
    }
}

impl From<PathBuf> for NicePathBuf {
    fn from(path: PathBuf) -> Self {
        Self { path }
    }
}

impl From<String> for NicePathBuf {
    fn from(path: String) -> Self {
        Self {
            path: PathBuf::from(path),
        }
    }
}

impl std::fmt::Display for NicePathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write_pretty_path(false, &self.path, f)
    }
}

impl std::fmt::Debug for NicePathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write_pretty_path(true, &self.path, f)
    }
}

pub struct NiceTempDir {
    path: TempDir,
}

impl NiceTempDir {
    pub fn new() -> Self {
        let path = if cfg!(target_vendor = "apple") {
            tempfile::Builder::new()
                .tempdir_in("/tmp")
                .expect("Couldn't create tempdir")
        } else {
            tempfile::tempdir().expect("Couldn't create tempdir")
        };
        debug_assert!(path.path().is_absolute());
        debug_assert!(matches!(std::fs::exists(path.path()), Ok(true)));
        Self { path }
    }

    pub fn exists(&self) -> Result<bool, std::io::Error> {
        std::fs::exists(self.path.path())
    }

    pub fn remove_dir_all(self) -> std::io::Result<()> {
        self.path.close()
    }

    pub fn join(&self, other: impl AsRef<Path>) -> NicePathBuf {
        NicePathBuf::new(self.path.path().join(other.as_ref()))
    }

    pub fn file_name(&self) -> Option<&OsStr> {
        self.path.path().file_name()
    }

    pub fn env_string(&self) -> String {
        NicePathBuf::from(self).env_string()
    }
}

impl std::fmt::Display for NiceTempDir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", NicePathBuf::new(self.path.path()))
    }
}

impl From<&'_ NiceTempDir> for NicePathBuf {
    fn from(tempdir: &NiceTempDir) -> Self {
        NicePathBuf::new(tempdir.path.path())
    }
}

/// Best effort to canonicalize a path.
fn canonicalize_path(path: &Path) -> Cow<Path> {
    if let Ok(path) = path.canonicalize() {
        return path.into();
    }

    let components = path.components();
    let Some(last) = components.last() else {
        return path.into();
    };

    let mut rest = PathBuf::from(last.as_os_str());

    // Walk up the path, canonicalizing each component and taking the first
    // component that exists.
    let mut path = path;
    while let Some(parent) = path.parent() {
        if let Ok(mut path) = parent.canonicalize() {
            for component in rest.components() {
                match component {
                    Component::ParentDir => {
                        if let Some(parent) = path.parent() {
                            path = parent.to_path_buf();
                        }
                    }
                    Component::CurDir => {}
                    _ => {
                        path = path.join(component.as_os_str());
                    }
                }
            }
            return path.into();
        }

        path = parent;
        let components = path.components();
        let Some(last) = components.last() else {
            return path.into();
        };

        rest = PathBuf::from(last.as_os_str()).join(rest);
    }

    path.into()
}

fn write_pretty_path(
    debug: bool,
    path: &Path,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    let tmp = &*CANONICAL_TEMP_DIR.read();
    let home = &*CANONICAL_HOME_DIR.read();
    let cwd = &*CANONICAL_CWD.read();

    let mut canon_path = canonicalize_path(path);

    // On Apple, we can strip the /private prefix from the path for display purposes
    if cfg!(target_vendor = "apple") {
        if canon_path.is_absolute() {
            if let Ok(without_private) = canon_path.strip_prefix("/private") {
                canon_path = Path::new("/").join(without_private).into();
            }
        }
    }

    // If the path is relative, we can try strip the cwd from its canonical version to eliminate any relative paths.
    if path.is_relative() {
        if let Some(cwd) = cwd {
            if let Ok(path) = canon_path.strip_prefix(cwd) {
                if debug {
                    write_debug_path(f, &path)?;
                } else {
                    write!(f, "{}", path.display())?;
                }
                return Ok(());
            }
        }
    }

    // Unlikely, but just print the path if we're not on unix or windows
    if !cfg!(unix) && !cfg!(windows) {
        if debug {
            write_debug_path(f, &path)?;
        } else {
            write!(f, "{}", path.display())?;
        }
        return Ok(());
    }

    // If the path is in tmp, try to prettify it
    if let Ok(path) = canon_path.strip_prefix(tmp) {
        if cfg!(unix) {
            let path = Path::new("/tmp").join(path);
            if debug {
                write_debug_path(f, &path)?;
            } else {
                write!(f, "{}", path.display())?;
            }
        } else if cfg!(windows) {
            let path = Path::new("%TEMP%").join(path);
            if debug {
                write_debug_path(f, &path)?;
            } else {
                write!(f, "{}", path.display())?;
            }
        }
        return Ok(());
    }

    // Skip out here in debug mode
    if debug {
        // On Windows, we can strip the \\?\ prefix from the path for display purposes
        if cfg!(windows) {
            if let Some(Component::Prefix(prefix)) = canon_path.components().next() {
                // This is a backslash explosion in debug mode...
                if let Prefix::VerbatimDisk(_) = prefix.kind() {
                    return write!(f, "{}", format!("{:?}", canon_path).replace(r"\\\\?\\", ""));
                }
            }
        }

        write!(f, "{:?}", canon_path)?;
        return Ok(());
    }

    // If the path is in home, try to prettify it
    if let Some(home) = home {
        if let Ok(path) = canon_path.strip_prefix(home) {
            if cfg!(unix) {
                write!(f, "~/{}", path.display())?;
            } else if cfg!(windows) {
                write!(f, "%USERPROFILE%\\{}", path.display())?;
            }
            return Ok(());
        }
    }

    // On Windows, we can strip the \\?\ prefix from the path for display purposes
    if cfg!(windows) {
        if let Some(Component::Prefix(prefix)) = canon_path.components().next() {
            if let Prefix::VerbatimDisk(_) = prefix.kind() {
                return write!(
                    f,
                    "{}",
                    canon_path.display().to_string().replace(r"\\?\", "")
                );
            }
        }
    }

    write!(f, "{}", canon_path.display())
}

fn write_debug_path(f: &mut std::fmt::Formatter<'_>, path: &Path) -> std::fmt::Result {
    if cfg!(windows) {
        write!(f, "<{}>", path.display())
    } else {
        write!(f, "{path:?}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(unix)]
    #[test]
    fn test_nice_path_buf_tmp_unix() {
        let path = NicePathBuf::new(Path::new("/tmp/hello.world"));

        assert_eq!("/tmp/hello.world", format!("{}", path));
        assert_eq!("\"/tmp/hello.world\"", format!("{:?}", path));

        let path = NicePathBuf::new(Path::new("//tmp//hello.world"));

        assert_eq!("/tmp/hello.world", format!("{}", path));
        assert_eq!("\"/tmp/hello.world\"", format!("{:?}", path));

        let path = NicePathBuf::new(Path::new("//does-not-exist-anywhere/..//tmp//hello.world"));

        assert_eq!("/tmp/hello.world", format!("{}", path));
        assert_eq!("\"/tmp/hello.world\"", format!("{:?}", path));

        let path = NicePathBuf::new(
            &Path::new("/tmp")
                .canonicalize()
                .unwrap()
                .join("hello.world"),
        );

        assert_eq!("/tmp/hello.world", format!("{}", path));
        assert_eq!("\"/tmp/hello.world\"", format!("{:?}", path));

        // Test partial canonicalization
        let temp_dir = NiceTempDir::new();
        let path = temp_dir.join("a/b/c/d");

        let name = temp_dir.file_name().unwrap().to_string_lossy();

        assert_eq!(format!("/tmp/{name}/a/b/c/d"), format!("{}", path));
        assert_eq!(format!("\"/tmp/{name}/a/b/c/d\""), format!("{:?}", path));
    }

    #[cfg(windows)]
    #[test]
    fn test_nice_path_buf_tmp_windows() {
        let tmp = std::env::temp_dir();
        let tmp = tmp.join("hello.world");

        let path = NicePathBuf::new(&tmp);

        assert_eq!(r"%TEMP%\hello.world", format!("{}", path));
        assert_eq!(r"<%TEMP%\hello.world>", format!("{:?}", path));

        let path = NicePathBuf::new(
            &std::env::temp_dir()
                .canonicalize()
                .unwrap()
                .join("hello.world"),
        );

        assert_eq!(r"%TEMP%\hello.world", format!("{}", path));
        assert_eq!(r"<%TEMP%\hello.world>", format!("{:?}", path));

        let path = NicePathBuf::new(r#"C:\directory"#);

        assert_eq!(r"C:\directory", format!("{}", path));
        assert_eq!(r"<C:\directory>", format!("{:?}", path));
    }
}
