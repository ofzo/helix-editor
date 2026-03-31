use std::path::{Path, PathBuf};

/// Lightweight status indicator for a file change, suitable for storage in maps.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileChangeStatus {
    Untracked,
    Modified,
    Conflict,
    Deleted,
    Renamed,
}

/// States for a file having been changed.
pub enum FileChange {
    /// Not tracked by the VCS.
    Untracked { path: PathBuf },
    /// File has been modified.
    Modified { path: PathBuf },
    /// File modification is in conflict with a different update.
    Conflict { path: PathBuf },
    /// File has been deleted.
    Deleted { path: PathBuf },
    /// File has been renamed.
    Renamed {
        from_path: PathBuf,
        to_path: PathBuf,
    },
}

impl FileChange {
    pub fn status(&self) -> FileChangeStatus {
        match self {
            Self::Untracked { .. } => FileChangeStatus::Untracked,
            Self::Modified { .. } => FileChangeStatus::Modified,
            Self::Conflict { .. } => FileChangeStatus::Conflict,
            Self::Deleted { .. } => FileChangeStatus::Deleted,
            Self::Renamed { .. } => FileChangeStatus::Renamed,
        }
    }

    pub fn path(&self) -> &Path {
        match self {
            Self::Untracked { path } => path,
            Self::Modified { path } => path,
            Self::Conflict { path } => path,
            Self::Deleted { path } => path,
            Self::Renamed { to_path, .. } => to_path,
        }
    }
}
