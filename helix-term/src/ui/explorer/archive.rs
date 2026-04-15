use anyhow::{bail, Result};
use std::collections::BTreeSet;
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::Path;

/// A single entry found inside an archive.
#[derive(Clone, Debug)]
pub struct ArchiveEntry {
    pub path: String,
    pub is_dir: bool,
}

/// Check whether a filesystem path has a supported archive extension.
pub fn is_archive(path: &Path) -> bool {
    let name = match path.file_name().and_then(|n| n.to_str()) {
        Some(n) => n.to_lowercase(),
        None => return false,
    };
    name.ends_with(".zip")
        || name.ends_with(".tar.gz")
        || name.ends_with(".tgz")
        || name.ends_with(".tar")
}

enum ArchiveKind {
    Zip,
    Tar,
    TarGz,
}

fn detect_kind(path: &Path) -> Option<ArchiveKind> {
    let name = path.file_name()?.to_str()?.to_lowercase();
    if name.ends_with(".zip") {
        Some(ArchiveKind::Zip)
    } else if name.ends_with(".tar.gz") || name.ends_with(".tgz") {
        Some(ArchiveKind::TarGz)
    } else if name.ends_with(".tar") {
        Some(ArchiveKind::Tar)
    } else {
        None
    }
}

/// List all entries in an archive, synthesizing missing directory entries.
pub fn list_entries(path: &Path) -> Result<Vec<ArchiveEntry>> {
    let kind = detect_kind(path)
        .ok_or_else(|| anyhow::anyhow!("Unsupported archive format: {}", path.display()))?;

    let raw = match kind {
        ArchiveKind::Zip => list_zip(path)?,
        ArchiveKind::Tar => list_tar(path)?,
        ArchiveKind::TarGz => list_tar_gz(path)?,
    };

    Ok(synthesize_dirs(raw))
}

/// Return direct children under `parent_prefix`.
/// For root level, pass `""`.
pub fn direct_children(entries: &[ArchiveEntry], parent_prefix: &str) -> Vec<ArchiveEntry> {
    let mut seen = BTreeSet::new();
    let mut result = Vec::new();

    for entry in entries {
        let relative = if parent_prefix.is_empty() {
            &entry.path
        } else if let Some(rest) = entry.path.strip_prefix(parent_prefix) {
            rest
        } else {
            continue;
        };

        if relative.is_empty() {
            continue;
        }

        // Get the first path segment
        let (first_segment, has_more) = match relative.find('/') {
            Some(pos) => (&relative[..pos], pos + 1 < relative.len()),
            None => (relative, false),
        };

        if first_segment.is_empty() {
            continue;
        }

        if seen.insert(first_segment.to_string()) {
            let is_dir = has_more || entry.is_dir;
            let child_path = if parent_prefix.is_empty() {
                if is_dir {
                    format!("{}/", first_segment)
                } else {
                    first_segment.to_string()
                }
            } else if is_dir {
                format!("{}{}/", parent_prefix, first_segment)
            } else {
                format!("{}{}", parent_prefix, first_segment)
            };
            result.push(ArchiveEntry {
                path: child_path,
                is_dir,
            });
        }
    }

    result
}

/// Read a specific file entry from an archive into memory.
pub fn read_entry(archive_path: &Path, entry_path: &str) -> Result<Vec<u8>> {
    let kind = detect_kind(archive_path)
        .ok_or_else(|| anyhow::anyhow!("Unsupported archive format: {}", archive_path.display()))?;

    match kind {
        ArchiveKind::Zip => read_zip_entry(archive_path, entry_path),
        ArchiveKind::Tar => read_tar_entry(archive_path, entry_path),
        ArchiveKind::TarGz => read_tar_gz_entry(archive_path, entry_path),
    }
}

// --- ZIP ---

fn list_zip(path: &Path) -> Result<Vec<ArchiveEntry>> {
    let file = BufReader::new(File::open(path)?);
    let mut archive = zip::ZipArchive::new(file)?;
    let mut entries = Vec::with_capacity(archive.len());

    for i in 0..archive.len() {
        let entry = archive.by_index_raw(i)?;
        let name = entry.name().to_string();
        // Normalize backslashes
        let name = name.replace('\\', "/");
        let is_dir = name.ends_with('/');
        entries.push(ArchiveEntry {
            path: name,
            is_dir,
        });
    }

    Ok(entries)
}

fn read_zip_entry(archive_path: &Path, entry_path: &str) -> Result<Vec<u8>> {
    let file = BufReader::new(File::open(archive_path)?);
    let mut archive = zip::ZipArchive::new(file)?;
    let mut entry = archive.by_name(entry_path)?;
    let mut buf = Vec::with_capacity(entry.size() as usize);
    entry.read_to_end(&mut buf)?;
    Ok(buf)
}

// --- TAR ---

fn list_tar(path: &Path) -> Result<Vec<ArchiveEntry>> {
    let file = File::open(path)?;
    list_tar_from_reader(file)
}

fn list_tar_gz(path: &Path) -> Result<Vec<ArchiveEntry>> {
    let file = File::open(path)?;
    let decoder = flate2::read::GzDecoder::new(file);
    list_tar_from_reader(decoder)
}

fn list_tar_from_reader<R: Read>(reader: R) -> Result<Vec<ArchiveEntry>> {
    let mut archive = tar::Archive::new(reader);
    let mut entries = Vec::new();

    for entry in archive.entries()? {
        let entry = entry?;
        let path = entry.path()?;
        let name = path.to_string_lossy().replace('\\', "/");
        // Skip empty paths or "."
        if name.is_empty() || name == "." || name == "./" {
            continue;
        }
        let is_dir = entry.header().entry_type().is_dir();
        entries.push(ArchiveEntry {
            path: name,
            is_dir,
        });
    }

    Ok(entries)
}

fn read_tar_entry(archive_path: &Path, entry_path: &str) -> Result<Vec<u8>> {
    let file = File::open(archive_path)?;
    read_tar_entry_from_reader(file, entry_path)
}

fn read_tar_gz_entry(archive_path: &Path, entry_path: &str) -> Result<Vec<u8>> {
    let file = File::open(archive_path)?;
    let decoder = flate2::read::GzDecoder::new(file);
    read_tar_entry_from_reader(decoder, entry_path)
}

fn read_tar_entry_from_reader<R: Read>(reader: R, entry_path: &str) -> Result<Vec<u8>> {
    let mut archive = tar::Archive::new(reader);
    for entry in archive.entries()? {
        let mut entry = entry?;
        let path = entry.path()?;
        let name = path.to_string_lossy().replace('\\', "/");
        if name == entry_path || name.strip_suffix('/') == Some(entry_path) {
            let mut buf = Vec::with_capacity(entry.size() as usize);
            entry.read_to_end(&mut buf)?;
            return Ok(buf);
        }
    }
    bail!("Entry '{}' not found in archive", entry_path)
}

/// Ensure all intermediate directories exist in the entry list.
fn synthesize_dirs(mut entries: Vec<ArchiveEntry>) -> Vec<ArchiveEntry> {
    let mut known_dirs = BTreeSet::new();

    // Collect existing directory entries
    for entry in &entries {
        if entry.is_dir {
            known_dirs.insert(entry.path.trim_end_matches('/').to_string());
        }
    }

    // For each file, ensure all parent directories exist
    let mut new_dirs = Vec::new();
    for entry in &entries {
        let path = entry.path.trim_end_matches('/');
        let mut current = String::new();
        for segment in path.split('/') {
            if !current.is_empty() {
                current.push('/');
            }
            current.push_str(segment);

            // Don't create a dir entry for the file itself (unless it IS a dir)
            if current == path && !entry.is_dir {
                continue;
            }

            if known_dirs.insert(current.clone()) {
                new_dirs.push(ArchiveEntry {
                    path: format!("{}/", current),
                    is_dir: true,
                });
            }
        }
    }

    entries.extend(new_dirs);
    entries
}
