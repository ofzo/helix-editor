use anyhow::{bail, Context, Result};
use arc_swap::ArcSwap;
use gix::filter::plumbing::driver::apply::Delay;
use gix::path::env;
use std::cell::RefCell;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use gix::bstr::ByteSlice;
use gix::diff::Rewrites;
use gix::dir::entry::Status;
use gix::objs::tree::EntryKind;
use gix::sec::trust::DefaultForLevel;
use gix::status::{
    index_worktree::Item,
    plumbing::index_as_worktree::{Change, EntryStatus},
    UntrackedFiles,
};
use gix::{Commit, ObjectId, Repository, ThreadSafeRepository};

use crate::FileChange;

/// Structured blame information for a single line.
pub struct BlameInfo {
    pub commit_hash: String,
    pub author: String,
    pub author_mail: String,
    pub author_time: String,
    pub summary: String,
}

#[cfg(test)]
mod test;

#[inline]
fn get_repo_dir(file: &Path) -> Result<&Path> {
    file.parent().context("file has no parent directory")
}

pub fn get_diff_base(file: &Path) -> Result<Vec<u8>> {
    debug_assert!(!file.exists() || file.is_file());
    debug_assert!(file.is_absolute());
    let file = gix::path::realpath(file).context("resolve symlinks")?;

    // TODO cache repository lookup

    let repo_dir = get_repo_dir(&file)?;
    let repo = open_repo(repo_dir)
        .context("failed to open git repo")?
        .to_thread_local();
    let head = repo.head_commit()?;
    let file_oid = find_file_in_commit(&repo, &head, &file)?;

    let file_object = repo.find_object(file_oid)?;
    let data = file_object.detach().data;
    // Get the actual data that git would make out of the git object.
    // This will apply the user's git config or attributes like crlf conversions.
    if let Some(work_dir) = repo.workdir() {
        let rela_path = file.strip_prefix(work_dir)?;
        let rela_path = gix::path::try_into_bstr(rela_path)?;
        let (mut pipeline, _) = repo.filter_pipeline(None)?;
        let mut worktree_outcome =
            pipeline.convert_to_worktree(&data, rela_path.as_ref(), Delay::Forbid)?;
        let mut buf = Vec::with_capacity(data.len());
        worktree_outcome.read_to_end(&mut buf)?;
        Ok(buf)
    } else {
        Ok(data)
    }
}

pub fn get_current_head_name(file: &Path) -> Result<Arc<ArcSwap<Box<str>>>> {
    debug_assert!(!file.exists() || file.is_file());
    debug_assert!(file.is_absolute());
    let file = gix::path::realpath(file).context("resolve symlinks")?;

    let repo_dir = get_repo_dir(&file)?;
    let repo = open_repo(repo_dir)
        .context("failed to open git repo")?
        .to_thread_local();
    let head_ref = repo.head_ref()?;
    let head_commit = repo.head_commit()?;

    let name = match head_ref {
        Some(reference) => reference.name().shorten().to_string(),
        None => {
            // Detached HEAD: try to find a tag pointing to this commit
            let head_id = head_commit.id;
            let tag_name = repo
                .references()
                .ok()
                .and_then(|refs| {
                    refs.tags()
                        .ok()
                        .and_then(|tags| {
                            tags.filter_map(Result::ok)
                                .find(|r| {
                                    r.clone()
                                        .into_fully_peeled_id()
                                        .ok()
                                        .map_or(false, |id| id == head_id)
                                })
                                .map(|r| format!("#{}", r.name().shorten()))
                        })
                });
            tag_name.unwrap_or_else(|| head_id.to_hex_with_len(8).to_string())
        }
    };

    Ok(Arc::new(ArcSwap::from_pointee(name.into_boxed_str())))
}

pub fn for_each_changed_file(cwd: &Path, f: impl Fn(Result<FileChange>) -> bool) -> Result<()> {
    status(&open_repo(cwd)?.to_thread_local(), f)
}

fn open_repo(path: &Path) -> Result<ThreadSafeRepository> {
    // custom open options
    let mut git_open_opts_map = gix::sec::trust::Mapping::<gix::open::Options>::default();

    // On windows various configuration options are bundled as part of the installations
    // This path depends on the install location of git and therefore requires some overhead to lookup
    // This is basically only used on windows and has some overhead hence it's disabled on other platforms.
    // `gitoxide` doesn't use this as default
    let config = gix::open::permissions::Config {
        system: true,
        git: true,
        user: true,
        env: true,
        includes: true,
        git_binary: cfg!(windows),
    };
    // change options for config permissions without touching anything else
    git_open_opts_map.reduced = git_open_opts_map
        .reduced
        .permissions(gix::open::Permissions {
            config,
            ..gix::open::Permissions::default_for_level(gix::sec::Trust::Reduced)
        });
    git_open_opts_map.full = git_open_opts_map.full.permissions(gix::open::Permissions {
        config,
        ..gix::open::Permissions::default_for_level(gix::sec::Trust::Full)
    });

    let open_options = gix::discover::upwards::Options {
        dot_git_only: true,
        ..Default::default()
    };

    let res = ThreadSafeRepository::discover_with_environment_overrides_opts(
        path,
        open_options,
        git_open_opts_map,
    )?;

    Ok(res)
}

thread_local! {
    /// Caches (work_dir, git_dir) to skip repeated git discovery for paths
    /// within the same repository.
    static REPO_CACHE: RefCell<Option<(PathBuf, PathBuf)>> = RefCell::new(None);
}

/// Open a git repo, using a cached `.git` dir path when the requested path
/// falls under a previously discovered work tree. Avoids the expensive
/// upward directory walk on repeated calls within the same repo.
fn open_repo_cached(path: &Path) -> Result<ThreadSafeRepository> {
    let cached = REPO_CACHE.with(|c| c.borrow().clone());
    if let Some((work_dir, git_dir)) = cached {
        if path.starts_with(&work_dir) {
            return ThreadSafeRepository::open(&git_dir).map_err(Into::into);
        }
    }
    let repo = open_repo(path)?;
    let work_dir = repo.work_dir().unwrap_or(Path::new("/")).to_path_buf();
    let git_dir = repo.git_dir().to_path_buf();
    REPO_CACHE.with(|c| *c.borrow_mut() = Some((work_dir, git_dir)));
    Ok(repo)
}

pub fn get_repo_root_dir(file: &Path) -> Result<Arc<PathBuf>> {
    debug_assert!(!file.exists() || file.is_file());
    debug_assert!(file.is_absolute());

    let repo_dir = file.parent().context("file has no parent directory")?;
    Ok(Arc::new(match open_repo(repo_dir) {
        Ok(repo) => repo
            .work_dir()
            .unwrap_or_else(|| Path::new("/"))
            .to_path_buf(),

        Err(_) => match env::home_dir() {
            Some(p) => p,
            None => PathBuf::from("/"),
        },
    }))
}

/// Emulates the result of running `git status` from the command line.
fn status(repo: &Repository, f: impl Fn(Result<FileChange>) -> bool) -> Result<()> {
    let work_dir = repo
        .workdir()
        .ok_or_else(|| anyhow::anyhow!("working tree not found"))?
        .to_path_buf();

    let status_platform = repo
        .status(gix::progress::Discard)?
        // Here we discard the `status.showUntrackedFiles` config, as it makes little sense in
        // our case to not list new (untracked) files. We could have respected this config
        // if the default value weren't `Collapsed` though, as this default value would render
        // the feature unusable to many.
        .untracked_files(UntrackedFiles::Files)
        // Turn on file rename detection, which is off by default.
        .index_worktree_rewrites(Some(Rewrites {
            copies: None,
            percentage: Some(0.5),
            limit: 1000,
            ..Default::default()
        }));

    // No filtering based on path
    let empty_patterns = vec![];

    let status_iter = status_platform.into_index_worktree_iter(empty_patterns)?;

    for item in status_iter {
        let Ok(item) = item.map_err(|err| f(Err(err.into()))) else {
            continue;
        };
        let change = match item {
            Item::Modification {
                rela_path, status, ..
            } => {
                let path = work_dir.join(rela_path.to_path()?);
                match status {
                    EntryStatus::Conflict { .. } => FileChange::Conflict { path },
                    EntryStatus::Change(Change::Removed) => FileChange::Deleted { path },
                    EntryStatus::Change(Change::Modification { .. }) => {
                        FileChange::Modified { path }
                    }
                    // Files marked with `git add --intent-to-add`. Such files
                    // still show up as new in `git status`, so it's appropriate
                    // to show them the same way as untracked files in the
                    // "changed file" picker. One example of this being used
                    // is Jujutsu, a Git-compatible VCS. It marks all new files
                    // with `--intent-to-add` automatically.
                    EntryStatus::IntentToAdd => FileChange::Untracked { path },
                    _ => continue,
                }
            }
            Item::DirectoryContents { entry, .. } if entry.status == Status::Untracked => {
                FileChange::Untracked {
                    path: work_dir.join(entry.rela_path.to_path()?),
                }
            }
            Item::Rewrite {
                source,
                dirwalk_entry,
                ..
            } => FileChange::Renamed {
                from_path: work_dir.join(source.rela_path().to_path()?),
                to_path: work_dir.join(dirwalk_entry.rela_path.to_path()?),
            },
            _ => continue,
        };
        if !f(Ok(change)) {
            break;
        }
    }

    Ok(())
}

/// Finds the object that contains the contents of a file at a specific commit.
fn find_file_in_commit(repo: &Repository, commit: &Commit, file: &Path) -> Result<ObjectId> {
    let repo_dir = repo.workdir().context("repo has no worktree")?;
    let rel_path = file.strip_prefix(repo_dir)?;
    let tree = commit.tree()?;
    let tree_entry = tree
        .lookup_entry_by_path(rel_path)?
        .context("file is untracked")?;
    match tree_entry.mode().kind() {
        // not a file, everything is new, do not show diff
        mode @ (EntryKind::Tree | EntryKind::Commit | EntryKind::Link) => {
            bail!("entry at {} is not a file but a {mode:?}", file.display())
        }
        // found a file
        EntryKind::Blob | EntryKind::BlobExecutable => Ok(tree_entry.object_id()),
    }
}

/// Check if a path is ignored by gitignore rules
/// Returns true if the path is ignored, false otherwise
pub fn is_ignored(path: &Path) -> bool {
    are_ignored(&[path.to_path_buf()])[0]
}

/// Batch check if paths are ignored by gitignore rules.
/// Returns a `Vec<bool>` aligned with the input paths.
/// Uses gix native excludes API (no subprocess).
pub fn are_ignored(paths: &[PathBuf]) -> Vec<bool> {
    if paths.is_empty() {
        return vec![];
    }

    let base = match paths[0].parent() {
        Some(p) => p,
        None => return vec![false; paths.len()],
    };

    let Ok(repo) = open_repo_cached(base) else {
        return vec![false; paths.len()];
    };

    let repo = repo.to_thread_local();
    let Some(work_dir) = repo.workdir() else {
        return vec![false; paths.len()];
    };

    let Ok(index) = repo.index() else {
        return vec![false; paths.len()];
    };

    let Ok(mut excludes) = repo.excludes(
        &index,
        None,
        gix::worktree::stack::state::ignore::Source::WorktreeThenIdMappingIfNotSkipped,
    ) else {
        return vec![false; paths.len()];
    };

    paths
        .iter()
        .map(|p| {
            let mode = if p.is_dir() {
                Some(gix::index::entry::Mode::DIR)
            } else {
                Some(gix::index::entry::Mode::FILE)
            };
            p.strip_prefix(work_dir)
                .ok()
                .and_then(|rel| excludes.at_path(rel, mode).ok())
                .map(|platform| platform.is_excluded())
                .unwrap_or(false)
        })
        .collect()
}

/// Get blame information for a specific line in a file.
/// `line` is 1-indexed (matching git blame convention).
pub fn blame_line(file: &Path, line: usize) -> Result<BlameInfo> {
    let file = gix::path::realpath(file).context("resolve symlinks")?;
    let repo_dir = get_repo_dir(&file)?;
    let repo = open_repo(repo_dir)
        .context("failed to open git repo")?
        .to_thread_local();
    let work_dir = repo
        .workdir()
        .context("repo has no working directory")?;
    let rel_path = file.strip_prefix(work_dir)?;

    let line_range = format!("{},{}", line, line);
    let output = std::process::Command::new("git")
        .args(["blame", "-L", &line_range, "--porcelain", "--"])
        .arg(rel_path)
        .current_dir(work_dir)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .output()
        .context("failed to run git blame")?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        bail!("git blame failed: {}", stderr.trim());
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    parse_porcelain_blame(&stdout)
}

fn parse_porcelain_blame(output: &str) -> Result<BlameInfo> {
    let mut commit_hash = String::new();
    let mut author = String::new();
    let mut author_mail = String::new();
    let mut author_time = String::new();
    let mut summary = String::new();

    for (i, line) in output.lines().enumerate() {
        if i == 0 {
            // First line: <hash> <orig_line> <final_line> [<num_lines>]
            commit_hash = line.split_whitespace().next().unwrap_or("").to_string();
        } else if let Some(val) = line.strip_prefix("author ") {
            author = val.to_string();
        } else if let Some(val) = line.strip_prefix("author-mail ") {
            author_mail = val.to_string();
        } else if let Some(val) = line.strip_prefix("author-time ") {
            // Convert unix timestamp to readable date
            if let Ok(ts) = val.parse::<i64>() {
                author_time = format_unix_timestamp(ts);
            } else {
                author_time = val.to_string();
            }
        } else if let Some(val) = line.strip_prefix("summary ") {
            summary = val.to_string();
        }
    }

    if commit_hash.is_empty() {
        bail!("failed to parse git blame output");
    }

    Ok(BlameInfo {
        commit_hash,
        author,
        author_mail,
        author_time,
        summary,
    })
}

fn format_unix_timestamp(ts: i64) -> String {
    // Simple date formatting without external crate
    // Calculate date from unix timestamp
    let secs_per_day: i64 = 86400;
    let days = ts / secs_per_day;
    let time_of_day = ts % secs_per_day;
    let hours = time_of_day / 3600;
    let minutes = (time_of_day % 3600) / 60;

    // Days since epoch to date (simplified algorithm)
    let mut y = 1970i64;
    let mut remaining_days = days;

    loop {
        let days_in_year = if y % 4 == 0 && (y % 100 != 0 || y % 400 == 0) {
            366
        } else {
            365
        };
        if remaining_days < days_in_year {
            break;
        }
        remaining_days -= days_in_year;
        y += 1;
    }

    let leap = y % 4 == 0 && (y % 100 != 0 || y % 400 == 0);
    let days_in_months: [i64; 12] = [
        31,
        if leap { 29 } else { 28 },
        31, 30, 31, 30, 31, 31, 30, 31, 30, 31,
    ];

    let mut m = 0usize;
    for (i, &dim) in days_in_months.iter().enumerate() {
        if remaining_days < dim {
            m = i;
            break;
        }
        remaining_days -= dim;
    }

    let d = remaining_days + 1;
    format!("{:04}-{:02}-{:02} {:02}:{:02}", y, m + 1, d, hours, minutes)
}
