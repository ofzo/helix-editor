use helix_dap::helix_dap_types::{Breakpoint, SourceBreakpoint};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// A breakpoint as stored by the editor, associated with a file and line.
#[derive(Debug, Clone)]
pub struct FileBreakpoint {
    /// Line number (1-based as displayed to user, 0-based internally for document operations).
    pub line: usize,
    /// Optional condition expression.
    pub condition: Option<String>,
    /// Optional hit condition (e.g., ">=5").
    pub hit_condition: Option<String>,
    /// Optional log message (logpoint).
    pub log_message: Option<String>,
    /// Whether the adapter verified this breakpoint.
    pub verified: bool,
    /// The adapter-assigned ID, if set.
    pub adapter_id: Option<usize>,
    /// Actual line the adapter resolved the breakpoint to, if different from requested.
    pub resolved_line: Option<usize>,
}

impl FileBreakpoint {
    pub fn new(line: usize) -> Self {
        Self {
            line,
            condition: None,
            hit_condition: None,
            log_message: None,
            verified: false,
            adapter_id: None,
            resolved_line: None,
        }
    }

    /// Convert to a DAP SourceBreakpoint for sending to the adapter.
    /// Internally line is 0-based; DAP uses 1-based lines.
    pub fn to_source_breakpoint(&self) -> SourceBreakpoint {
        SourceBreakpoint {
            line: self.line + 1,
            column: None,
            condition: self.condition.clone(),
            hit_condition: self.hit_condition.clone(),
            log_message: self.log_message.clone(),
        }
    }

    /// The effective line (resolved if available, otherwise requested).
    pub fn effective_line(&self) -> usize {
        self.resolved_line.unwrap_or(self.line)
    }
}

/// Stores breakpoints per file path.
#[derive(Debug, Default)]
pub struct BreakpointStore {
    breakpoints: HashMap<PathBuf, Vec<FileBreakpoint>>,
}

impl BreakpointStore {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get breakpoints for a specific file.
    pub fn get(&self, path: &Path) -> Option<&[FileBreakpoint]> {
        self.breakpoints.get(path).map(|v| v.as_slice())
    }

    /// Get mutable breakpoints for a specific file.
    pub fn get_mut(&mut self, path: &Path) -> Option<&mut Vec<FileBreakpoint>> {
        self.breakpoints.get_mut(path)
    }

    /// Toggle a breakpoint at the given line for the given file.
    /// Returns `true` if a breakpoint was added, `false` if one was removed.
    pub fn toggle(&mut self, path: PathBuf, line: usize) -> bool {
        let breakpoints = self.breakpoints.entry(path).or_default();
        if let Some(pos) = breakpoints.iter().position(|bp| bp.line == line) {
            breakpoints.remove(pos);
            false
        } else {
            breakpoints.push(FileBreakpoint::new(line));
            true
        }
    }

    /// Set breakpoints for a file, replacing any existing ones.
    pub fn set(&mut self, path: PathBuf, breakpoints: Vec<FileBreakpoint>) {
        if breakpoints.is_empty() {
            self.breakpoints.remove(&path);
        } else {
            self.breakpoints.insert(path, breakpoints);
        }
    }

    /// Remove all breakpoints for a file.
    pub fn clear_file(&mut self, path: &Path) {
        self.breakpoints.remove(path);
    }

    /// Remove all breakpoints.
    pub fn clear_all(&mut self) {
        self.breakpoints.clear();
    }

    /// Iterate over all files that have breakpoints.
    pub fn iter(&self) -> impl Iterator<Item = (&Path, &[FileBreakpoint])> {
        self.breakpoints
            .iter()
            .map(|(path, bps)| (path.as_path(), bps.as_slice()))
    }

    /// Get all file paths that have breakpoints.
    pub fn files(&self) -> impl Iterator<Item = &Path> {
        self.breakpoints.keys().map(|p| p.as_path())
    }

    /// Total number of breakpoints across all files.
    pub fn count(&self) -> usize {
        self.breakpoints.values().map(|v| v.len()).sum()
    }

    /// Check if a specific line in a file has a breakpoint.
    pub fn has_breakpoint(&self, path: &Path, line: usize) -> bool {
        self.breakpoints
            .get(path)
            .is_some_and(|bps| bps.iter().any(|bp| bp.line == line))
    }

    /// Update breakpoints from adapter verification response.
    pub fn update_from_adapter(&mut self, path: &Path, adapter_breakpoints: &[Breakpoint]) {
        if let Some(file_bps) = self.breakpoints.get_mut(path) {
            for (file_bp, adapter_bp) in file_bps.iter_mut().zip(adapter_breakpoints.iter()) {
                file_bp.verified = adapter_bp.verified;
                file_bp.adapter_id = adapter_bp.id;
                // Adapter returns 1-based lines; convert to 0-based for internal storage
                file_bp.resolved_line = adapter_bp.line.map(|l| l.saturating_sub(1));
            }
        }
    }
}
