use tree_house::query_iter::QueryIterEvent;

use crate::syntax::{Loader, Syntax};
use crate::RopeSlice;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FoldRange {
    /// The fold header line (stays visible when folded)
    pub start_line: usize,
    /// The last line hidden when folded (inclusive)
    pub end_line: usize,
}

/// Compute fold ranges from tree-sitter `@fold` captures.
///
/// Returns a sorted, deduplicated list of fold ranges that span at least 2 lines.
pub fn compute_fold_ranges(
    syntax: &Syntax,
    text: RopeSlice,
    loader: &Loader,
) -> Vec<FoldRange> {
    let mut ranges = Vec::new();

    for event in syntax.folds(text, loader, ..) {
        let QueryIterEvent::Match(mat) = event else {
            continue;
        };
        let node = mat.node;
        let start_byte = node.start_byte() as usize;
        let end_byte = node.end_byte() as usize;
        let start_line = text.byte_to_line(start_byte);
        let end_line = text.byte_to_line(end_byte.saturating_sub(1).max(start_byte));

        // Only keep ranges spanning 2+ lines
        if start_line < end_line {
            ranges.push(FoldRange {
                start_line,
                end_line,
            });
        }
    }

    ranges.sort_by_key(|r| r.start_line);
    ranges.dedup_by_key(|r| r.start_line);
    ranges
}
