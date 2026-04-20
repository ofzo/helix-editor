use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use notify::{RecommendedWatcher, RecursiveMode, Watcher};
use tokio::sync::mpsc;

/// Debounce interval to coalesce multiple filesystem events (e.g. from atomic saves).
const DEBOUNCE_DURATION: Duration = Duration::from_millis(500);

pub struct FileWatcher {
    watcher: RecommendedWatcher,
    rx: mpsc::UnboundedReceiver<PathBuf>,
}

impl FileWatcher {
    pub fn new() -> anyhow::Result<Self> {
        let (tx, rx) = mpsc::unbounded_channel();

        // Track last event time per path for debouncing
        let debounce_state: std::sync::Arc<std::sync::Mutex<HashMap<PathBuf, Instant>>> =
            std::sync::Arc::new(std::sync::Mutex::new(HashMap::new()));

        let tx_clone = tx.clone();
        let debounce = debounce_state.clone();

        let watcher = notify::recommended_watcher(move |res: Result<notify::Event, notify::Error>| {
            let event = match res {
                Ok(event) => event,
                Err(e) => {
                    log::warn!("file watcher error: {}", e);
                    return;
                }
            };

            // Care about data modifications, create, and remove events
            match event.kind {
                notify::EventKind::Modify(notify::event::ModifyKind::Data(_))
                | notify::EventKind::Modify(notify::event::ModifyKind::Any)
                | notify::EventKind::Create(_)
                | notify::EventKind::Remove(_) => {}
                _ => return,
            }

            let now = Instant::now();
            let mut state = debounce.lock().unwrap();

            for path in event.paths {
                // Canonicalize to handle symlinks
                let path = path.canonicalize().unwrap_or(path);

                if let Some(last) = state.get(&path) {
                    if now.duration_since(*last) < DEBOUNCE_DURATION {
                        state.insert(path, now);
                        continue;
                    }
                }
                state.insert(path.clone(), now);
                let _ = tx_clone.send(path);
            }
        })?;

        Ok(Self { watcher, rx })
    }

    /// Start watching a file path for changes.
    pub fn watch(&mut self, path: &Path) -> anyhow::Result<()> {
        // Watch the parent directory to catch atomic save renames
        let watch_path = if path.is_file() {
            path.parent().unwrap_or(path)
        } else {
            path
        };
        self.watcher
            .watch(watch_path, RecursiveMode::NonRecursive)?;
        Ok(())
    }

    /// Watch a directory recursively (for the file explorer tree).
    pub fn watch_recursive(&mut self, path: &Path) -> anyhow::Result<()> {
        self.watcher.watch(path, RecursiveMode::Recursive)?;
        Ok(())
    }

    /// Stop watching a file path.
    pub fn unwatch(&mut self, path: &Path) -> anyhow::Result<()> {
        let watch_path = if path.is_file() {
            path.parent().unwrap_or(path)
        } else {
            path
        };
        let _ = self.watcher.unwatch(watch_path);
        Ok(())
    }

    /// Receive the next changed file path.
    pub async fn recv(&mut self) -> Option<PathBuf> {
        self.rx.recv().await
    }
}
