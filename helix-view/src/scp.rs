use anyhow::{anyhow, Result};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::process::Command;

pub struct ScpUrl {
    pub user: Option<String>,
    pub host: String,
    pub path: String,
}

impl ScpUrl {
    /// Parse an `scp://[user@]host/path` URL.
    pub fn parse(url: &str) -> Option<ScpUrl> {
        let rest = url.strip_prefix("scp://")?;
        let (authority, path) = rest.split_once('/')?;
        if authority.is_empty() || path.is_empty() {
            return None;
        }
        let (user, host) = if let Some((u, h)) = authority.split_once('@') {
            (Some(u.to_string()), h.to_string())
        } else {
            (None, authority.to_string())
        };
        Some(ScpUrl {
            user,
            host,
            path: format!("/{path}"),
        })
    }

    /// Reconstruct the `scp://` URL for display.
    pub fn to_url_string(&self) -> String {
        match &self.user {
            Some(user) => format!("scp://{}@{}{}", user, self.host, self.path),
            None => format!("scp://{}{}", self.host, self.path),
        }
    }

    /// Build the `user@host:/path` argument for the `scp` CLI.
    fn to_scp_arg(&self) -> String {
        match &self.user {
            Some(user) => format!("{}@{}:{}", user, self.host, self.path),
            None => format!("{}:{}", self.host, self.path),
        }
    }
}

/// Returns `true` if the string looks like an `scp://` URL.
pub fn is_scp_url(s: &str) -> bool {
    s.starts_with("scp://")
}

/// Download a remote file via `scp` to a local temp file (blocking).
pub fn download(url: &ScpUrl) -> Result<PathBuf> {
    let temp_path = temp_path_for(url);
    let status = Command::new("scp")
        .arg(url.to_scp_arg())
        .arg(&temp_path)
        .status()
        .map_err(|e| anyhow!("failed to run scp: {e}"))?;
    if !status.success() {
        anyhow::bail!(
            "scp download failed (exit {}): {}",
            status.code().unwrap_or(-1),
            url.to_url_string()
        );
    }
    Ok(temp_path)
}

/// Upload a local file to the remote host via `scp` (async, spawns blocking task).
pub async fn upload(local: &Path, url: &ScpUrl) -> Result<()> {
    let local = local.to_path_buf();
    let scp_arg = url.to_scp_arg();
    let url_display = url.to_url_string();
    tokio::task::spawn_blocking(move || {
        let status = Command::new("scp")
            .arg(&local)
            .arg(&scp_arg)
            .status()
            .map_err(|e| anyhow!("failed to run scp: {e}"))?;
        if !status.success() {
            anyhow::bail!(
                "scp upload failed (exit {}): {}",
                status.code().unwrap_or(-1),
                url_display
            );
        }
        Ok(())
    })
    .await?
}

/// Derive a deterministic temp file path from the URL, preserving the file extension.
fn temp_path_for(url: &ScpUrl) -> PathBuf {
    let url_str = url.to_url_string();
    let mut hasher = DefaultHasher::new();
    url_str.hash(&mut hasher);
    let hash = hasher.finish();

    let ext = Path::new(&url.path)
        .extension()
        .and_then(|e| e.to_str())
        .unwrap_or("");

    let filename = if ext.is_empty() {
        format!("helix-scp-{hash:x}")
    } else {
        format!("helix-scp-{hash:x}.{ext}")
    };

    std::env::temp_dir().join(filename)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_with_user() {
        let url = ScpUrl::parse("scp://alice@example.com/home/alice/file.txt").unwrap();
        assert_eq!(url.user.as_deref(), Some("alice"));
        assert_eq!(url.host, "example.com");
        assert_eq!(url.path, "/home/alice/file.txt");
    }

    #[test]
    fn parse_without_user() {
        let url = ScpUrl::parse("scp://myhost/etc/config").unwrap();
        assert!(url.user.is_none());
        assert_eq!(url.host, "myhost");
        assert_eq!(url.path, "/etc/config");
    }

    #[test]
    fn parse_rejects_missing_path() {
        assert!(ScpUrl::parse("scp://host").is_none());
    }

    #[test]
    fn parse_rejects_empty_authority() {
        assert!(ScpUrl::parse("scp:///some/path").is_none());
    }

    #[test]
    fn parse_rejects_empty_path() {
        assert!(ScpUrl::parse("scp://host/").is_none());
    }

    #[test]
    fn parse_rejects_wrong_scheme() {
        assert!(ScpUrl::parse("http://host/path").is_none());
    }

    #[test]
    fn to_url_string_roundtrip_with_user() {
        let url = ScpUrl::parse("scp://bob@server/data/file.rs").unwrap();
        assert_eq!(url.to_url_string(), "scp://bob@server/data/file.rs");
    }

    #[test]
    fn to_url_string_roundtrip_without_user() {
        let url = ScpUrl::parse("scp://server/data/file.rs").unwrap();
        assert_eq!(url.to_url_string(), "scp://server/data/file.rs");
    }

    #[test]
    fn to_scp_arg_with_user() {
        let url = ScpUrl::parse("scp://bob@server/data/file.rs").unwrap();
        assert_eq!(url.to_scp_arg(), "bob@server:/data/file.rs");
    }

    #[test]
    fn to_scp_arg_without_user() {
        let url = ScpUrl::parse("scp://server/data/file.rs").unwrap();
        assert_eq!(url.to_scp_arg(), "server:/data/file.rs");
    }

    #[test]
    fn is_scp_url_positive() {
        assert!(is_scp_url("scp://host/path"));
        assert!(is_scp_url("scp://"));
    }

    #[test]
    fn is_scp_url_negative() {
        assert!(!is_scp_url("http://host"));
        assert!(!is_scp_url("/local/path"));
        assert!(!is_scp_url(""));
    }

    #[test]
    fn temp_path_deterministic() {
        let url = ScpUrl::parse("scp://host/file.txt").unwrap();
        let p1 = temp_path_for(&url);
        let p2 = temp_path_for(&url);
        assert_eq!(p1, p2);
    }

    #[test]
    fn temp_path_preserves_extension() {
        let url = ScpUrl::parse("scp://host/project/main.rs").unwrap();
        let path = temp_path_for(&url);
        assert_eq!(path.extension().and_then(|e| e.to_str()), Some("rs"));
    }

    #[test]
    fn temp_path_no_extension() {
        let url = ScpUrl::parse("scp://host/bin/myapp").unwrap();
        let path = temp_path_for(&url);
        assert!(path.extension().is_none() || path.to_str().unwrap().contains("helix-scp-"));
        let name = path.file_name().unwrap().to_str().unwrap();
        assert!(!name.contains('.'));
    }
}
