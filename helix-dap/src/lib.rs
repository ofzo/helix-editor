pub mod client;
pub mod connection;
pub mod transport;

pub use client::Client;
pub use connection::{ConnectionConfig, ConnectionManager, ConnectionState, RequestMode};
pub use helix_dap_types;

use serde_json::Value;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("stream closed")]
    StreamClosed,

    #[error("DAP error: {message}")]
    DapError {
        message: String,
        body: Option<Value>,
    },

    #[error("timeout")]
    Timeout,

    #[error(transparent)]
    Io(#[from] std::io::Error),

    #[error(transparent)]
    Json(#[from] serde_json::Error),

    #[error(transparent)]
    Other(#[from] anyhow::Error),
}
