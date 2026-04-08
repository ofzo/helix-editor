use std::time::{Duration, Instant};

/// The current state of the debug adapter connection.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConnectionState {
    /// No active connection.
    Disconnected,
    /// Connection is being established.
    Connecting,
    /// Fully connected and operational.
    Connected,
    /// Connection was lost, attempting to reconnect (attach mode only).
    Reconnecting { attempt: u32 },
}

impl ConnectionState {
    pub fn is_connected(&self) -> bool {
        matches!(self, ConnectionState::Connected)
    }

    pub fn is_active(&self) -> bool {
        !matches!(self, ConnectionState::Disconnected)
    }
}

impl std::fmt::Display for ConnectionState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConnectionState::Disconnected => write!(f, "Disconnected"),
            ConnectionState::Connecting => write!(f, "Connecting"),
            ConnectionState::Connected => write!(f, "Connected"),
            ConnectionState::Reconnecting { attempt } => {
                write!(f, "Reconnecting (attempt {attempt})")
            }
        }
    }
}

/// How the debug session was started, which determines reconnection behavior.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RequestMode {
    /// Session started via `launch` — no reconnection on disconnect.
    Launch,
    /// Session started via `attach` — reconnection may be attempted.
    Attach,
}

/// Configuration for connection management behavior.
#[derive(Debug, Clone)]
pub struct ConnectionConfig {
    /// Maximum number of reconnection attempts (attach mode only).
    pub max_retries: u32,
    /// Base delay for exponential backoff (1s → 2s → 4s).
    pub retry_base_delay: Duration,
    /// Interval between heartbeat probes.
    pub heartbeat_interval: Duration,
    /// Maximum time to wait for an adapter connection.
    pub connection_timeout: Duration,
}

impl Default for ConnectionConfig {
    fn default() -> Self {
        Self {
            max_retries: 3,
            retry_base_delay: Duration::from_secs(1),
            heartbeat_interval: Duration::from_secs(10),
            connection_timeout: Duration::from_secs(10),
        }
    }
}

/// Manages connection state, heartbeat detection, and reconnection logic.
#[derive(Debug)]
pub struct ConnectionManager {
    state: ConnectionState,
    config: ConnectionConfig,
    request_mode: Option<RequestMode>,
    last_heartbeat_response: Option<Instant>,
}

impl ConnectionManager {
    pub fn new(config: ConnectionConfig) -> Self {
        Self {
            state: ConnectionState::Disconnected,
            config,
            request_mode: None,
            last_heartbeat_response: None,
        }
    }

    pub fn state(&self) -> &ConnectionState {
        &self.state
    }

    pub fn request_mode(&self) -> Option<RequestMode> {
        self.request_mode
    }

    pub fn config(&self) -> &ConnectionConfig {
        &self.config
    }

    /// Transition to the Connecting state when starting a session.
    pub fn start_connecting(&mut self, mode: RequestMode) {
        self.request_mode = Some(mode);
        self.state = ConnectionState::Connecting;
        self.last_heartbeat_response = None;
    }

    /// Transition to Connected when the adapter sends `initialized`.
    pub fn mark_connected(&mut self) {
        self.state = ConnectionState::Connected;
        self.last_heartbeat_response = Some(Instant::now());
    }

    /// Record a successful heartbeat response.
    pub fn record_heartbeat(&mut self) {
        self.last_heartbeat_response = Some(Instant::now());
    }

    /// Check if a heartbeat probe should be sent.
    pub fn should_probe(&self) -> bool {
        if !self.state.is_connected() {
            return false;
        }

        self.last_heartbeat_response
            .map(|t| t.elapsed() >= self.config.heartbeat_interval)
            .unwrap_or(true)
    }

    /// Check if the connection appears stale (no heartbeat response for too long).
    pub fn is_heartbeat_stale(&self) -> bool {
        if !self.state.is_connected() {
            return false;
        }

        // Consider stale if no response for 1.5x the heartbeat interval
        let stale_threshold = self.config.heartbeat_interval + self.config.heartbeat_interval / 2;
        self.last_heartbeat_response
            .map(|t| t.elapsed() >= stale_threshold)
            .unwrap_or(false)
    }

    /// Handle connection loss. Returns the new state after handling.
    ///
    /// - For Launch mode: always disconnects.
    /// - For Attach mode: attempts reconnection up to max_retries.
    pub fn handle_connection_lost(&mut self) -> &ConnectionState {
        match self.request_mode {
            Some(RequestMode::Attach) => {
                let attempt = match &self.state {
                    ConnectionState::Reconnecting { attempt } => attempt + 1,
                    _ => 1,
                };

                if attempt > self.config.max_retries {
                    self.state = ConnectionState::Disconnected;
                    self.request_mode = None;
                } else {
                    self.state = ConnectionState::Reconnecting { attempt };
                }
            }
            _ => {
                self.state = ConnectionState::Disconnected;
                self.request_mode = None;
            }
        }

        &self.state
    }

    /// Get the delay before the next reconnection attempt (exponential backoff).
    pub fn reconnect_delay(&self) -> Option<Duration> {
        match &self.state {
            ConnectionState::Reconnecting { attempt } => {
                let delay = self.config.retry_base_delay * 2u32.saturating_pow(attempt - 1);
                Some(delay)
            }
            _ => None,
        }
    }

    /// Mark the connection as fully disconnected (manual disconnect or cleanup).
    pub fn disconnect(&mut self) {
        self.state = ConnectionState::Disconnected;
        self.request_mode = None;
        self.last_heartbeat_response = None;
    }

    /// Check if reconnection should be attempted.
    pub fn should_reconnect(&self) -> bool {
        matches!(self.state, ConnectionState::Reconnecting { .. })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_launch_no_reconnect() {
        let mut mgr = ConnectionManager::new(ConnectionConfig::default());
        mgr.start_connecting(RequestMode::Launch);
        assert_eq!(mgr.state(), &ConnectionState::Connecting);

        mgr.mark_connected();
        assert_eq!(mgr.state(), &ConnectionState::Connected);

        mgr.handle_connection_lost();
        assert_eq!(mgr.state(), &ConnectionState::Disconnected);
        assert!(!mgr.should_reconnect());
    }

    #[test]
    fn test_attach_reconnect_attempts() {
        let mut mgr = ConnectionManager::new(ConnectionConfig {
            max_retries: 3,
            ..Default::default()
        });
        mgr.start_connecting(RequestMode::Attach);
        mgr.mark_connected();

        // First loss: reconnect attempt 1
        mgr.handle_connection_lost();
        assert_eq!(
            mgr.state(),
            &ConnectionState::Reconnecting { attempt: 1 }
        );
        assert!(mgr.should_reconnect());

        // Second loss: attempt 2
        mgr.handle_connection_lost();
        assert_eq!(
            mgr.state(),
            &ConnectionState::Reconnecting { attempt: 2 }
        );

        // Third loss: attempt 3
        mgr.handle_connection_lost();
        assert_eq!(
            mgr.state(),
            &ConnectionState::Reconnecting { attempt: 3 }
        );

        // Fourth loss: max retries exceeded
        mgr.handle_connection_lost();
        assert_eq!(mgr.state(), &ConnectionState::Disconnected);
        assert!(!mgr.should_reconnect());
    }

    #[test]
    fn test_reconnect_delay_exponential_backoff() {
        let mut mgr = ConnectionManager::new(ConnectionConfig {
            retry_base_delay: Duration::from_secs(1),
            max_retries: 3,
            ..Default::default()
        });
        mgr.start_connecting(RequestMode::Attach);
        mgr.mark_connected();

        mgr.handle_connection_lost(); // attempt 1
        assert_eq!(mgr.reconnect_delay(), Some(Duration::from_secs(1)));

        mgr.handle_connection_lost(); // attempt 2
        assert_eq!(mgr.reconnect_delay(), Some(Duration::from_secs(2)));

        mgr.handle_connection_lost(); // attempt 3
        assert_eq!(mgr.reconnect_delay(), Some(Duration::from_secs(4)));
    }

    #[test]
    fn test_manual_disconnect() {
        let mut mgr = ConnectionManager::new(ConnectionConfig::default());
        mgr.start_connecting(RequestMode::Attach);
        mgr.mark_connected();

        mgr.disconnect();
        assert_eq!(mgr.state(), &ConnectionState::Disconnected);
        assert_eq!(mgr.request_mode(), None);
    }

    #[test]
    fn test_state_display() {
        assert_eq!(ConnectionState::Disconnected.to_string(), "Disconnected");
        assert_eq!(ConnectionState::Connecting.to_string(), "Connecting");
        assert_eq!(ConnectionState::Connected.to_string(), "Connected");
        assert_eq!(
            ConnectionState::Reconnecting { attempt: 2 }.to_string(),
            "Reconnecting (attempt 2)"
        );
    }
}
