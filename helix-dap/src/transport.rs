use anyhow::Context;
use log::{error, info, warn};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::{
    io::{AsyncBufRead, AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, AsyncWrite, BufReader, BufWriter},
    process::{ChildStderr, ChildStdin, ChildStdout},
    sync::{
        mpsc::{unbounded_channel, UnboundedReceiver, UnboundedSender},
        Mutex,
    },
};

use crate::{Error, Result};

/// Payload sent from the client to the transport for delivery to the debug adapter.
#[derive(Debug)]
pub enum Payload {
    /// A DAP request with a channel to receive the response.
    Request {
        seq: u64,
        chan: tokio::sync::oneshot::Sender<Result<Value>>,
        value: Value,
    },
    /// A DAP event or reverse request (sent from adapter, forwarded to client).
    Event(Value),
}

/// A message received from the debug adapter.
#[derive(Debug, Clone)]
pub enum AdapterMessage {
    /// A response to a request we sent.
    Response(helix_dap_types::Response),
    /// An event from the adapter.
    Event(helix_dap_types::Event),
}

/// Read a DAP message from a stream using Content-Length header framing.
///
/// DAP uses the same framing as LSP:
/// ```text
/// Content-Length: <length>\r\n
/// \r\n
/// <JSON payload>
/// ```
async fn recv_dap_message(
    reader: &mut (impl AsyncBufRead + Unpin + Send),
    buffer: &mut String,
    content: &mut Vec<u8>,
    name: &str,
) -> Result<Value> {
    let mut content_length = None;
    loop {
        buffer.clear();
        if reader.read_line(buffer).await? == 0 {
            return Err(Error::StreamClosed);
        }

        if buffer == "\r\n" {
            break;
        }

        let header = buffer.trim();
        if let Some(("Content-Length", value)) = header.split_once(": ") {
            content_length = Some(
                value
                    .parse::<usize>()
                    .context("invalid content length")?,
            );
        }
    }

    let content_length = content_length.context("missing content length")?;
    content.resize(content_length, 0);
    reader.read_exact(content).await?;

    let msg = std::str::from_utf8(content).context("invalid utf8 from debug adapter")?;
    info!("{name} <- {msg}");

    let value: Value = serde_json::from_slice(content)?;
    content.clear();

    Ok(value)
}

/// Write a DAP message to a stream with Content-Length header framing.
async fn send_dap_message(
    writer: &mut (impl AsyncWrite + Unpin + Send),
    json: &str,
    name: &str,
) -> Result<()> {
    info!("{name} -> {json}");

    let header = format!("Content-Length: {}\r\n\r\n", json.len());
    writer.write_all(header.as_bytes()).await?;
    writer.write_all(json.as_bytes()).await?;
    writer.flush().await?;

    Ok(())
}

/// Transport manages the low-level communication with a DAP debug adapter process.
///
/// It spawns async tasks for reading from stdout, reading from stderr,
/// and writing to stdin, communicating via channels.
#[derive(Debug)]
pub struct Transport {
    name: String,
    pending_requests: Mutex<HashMap<u64, tokio::sync::oneshot::Sender<Result<Value>>>>,
    /// Sender for writing messages back to the adapter (for reverse request responses).
    payload_tx: UnboundedSender<Payload>,
}

impl Transport {
    /// Start the transport, spawning read/write/error tasks.
    ///
    /// Returns:
    /// - A receiver for events/reverse requests from the adapter
    /// - A sender for payloads to send to the adapter
    pub fn start(
        server_stdout: BufReader<ChildStdout>,
        server_stdin: BufWriter<ChildStdin>,
        server_stderr: BufReader<ChildStderr>,
        name: String,
    ) -> (
        UnboundedReceiver<AdapterMessage>,
        UnboundedSender<Payload>,
    ) {
        let (event_tx, event_rx) = unbounded_channel();
        let (payload_tx, payload_rx) = unbounded_channel();

        let transport = Arc::new(Self {
            name,
            pending_requests: Mutex::new(HashMap::default()),
            payload_tx: payload_tx.clone(),
        });

        tokio::spawn(Self::recv_task(
            transport.clone(),
            server_stdout,
            event_tx,
        ));
        tokio::spawn(Self::err_task(transport.clone(), server_stderr));
        tokio::spawn(Self::send_task(
            transport,
            server_stdin,
            payload_rx,
        ));

        (event_rx, payload_tx)
    }

    /// Start the transport over a TCP connection.
    ///
    /// Returns the same channel pair as `start()`.
    pub fn start_tcp(
        reader: tokio::net::tcp::OwnedReadHalf,
        writer: tokio::net::tcp::OwnedWriteHalf,
        name: String,
    ) -> (
        UnboundedReceiver<AdapterMessage>,
        UnboundedSender<Payload>,
    ) {
        let (event_tx, event_rx) = unbounded_channel();
        let (payload_tx, payload_rx) = unbounded_channel();

        let transport = Arc::new(Self {
            name,
            pending_requests: Mutex::new(HashMap::default()),
            payload_tx: payload_tx.clone(),
        });

        let buf_reader = BufReader::new(reader);
        let buf_writer = BufWriter::new(writer);

        tokio::spawn(Self::recv_generic_task(
            transport.clone(),
            buf_reader,
            event_tx,
        ));
        tokio::spawn(Self::send_generic_task(
            transport,
            buf_writer,
            payload_rx,
        ));

        (event_rx, payload_tx)
    }

    /// Receive loop for stdio: reads from ChildStdout.
    async fn recv_task(
        transport: Arc<Self>,
        server_stdout: BufReader<ChildStdout>,
        event_tx: UnboundedSender<AdapterMessage>,
    ) {
        Self::recv_generic_task(transport, server_stdout, event_tx).await;
    }

    /// Generic receive loop that works with any AsyncBufRead.
    async fn recv_generic_task(
        transport: Arc<Self>,
        mut reader: impl AsyncBufRead + Unpin + Send,
        event_tx: UnboundedSender<AdapterMessage>,
    ) {
        let mut recv_buffer = String::new();
        let mut content_buffer = Vec::new();

        loop {
            match recv_dap_message(
                &mut reader,
                &mut recv_buffer,
                &mut content_buffer,
                &transport.name,
            )
            .await
            {
                Ok(msg) => {
                    if let Err(err) = transport.process_message(&event_tx, msg).await {
                        error!("{} error processing message: {err:?}", transport.name);
                        break;
                    }
                }
                Err(Error::StreamClosed) => {
                    info!("{} stream closed", transport.name);
                    break;
                }
                Err(err) => {
                    error!("{} recv error: {err:?}", transport.name);
                    break;
                }
            }
        }

        // Close outstanding requests
        for (_seq, tx) in transport.pending_requests.lock().await.drain() {
            let _ = tx.send(Err(Error::StreamClosed));
        }
    }

    /// Stderr reading task.
    async fn err_task(transport: Arc<Self>, mut server_stderr: BufReader<ChildStderr>) {
        let mut buffer = String::new();
        loop {
            buffer.clear();
            match server_stderr.read_line(&mut buffer).await {
                Ok(0) => break,
                Ok(_) => {
                    warn!("{} stderr: {}", transport.name, buffer.trim());
                }
                Err(err) => {
                    error!("{} stderr read error: {err:?}", transport.name);
                    break;
                }
            }
        }
    }

    /// Send loop for stdio: writes to ChildStdin.
    async fn send_task(
        transport: Arc<Self>,
        server_stdin: BufWriter<ChildStdin>,
        payload_rx: UnboundedReceiver<Payload>,
    ) {
        Self::send_generic_task(transport, server_stdin, payload_rx).await;
    }

    /// Generic send loop that works with any AsyncWrite.
    async fn send_generic_task(
        transport: Arc<Self>,
        mut writer: impl AsyncWrite + Unpin + Send,
        mut payload_rx: UnboundedReceiver<Payload>,
    ) {
        while let Some(payload) = payload_rx.recv().await {
            match payload {
                Payload::Request { seq, chan, value } => {
                    let json = match serde_json::to_string(&value) {
                        Ok(json) => json,
                        Err(err) => {
                            let _ = chan.send(Err(err.into()));
                            continue;
                        }
                    };

                    transport
                        .pending_requests
                        .lock()
                        .await
                        .insert(seq, chan);

                    if let Err(err) = send_dap_message(&mut writer, &json, &transport.name).await {
                        error!("{} send error: {err:?}", transport.name);
                        // Remove the pending request since we failed to send
                        if let Some(tx) = transport.pending_requests.lock().await.remove(&seq) {
                            let _ = tx.send(Err(err));
                        }
                    }
                }
                Payload::Event(value) => {
                    // This path is for sending events/responses back to the adapter
                    // (e.g., runInTerminal reverse request responses)
                    let json = match serde_json::to_string(&value) {
                        Ok(json) => json,
                        Err(err) => {
                            error!("{} serialize error: {err:?}", transport.name);
                            continue;
                        }
                    };
                    if let Err(err) = send_dap_message(&mut writer, &json, &transport.name).await {
                        error!("{} send error: {err:?}", transport.name);
                    }
                }
            }
        }
    }

    /// Process a raw JSON message from the adapter, dispatching to the appropriate handler.
    async fn process_message(
        &self,
        event_tx: &UnboundedSender<AdapterMessage>,
        msg: Value,
    ) -> Result<()> {
        let msg_type = msg
            .get("type")
            .and_then(|t| t.as_str())
            .unwrap_or("unknown");

        match msg_type {
            "response" => {
                let response: helix_dap_types::Response = serde_json::from_value(msg)?;
                self.process_response(response).await?;
            }
            "event" => {
                let event: helix_dap_types::Event = serde_json::from_value(msg)?;
                event_tx
                    .send(AdapterMessage::Event(event))
                    .context("failed to send event to client")?;
            }
            "request" => {
                // Reverse request from adapter (e.g., runInTerminal, startDebugging)
                let seq = msg.get("seq").and_then(|s| s.as_u64()).unwrap_or(0);
                let command = msg.get("command").and_then(|c| c.as_str()).unwrap_or("unknown").to_string();
                warn!("{} received reverse request: {} (seq={})", self.name, command, seq);

                let success = command == "startDebugging";
                let response = serde_json::json!({
                    "seq": 0,
                    "type": "response",
                    "request_seq": seq,
                    "command": command,
                    "success": success,
                });

                // Send response back to adapter
                let _ = self.payload_tx.send(Payload::Event(response));

                // Forward startDebugging as a synthetic event so the client can create a child session
                if command == "startDebugging" {
                    let config = msg.get("arguments")
                        .and_then(|a| a.get("configuration"))
                        .cloned()
                        .unwrap_or(Value::Null);
                    let request_type = msg.get("arguments")
                        .and_then(|a| a.get("request"))
                        .and_then(|r| r.as_str())
                        .unwrap_or("launch")
                        .to_string();
                    event_tx
                        .send(AdapterMessage::Event(helix_dap_types::Event {
                            seq,
                            msg_type: "event".to_string(),
                            event: "startDebugging".to_string(),
                            body: Some(serde_json::json!({
                                "configuration": config,
                                "request": request_type,
                            })),
                        }))
                        .context("failed to forward startDebugging")?;
                }
            }
            other => {
                warn!("{} received unknown message type: {other}", self.name);
            }
        }

        Ok(())
    }

    /// Process a response, resolving the pending request's oneshot channel.
    async fn process_response(&self, response: helix_dap_types::Response) -> Result<()> {
        let seq = response.request_seq;

        if let Some(tx) = self.pending_requests.lock().await.remove(&seq) {
            let result = if response.success {
                Ok(response.body.unwrap_or(Value::Null))
            } else {
                Err(Error::DapError {
                    message: response
                        .message
                        .unwrap_or_else(|| "unknown error".to_string()),
                    body: response.body,
                })
            };
            // Ignore send error: the receiver may have dropped (timeout)
            let _ = tx.send(result);
        } else {
            warn!(
                "{} received response for unknown request seq={seq}",
                self.name
            );
        }

        Ok(())
    }
}
