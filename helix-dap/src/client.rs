use crate::transport::{AdapterMessage, Payload, Transport};
use crate::{Error, Result};

use helix_dap_types::{
    requests::*, DebuggerCapabilities, Source, SourceBreakpoint, ThreadId,
};
use serde::de::DeserializeOwned;
use serde_json::Value;
use std::process::Stdio;
use std::sync::atomic::{AtomicU64, Ordering};
use tokio::{
    io::{BufReader, BufWriter},
    process::{Child, Command},
    sync::mpsc::{UnboundedReceiver, UnboundedSender},
    time,
};

/// Configuration for starting a debug adapter.
#[derive(Debug, Clone)]
pub struct AdapterConfig {
    /// The command to run the debug adapter.
    pub command: String,
    /// Arguments to pass to the debug adapter command.
    pub args: Vec<String>,
    /// Port for TCP connection (if None, use stdio).
    pub port: Option<u16>,
}

/// A DAP client that communicates with a single debug adapter.
pub struct Client {
    name: String,
    _process: Option<Child>,
    /// PID of the adapter process (used for process group cleanup).
    process_id: Option<u32>,
    server_tx: UnboundedSender<Payload>,
    request_counter: AtomicU64,
    pub capabilities: Option<DebuggerCapabilities>,
    pub request_timeout: u64,
}

impl Client {
    /// Start a debug adapter process using stdio transport.
    pub fn start(
        config: &AdapterConfig,
        name: String,
    ) -> Result<(Self, UnboundedReceiver<AdapterMessage>)> {
        let mut cmd = Command::new(&config.command);
        cmd.args(&config.args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .kill_on_drop(true);
        #[cfg(unix)]
        cmd.process_group(0);
        let mut process = cmd.spawn()
            .map_err(|e| {
                Error::Other(anyhow::anyhow!(
                    "failed to start debug adapter '{}': {e}",
                    config.command
                ))
            })?;

        let stdin = process.stdin.take().unwrap();
        let stdout = process.stdout.take().unwrap();
        let stderr = process.stderr.take().unwrap();

        let (event_rx, server_tx) = Transport::start(
            BufReader::new(stdout),
            BufWriter::new(stdin),
            BufReader::new(stderr),
            name.clone(),
        );

        let pid = process.id();
        let client = Self {
            name,
            _process: Some(process),
            process_id: pid,
            server_tx,
            request_counter: AtomicU64::new(1),
            capabilities: None,
            request_timeout: 10,
        };

        Ok((client, event_rx))
    }

    /// Start a debug adapter process that communicates over TCP.
    ///
    /// Spawns the adapter process with the given port argument, waits for it to
    /// start listening, then connects via TCP. The process is kept alive and
    /// killed when the client is dropped.
    pub async fn start_tcp(
        config: &AdapterConfig,
        port: u16,
        port_arg_template: &str,
        name: String,
    ) -> Result<(Self, UnboundedReceiver<AdapterMessage>)> {
        // Build the port argument by replacing `{}` with the port number
        let port_arg = port_arg_template.replace("{}", &port.to_string());
        let port_args: Vec<&str> = port_arg.split_whitespace().collect();

        let mut args = config.args.clone();
        for a in port_args {
            args.push(a.to_string());
        }

        let mut cmd = Command::new(&config.command);
        cmd.args(&args)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .kill_on_drop(true);
        #[cfg(unix)]
        cmd.process_group(0);
        let process = cmd.spawn()
            .map_err(|e| {
                Error::Other(anyhow::anyhow!(
                    "failed to start debug adapter '{}': {e}",
                    config.command
                ))
            })?;

        // Give the adapter time to start listening
        let addr = format!("127.0.0.1:{}", port);
        for _ in 0..50 {
            // Try for up to 5 seconds
            time::sleep(std::time::Duration::from_millis(100)).await;
            if let Ok(stream) = tokio::net::TcpStream::connect(&addr).await {
                let (reader, writer) = stream.into_split();
                let (event_rx, server_tx) = Transport::start_tcp(reader, writer, name.clone());

                let pid = process.id();
                let client = Self {
                    name,
                    _process: Some(process),
                    process_id: pid,
                    server_tx,
                    request_counter: AtomicU64::new(1),
                    capabilities: None,
                    request_timeout: 10,
                };
                return Ok((client, event_rx));
            }
        }

        Err(Error::Other(anyhow::anyhow!(
            "timed out connecting to debug adapter at {}",
            addr
        )))
    }

    /// Connect to an already-running debug adapter over TCP.
    pub async fn connect_tcp(
        addr: &str,
        name: String,
    ) -> Result<(Self, UnboundedReceiver<AdapterMessage>)> {
        let stream = tokio::net::TcpStream::connect(addr).await?;
        let (reader, writer) = stream.into_split();

        let (event_rx, server_tx) = Transport::start_tcp(reader, writer, name.clone());

        let client = Self {
            name,
            _process: None,
            process_id: None,
            server_tx,
            request_counter: AtomicU64::new(1),
            capabilities: None,
            request_timeout: 10,
        };

        Ok((client, event_rx))
    }

    fn next_seq(&self) -> u64 {
        self.request_counter.fetch_add(1, Ordering::Relaxed)
    }

    /// Send a raw DAP request and wait for the response.
    async fn request(&self, command: &str, arguments: Option<Value>) -> Result<Value> {
        let rx = self.send_request(command, arguments)?;

        let timeout = time::Duration::from_secs(self.request_timeout);
        match time::timeout(timeout, rx).await {
            Ok(Ok(result)) => result,
            Ok(Err(_)) => Err(Error::StreamClosed),
            Err(_) => Err(Error::Timeout),
        }
    }

    /// Send a raw DAP request without waiting. Returns a receiver for the response.
    pub fn send_request(
        &self,
        command: &str,
        arguments: Option<Value>,
    ) -> Result<tokio::sync::oneshot::Receiver<Result<Value>>> {
        let seq = self.next_seq();

        let request = serde_json::json!({
            "seq": seq,
            "type": "request",
            "command": command,
            "arguments": arguments,
        });

        let (tx, rx) = tokio::sync::oneshot::channel();

        self.server_tx
            .send(Payload::Request {
                seq,
                chan: tx,
                value: request,
            })
            .map_err(|_| Error::StreamClosed)?;

        Ok(rx)
    }

    /// Send a typed request and deserialize the response body.
    async fn request_typed<R: DeserializeOwned>(
        &self,
        command: &str,
        arguments: Option<Value>,
    ) -> Result<R> {
        let value = self.request(command, arguments).await?;
        serde_json::from_value(value).map_err(Into::into)
    }

    // ─── DAP Lifecycle Requests ────────────────────────────────

    /// Send the `initialize` request. Stores capabilities from the response.
    pub async fn initialize(&mut self, adapter_id: String) -> Result<DebuggerCapabilities> {
        let args = InitializeArguments {
            client_id: Some("helix".to_string()),
            client_name: Some("Helix Editor".to_string()),
            adapter_id,
            locale: Some("en-US".to_string()),
            lines_start_at_1: Some(true),
            columns_start_at_1: Some(true),
            path_format: Some("path".to_string()),
            supports_variable_type: Some(true),
            supports_variable_paging: Some(false),
            supports_run_in_terminal_request: Some(false),
            supports_memory_references: Some(false),
            supports_progress_reporting: Some(false),
            supports_invalidated_event: Some(false),
            supports_memory_event: Some(false),
        };

        let value = self
            .request(
                commands::INITIALIZE,
                Some(serde_json::to_value(&args)?),
            )
            .await?;

        let capabilities: DebuggerCapabilities = serde_json::from_value(value)?;
        self.capabilities = Some(capabilities.clone());
        Ok(capabilities)
    }

    /// Send the `launch` request with the given configuration.
    pub async fn launch(&self, config: Value) -> Result<()> {
        self.request(commands::LAUNCH, Some(config)).await?;
        Ok(())
    }

    /// Send the `attach` request with the given configuration.
    pub async fn attach(&self, config: Value) -> Result<()> {
        self.request(commands::ATTACH, Some(config)).await?;
        Ok(())
    }

    /// Send the `disconnect` request.
    pub async fn disconnect(
        &self,
        restart: Option<bool>,
        terminate_debuggee: Option<bool>,
    ) -> Result<()> {
        let args = DisconnectArguments {
            restart,
            terminate_debuggee,
            suspend_debuggee: None,
        };
        self.request(
            commands::DISCONNECT,
            Some(serde_json::to_value(&args)?),
        )
        .await?;
        Ok(())
    }

    /// Get the adapter process ID, if we spawned one.
    pub fn process_id(&self) -> Option<u32> {
        self.process_id
    }

    /// Kill the debug adapter process and its entire process group.
    pub fn kill_process_group(&self) {
        #[cfg(unix)]
        if let Some(pid) = self.process_id {
            log::warn!("Killing debug adapter process group (pid={})", pid);
            // SIGKILL the entire process group
            unsafe { libc::killpg(pid as i32, libc::SIGKILL); }
        }
    }

    /// Send the `terminate` request.
    pub async fn terminate(&self, restart: Option<bool>) -> Result<()> {
        let args = TerminateArguments { restart };
        self.request(
            commands::TERMINATE,
            Some(serde_json::to_value(&args)?),
        )
        .await?;
        Ok(())
    }

    /// Send the `configurationDone` request.
    pub async fn configuration_done(&self) -> Result<()> {
        self.request(commands::CONFIGURATION_DONE, None).await?;
        Ok(())
    }

    /// Send the `restart` request.
    pub async fn restart(&self, args: Option<Value>) -> Result<()> {
        self.request(commands::RESTART, args).await?;
        Ok(())
    }

    // ─── Breakpoint Requests ──────────────────────────────────

    /// Send the `setBreakpoints` request.
    pub async fn set_breakpoints(
        &self,
        source: Source,
        breakpoints: Vec<SourceBreakpoint>,
    ) -> Result<SetBreakpointsResponseBody> {
        let args = SetBreakpointsArguments {
            source,
            breakpoints: Some(breakpoints),
            source_modified: None,
        };
        self.request_typed(
            commands::SET_BREAKPOINTS,
            Some(serde_json::to_value(&args)?),
        )
        .await
    }

    /// Send the `setFunctionBreakpoints` request.
    pub async fn set_function_breakpoints(
        &self,
        breakpoints: Vec<helix_dap_types::FunctionBreakpoint>,
    ) -> Result<SetBreakpointsResponseBody> {
        let args = SetFunctionBreakpointsArguments { breakpoints };
        self.request_typed(
            commands::SET_FUNCTION_BREAKPOINTS,
            Some(serde_json::to_value(&args)?),
        )
        .await
    }

    /// Send the `setExceptionBreakpoints` request.
    pub async fn set_exception_breakpoints(
        &self,
        filters: Vec<String>,
    ) -> Result<()> {
        let args = SetExceptionBreakpointsArguments {
            filters,
            filter_options: None,
            exception_options: None,
        };
        self.request(
            commands::SET_EXCEPTION_BREAKPOINTS,
            Some(serde_json::to_value(&args)?),
        )
        .await?;
        Ok(())
    }

    // ─── Execution Control Requests ───────────────────────────

    /// Send the `continue` request.
    pub async fn resume(&self, thread_id: ThreadId) -> Result<ContinueResponseBody> {
        let args = ContinueArguments {
            thread_id,
            single_thread: None,
        };
        self.request_typed(
            commands::CONTINUE,
            Some(serde_json::to_value(&args)?),
        )
        .await
    }

    /// Send the `next` (step over) request.
    pub async fn next(&self, thread_id: ThreadId) -> Result<()> {
        let args = NextArguments {
            thread_id,
            single_thread: None,
            granularity: None,
        };
        self.request(
            commands::NEXT,
            Some(serde_json::to_value(&args)?),
        )
        .await?;
        Ok(())
    }

    /// Send the `stepIn` request.
    pub async fn step_in(&self, thread_id: ThreadId) -> Result<()> {
        let args = StepInArguments {
            thread_id,
            single_thread: None,
            target_id: None,
            granularity: None,
        };
        self.request(
            commands::STEP_IN,
            Some(serde_json::to_value(&args)?),
        )
        .await?;
        Ok(())
    }

    /// Send the `stepOut` request.
    pub async fn step_out(&self, thread_id: ThreadId) -> Result<()> {
        let args = StepOutArguments {
            thread_id,
            single_thread: None,
            granularity: None,
        };
        self.request(
            commands::STEP_OUT,
            Some(serde_json::to_value(&args)?),
        )
        .await?;
        Ok(())
    }

    /// Send the `pause` request.
    pub async fn pause(&self, thread_id: ThreadId) -> Result<()> {
        let args = PauseArguments { thread_id };
        self.request(
            commands::PAUSE,
            Some(serde_json::to_value(&args)?),
        )
        .await?;
        Ok(())
    }

    /// Send the `stepBack` request.
    pub async fn step_back(&self, thread_id: ThreadId) -> Result<()> {
        let args = StepBackArguments {
            thread_id,
            single_thread: None,
            granularity: None,
        };
        self.request(
            commands::STEP_BACK,
            Some(serde_json::to_value(&args)?),
        )
        .await?;
        Ok(())
    }

    // ─── Inspection Requests ──────────────────────────────────

    /// Send the `threads` request.
    pub async fn threads(&self) -> Result<ThreadsResponseBody> {
        self.request_typed(commands::THREADS, None).await
    }

    /// Send the `stackTrace` request.
    pub async fn stack_trace(
        &self,
        thread_id: ThreadId,
    ) -> Result<StackTraceResponseBody> {
        let args = StackTraceArguments {
            thread_id,
            start_frame: None,
            levels: Some(20),
            format: None,
        };
        self.request_typed(
            commands::STACK_TRACE,
            Some(serde_json::to_value(&args)?),
        )
        .await
    }

    /// Send the `scopes` request.
    pub async fn scopes(&self, frame_id: usize) -> Result<ScopesResponseBody> {
        let args = ScopesArguments { frame_id };
        self.request_typed(
            commands::SCOPES,
            Some(serde_json::to_value(&args)?),
        )
        .await
    }

    /// Send the `variables` request.
    pub async fn variables(
        &self,
        variables_reference: usize,
    ) -> Result<VariablesResponseBody> {
        let args = VariablesArguments {
            variables_reference,
            filter: None,
            start: None,
            count: None,
            format: None,
        };
        self.request_typed(
            commands::VARIABLES,
            Some(serde_json::to_value(&args)?),
        )
        .await
    }

    /// Send the `evaluate` request.
    pub async fn evaluate(
        &self,
        expression: String,
        frame_id: Option<usize>,
        context: Option<helix_dap_types::EvaluateContext>,
    ) -> Result<EvaluateResponseBody> {
        let args = EvaluateArguments {
            expression,
            frame_id,
            context,
            format: None,
        };
        self.request_typed(
            commands::EVALUATE,
            Some(serde_json::to_value(&args)?),
        )
        .await
    }

    /// Send the `setVariable` request.
    pub async fn set_variable(
        &self,
        variables_reference: usize,
        name: String,
        value: String,
    ) -> Result<SetVariableResponseBody> {
        let args = SetVariableArguments {
            variables_reference,
            name,
            value,
            format: None,
        };
        self.request_typed(
            commands::SET_VARIABLE,
            Some(serde_json::to_value(&args)?),
        )
        .await
    }

    /// Send the `completions` request (for debug console).
    pub async fn completions(
        &self,
        frame_id: Option<usize>,
        text: String,
        column: usize,
    ) -> Result<CompletionsResponseBody> {
        let args = CompletionsArguments {
            frame_id,
            text,
            column,
            line: None,
        };
        self.request_typed(
            commands::COMPLETIONS,
            Some(serde_json::to_value(&args)?),
        )
        .await
    }

    /// Send the `exceptionInfo` request.
    pub async fn exception_info(
        &self,
        thread_id: ThreadId,
    ) -> Result<ExceptionInfoResponseBody> {
        let args = ExceptionInfoArguments { thread_id };
        self.request_typed(
            commands::EXCEPTION_INFO,
            Some(serde_json::to_value(&args)?),
        )
        .await
    }

    /// Send the `source` request for a source reference.
    pub async fn source(
        &self,
        source: Option<Source>,
        source_reference: usize,
    ) -> Result<SourceResponseBody> {
        let args = SourceArguments {
            source,
            source_reference,
        };
        self.request_typed(
            commands::SOURCE,
            Some(serde_json::to_value(&args)?),
        )
        .await
    }

    /// Send the `loadedSources` request.
    pub async fn loaded_sources(&self) -> Result<LoadedSourcesResponseBody> {
        self.request_typed(commands::LOADED_SOURCES, None).await
    }

    // ─── Capabilities Helpers ─────────────────────────────────

    /// Check if the adapter supports a given capability.
    pub fn supports_configuration_done(&self) -> bool {
        self.capabilities
            .as_ref()
            .and_then(|c| c.supports_configuration_done_request)
            .unwrap_or(false)
    }

    pub fn supports_function_breakpoints(&self) -> bool {
        self.capabilities
            .as_ref()
            .and_then(|c| c.supports_function_breakpoints)
            .unwrap_or(false)
    }

    pub fn supports_conditional_breakpoints(&self) -> bool {
        self.capabilities
            .as_ref()
            .and_then(|c| c.supports_conditional_breakpoints)
            .unwrap_or(false)
    }

    pub fn supports_hit_conditional_breakpoints(&self) -> bool {
        self.capabilities
            .as_ref()
            .and_then(|c| c.supports_hit_conditional_breakpoints)
            .unwrap_or(false)
    }

    pub fn supports_log_points(&self) -> bool {
        self.capabilities
            .as_ref()
            .and_then(|c| c.supports_log_points)
            .unwrap_or(false)
    }

    pub fn supports_step_back(&self) -> bool {
        self.capabilities
            .as_ref()
            .and_then(|c| c.supports_step_back)
            .unwrap_or(false)
    }

    pub fn supports_restart(&self) -> bool {
        self.capabilities
            .as_ref()
            .and_then(|c| c.supports_restart_request)
            .unwrap_or(false)
    }

    pub fn supports_terminate(&self) -> bool {
        self.capabilities
            .as_ref()
            .and_then(|c| c.supports_terminate_request)
            .unwrap_or(false)
    }

    pub fn supports_completions(&self) -> bool {
        self.capabilities
            .as_ref()
            .and_then(|c| c.supports_completions_request)
            .unwrap_or(false)
    }

    pub fn supports_evaluate_for_hovers(&self) -> bool {
        self.capabilities
            .as_ref()
            .and_then(|c| c.supports_evaluate_for_hovers)
            .unwrap_or(false)
    }

    pub fn supports_exception_info(&self) -> bool {
        self.capabilities
            .as_ref()
            .and_then(|c| c.supports_exception_info_request)
            .unwrap_or(false)
    }

    pub fn supports_set_variable(&self) -> bool {
        self.capabilities
            .as_ref()
            .and_then(|c| c.supports_set_variable)
            .unwrap_or(false)
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl std::fmt::Debug for Client {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Client")
            .field("name", &self.name)
            .field("capabilities", &self.capabilities)
            .finish()
    }
}
