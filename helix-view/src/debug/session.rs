use helix_dap::connection::{ConnectionConfig, ConnectionManager, ConnectionState};
use helix_dap::helix_dap_types::{DebuggerCapabilities, Scope, StackFrame, Thread, ThreadId, Variable};
use helix_dap::transport::AdapterMessage;
use helix_dap::Client as DapClient;
use std::sync::Arc;
use tokio::sync::mpsc::UnboundedReceiver;

/// A variable node in the tree, tracking expansion state and children.
#[derive(Debug, Clone)]
pub struct VariableNode {
    pub variable: Variable,
    pub children: Vec<VariableNode>,
    pub expanded: bool,
    pub depth: usize,
}

impl VariableNode {
    pub fn from_variable(var: Variable, depth: usize) -> Self {
        Self {
            variable: var,
            children: Vec::new(),
            expanded: false,
            depth,
        }
    }

    pub fn is_expandable(&self) -> bool {
        self.variable.variables_reference > 0
    }

    /// Convert a flat list of variables into top-level nodes.
    pub fn from_variables(vars: Vec<Variable>) -> Vec<VariableNode> {
        vars.into_iter()
            .map(|v| VariableNode::from_variable(v, 0))
            .collect()
    }
}

/// Which UI panel currently has focus in debug mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DebugFocus {
    /// The code editor has focus (Ctrl-d prefix for debug actions).
    Code,
    /// The sidebar (variables/callstack/threads) has focus.
    Sidebar,
    /// The debug console has focus.
    Console,
}

impl Default for DebugFocus {
    fn default() -> Self {
        DebugFocus::Code
    }
}

/// The active state of a debug session, tracking threads, stack frames,
/// adapter capabilities, and UI focus.
#[derive(Debug)]
pub struct DebugSession {
    /// Whether debug mode overlay is active.
    pub active: bool,
    /// Which UI panel currently has focus.
    pub focus: DebugFocus,
    /// Connection state manager.
    pub connection: ConnectionManager,
    /// Adapter capabilities (set after initialize response).
    pub capabilities: Option<DebuggerCapabilities>,
    /// Active threads from the adapter.
    pub threads: Vec<Thread>,
    /// The currently focused thread.
    pub active_thread_id: Option<ThreadId>,
    /// Stack frames for the active thread.
    pub stack_frames: Vec<StackFrame>,
    /// The currently selected stack frame index.
    pub active_frame_index: usize,
    /// Whether the debuggee is currently stopped.
    pub is_stopped: bool,
    /// Optional stop reason for display.
    pub stop_reason: Option<String>,
    /// The active DAP client (None when no debug session).
    pub client: Option<Arc<DapClient>>,
    /// Event receiver from the DAP transport.
    pub event_rx: Option<UnboundedReceiver<AdapterMessage>>,
    /// Output lines from the debug adapter (category, text).
    pub output_lines: Vec<(String, String)>,
    /// Scopes for the current frame.
    pub scopes: Vec<Scope>,
    /// Variables for each scope as a tree of expandable nodes.
    pub variables: Vec<(String, Vec<VariableNode>)>,
    /// PID of the debug adapter process for cleanup on drop.
    pub adapter_pid: Option<u32>,
}

impl Default for DebugSession {
    fn default() -> Self {
        Self {
            active: false,
            focus: DebugFocus::default(),
            connection: ConnectionManager::new(ConnectionConfig::default()),
            capabilities: None,
            threads: Vec::new(),
            active_thread_id: None,
            stack_frames: Vec::new(),
            active_frame_index: 0,
            is_stopped: false,
            stop_reason: None,
            client: None,
            event_rx: None,
            output_lines: Vec::new(),
            scopes: Vec::new(),
            variables: Vec::new(),
            adapter_pid: None,
        }
    }
}

impl Drop for DebugSession {
    fn drop(&mut self) {
        #[cfg(unix)]
        if let Some(pid) = self.adapter_pid {
            unsafe { libc::killpg(pid as i32, libc::SIGKILL); }
        }
    }
}

impl DebugSession {
    pub fn is_active(&self) -> bool {
        self.active
    }

    pub fn connection_state(&self) -> &ConnectionState {
        self.connection.state()
    }

    /// Get the currently active stack frame, if any.
    pub fn current_frame(&self) -> Option<&StackFrame> {
        self.stack_frames.get(self.active_frame_index)
    }

    /// Flatten all visible variable nodes into a list of (scope_idx, node reference, path).
    /// Only includes children of expanded nodes.
    pub fn flatten_variables(&self) -> Vec<(usize, Vec<usize>, &VariableNode)> {
        let mut result = Vec::new();
        for (scope_idx, (_name, nodes)) in self.variables.iter().enumerate() {
            Self::flatten_nodes(nodes, scope_idx, &mut Vec::new(), &mut result);
        }
        result
    }

    fn flatten_nodes<'a>(
        nodes: &'a [VariableNode],
        scope_idx: usize,
        path: &mut Vec<usize>,
        result: &mut Vec<(usize, Vec<usize>, &'a VariableNode)>,
    ) {
        for (i, node) in nodes.iter().enumerate() {
            path.push(i);
            result.push((scope_idx, path.clone(), node));
            if node.expanded && !node.children.is_empty() {
                Self::flatten_nodes(&node.children, scope_idx, path, result);
            }
            path.pop();
        }
    }

    /// Toggle a variable node's expanded state. Returns Some(variables_reference) if
    /// the node is being expanded and has no cached children (needs async fetch).
    pub fn toggle_variable(&mut self, scope_idx: usize, path: &[usize]) -> Option<usize> {
        if let Some((_name, nodes)) = self.variables.get_mut(scope_idx) {
            if let Some(node) = Self::find_node_mut(nodes, path) {
                if !node.is_expandable() {
                    return None;
                }
                node.expanded = !node.expanded;
                if node.expanded && node.children.is_empty() {
                    return Some(node.variable.variables_reference);
                }
            }
        }
        None
    }

    fn find_node_mut<'a>(nodes: &'a mut [VariableNode], path: &[usize]) -> Option<&'a mut VariableNode> {
        if path.is_empty() {
            return None;
        }
        let node = nodes.get_mut(path[0])?;
        if path.len() == 1 {
            Some(node)
        } else {
            Self::find_node_mut(&mut node.children, &path[1..])
        }
    }

    /// Set children for a variable node identified by its variables_reference.
    pub fn set_variable_children(&mut self, variables_reference: usize, children: Vec<Variable>) {
        for (_name, nodes) in &mut self.variables {
            if Self::set_children_recursive(nodes, variables_reference, &children) {
                return;
            }
        }
    }

    fn set_children_recursive(
        nodes: &mut [VariableNode],
        variables_reference: usize,
        children: &[Variable],
    ) -> bool {
        for node in nodes.iter_mut() {
            if node.variable.variables_reference == variables_reference {
                let depth = node.depth + 1;
                node.children = children
                    .iter()
                    .map(|v| VariableNode::from_variable(v.clone(), depth))
                    .collect();
                return true;
            }
            if Self::set_children_recursive(&mut node.children, variables_reference, children) {
                return true;
            }
        }
        false
    }

    /// Reset session state (called on disconnect).
    pub fn reset(&mut self) {
        self.active = false;
        self.focus = DebugFocus::Code;
        self.capabilities = None;
        self.threads.clear();
        self.active_thread_id = None;
        self.stack_frames.clear();
        self.active_frame_index = 0;
        self.is_stopped = false;
        self.stop_reason = None;
        self.client = None;
        self.event_rx = None;
        self.output_lines.clear();
        self.scopes.clear();
        self.variables.clear();
        self.connection.disconnect();
        // Kill process group before clearing the PID
        #[cfg(unix)]
        if let Some(pid) = self.adapter_pid.take() {
            unsafe { libc::killpg(pid as i32, libc::SIGKILL); }
        }
        self.adapter_pid = None;
    }
}
