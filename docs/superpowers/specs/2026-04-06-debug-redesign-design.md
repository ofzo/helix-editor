# Helix Debug System Redesign

**Date:** 2026-04-06
**Status:** Draft
**Scope:** Complete removal and redesign of debugging capabilities

## 1. Overview

### 1.1 Motivation

The existing debug implementation in helix has several limitations:
- No dedicated debug mode - debug commands are buried under `space G` prefix
- No persistent UI for variables, call stack, or threads
- No connection resilience (no heartbeat, no reconnection)
- No project-level debug configuration
- Breakpoints are not persisted across sessions
- No clear separation between "entering debug" and "debugging" phases

This redesign creates a first-class debugging experience inspired by VS Code's capabilities while respecting helix's terminal-based, modal editing nature.

### 1.2 Design Principles

1. **Enter vs. Active separation**: Clearly separate the process of starting a debug session (configuration selection, adapter launch, connection) from the active debugging experience (stepping, inspecting, evaluating).
2. **Connection stability first**: The DAP connection layer manages its own lifecycle independently from the UI, with heartbeat detection and automatic reconnection for attach sessions.
3. **Debug mode as first-class citizen**: A dedicated `Debug` mode sits alongside Normal/Insert/Select, with its own keymap and screen layout.
4. **Minimal configuration, maximum power**: Works out of the box via `languages.toml` defaults, with project-level `.helix/debug.toml` for customization.

## 2. Architecture

### 2.1 Module Structure

```
helix-dap-types/     - DAP protocol type definitions (rewritten)
helix-dap/           - DAP client with connection management
  src/
    lib.rs           - Public API
    client.rs        - DAP client (request/response handling)
    transport.rs     - Stdio/TCP transport layer
    connection.rs    - Connection state machine, heartbeat, reconnection
    registry.rs      - Multi-session adapter registry
helix-view/          - Debug state management
  src/
    editor.rs        - Breakpoint storage, debug session state
    handlers/dap.rs  - DAP event handling (stopped, continued, etc.)
    debug/
      mod.rs         - Debug state types
      breakpoints.rs - Breakpoint management and persistence
      session.rs     - Debug session state (layout, focus, panels)
helix-term/          - Debug UI and commands
  src/
    commands/dap.rs  - Debug command implementations
    commands/typed.rs - :debug, :debug-attach, :debug-eval typed commands
    ui/debug/
      mod.rs         - Debug layout manager
      sidebar.rs     - Variables/CallStack/Threads sidebar
      console.rs     - Debug console (output + expression input)
      toolbar.rs     - Debug controls status line
    keymap/default.rs - Debug mode keybindings
```

### 2.2 Data Flow

```
User Action (keystroke/command)
       │
       ▼
Debug Commands (helix-term/commands/dap.rs)
       │
       ▼
DAP Client (helix-dap/client.rs)
       │
       ▼
Transport Layer (helix-dap/transport.rs)  ←→  DAP Adapter Process
       │
       ▼
Connection Manager (helix-dap/connection.rs)
       │ (state changes, events)
       ▼
Event Handler (helix-view/handlers/dap.rs)
       │
       ▼
Debug State (helix-view/debug/)
       │
       ▼
Debug UI (helix-term/ui/debug/)  →  Screen Rendering
```

## 3. Connection Management Layer

### 3.1 Connection State Machine

```
                    ┌─────────────┐
                    │Disconnected │
                    └──────┬──────┘
                           │ launch/attach
                           ▼
                    ┌─────────────┐
              ┌────→│ Connecting  │
              │     └──────┬──────┘
              │            │ initialized event
              │            ▼
              │     ┌─────────────┐
              │     │  Connected  │
              │     └──────┬──────┘
              │            │ connection lost
              │            ▼
              │     ┌──────────────┐
              └─────│ Reconnecting │ (attach only, max 3 retries)
                    └──────┬───────┘
                           │ max retries exceeded
                           ▼
                    ┌─────────────┐
                    │Disconnected │
                    └─────────────┘
```

### 3.2 ConnectionManager

```rust
pub enum ConnectionState {
    Disconnected,
    Connecting,
    Connected,
    Reconnecting { attempt: u32 },
}

pub struct ConnectionConfig {
    pub max_retries: u32,          // default: 3
    pub retry_base_delay: Duration, // default: 1s (exponential backoff: 1s, 2s, 4s)
    pub heartbeat_interval: Duration, // default: 5s
    pub connection_timeout: Duration, // default: 10s
}

pub struct ConnectionManager {
    state: ConnectionState,
    config: ConnectionConfig,
    last_heartbeat_response: Instant,
    request_mode: RequestMode, // Launch or Attach
}
```

### 3.3 Heartbeat & Liveness Detection

Connection liveness is detected through multiple mechanisms:

**Transport-level detection (primary):**
- For **stdio** transport: monitor the adapter process (check if PID is still alive). Process exit = connection lost.
- For **TCP** transport: use TCP keepalive settings on the socket. Connection reset/timeout = connection lost.

**Application-level detection (secondary):**
- Periodically (every 10s) send a `threads` request as a lightweight probe
- If no response within 15s, mark connection as potentially lost
- Note: some adapters may not respond to `threads` while the target is running; the transport-level check is authoritative
- If the adapter responds with an error, the connection is still alive (error != disconnect)

**Disconnect handling:**
- For **Launch** mode: connection loss means the debugged process or adapter died - notify user, offer restart
- For **Attach** mode: attempt automatic reconnection with exponential backoff (the target process may still be alive)

### 3.4 Startup Flows

#### Launch Flow

1. User triggers `:debug` or `space d l`
2. Open configuration picker showing:
   - Project configs from `.helix/debug.toml`
   - Language default templates from `languages.toml`
3. If config has completion parameters (filename, args), prompt user for values
4. Spawn DAP adapter process (show spinner in status bar)
5. Send `initialize` request, negotiate capabilities
6. Send `launch` request with resolved configuration
7. Receive `initialized` event
8. Send all pre-set breakpoints via `setBreakpoints`
9. Send `configurationDone`
10. Enter Debug mode (layout switch, keymap change)

#### Attach Flow

1. User triggers `:debug-attach` or `space d a`
2. Open configuration picker (filtered to attach configs)
3. If PID needed, show process picker (list running processes)
4. Spawn DAP adapter, send `initialize` + `attach`
5. Same steps 7-10 as Launch

### 3.5 Error Recovery

| Scenario | Behavior |
|---|---|
| Adapter spawn failure | Status bar error, show command in console for manual debugging |
| Connection timeout during startup | Retry once, then show error with suggestion |
| Heartbeat timeout (Launch) | Notify "Debugged process terminated", offer restart |
| Heartbeat timeout (Attach) | Auto-reconnect with backoff, show status |
| Max retries exceeded (Attach) | Notify user, offer manual reconnect or exit |
| Transport IO error | Same as heartbeat timeout based on mode |

## 4. Debug Mode

### 4.1 Mode Architecture

Debug mode is implemented as a **meta-mode / overlay** rather than a peer of Normal/Insert/Select. The existing `Mode` enum is not modified. Instead, the editor tracks a separate `debug_active: bool` flag:

```rust
// In Editor (helix-view)
pub struct DebugSession {
    pub active: bool,
    pub focus: DebugFocus,
    pub layout: DebugLayout,
    // ... connection, state, etc.
}

pub enum DebugFocus {
    Code,       // Code editor area - Normal/Insert/Select modes work normally
    Sidebar,    // Variables/CallStack/Threads panel
    Console,    // Debug console input
}
```

**How it works:**
- When `debug_active` is true, the compositor renders the debug layout instead of the normal layout
- The code editor area inside debug layout retains full Normal/Insert/Select mode switching
- Debug-specific keybindings are injected as a **keymap overlay** that intercepts keys before they reach the normal mode keymap
- When focus is on `Code`, **only function-key and Ctrl-prefixed keys** are intercepted for debug actions; single-letter keys pass through to Normal/Insert mode
- When focus is on `Sidebar` or `Console`, the debug keymap handles all input

**Key conflict resolution strategy:**

When code editor has focus:
- `F5` = Continue, `F10` = Step Over, `F11` = Step In, `Shift-F11` = Step Out (VS Code style, no conflicts)
- `Ctrl-d c` = Continue, `Ctrl-d n` = Next, `Ctrl-d s` = Step In, `Ctrl-d o` = Step Out (prefix avoids conflicts)
- Single letters (`h/j/k/l`, `s`, `c`, `n`, etc.) pass through to Normal mode unchanged

When sidebar/console has focus:
- Single-letter debug shortcuts work directly (`c` = continue, `n` = next, etc.)
- `Esc` returns focus to code area

This approach avoids modifying the `Mode` enum (which would have massive blast radius across the codebase) and preserves full editing capability during debugging.

**Entering debug mode:**
1. Debug session connects successfully
2. `debug_active` set to true, focus set to `Code`
3. Compositor switches to debug layout
4. Status line shows debug status
5. Gutter shows breakpoint indicators and current execution line

**Exiting debug mode:**
1. Debug session terminates (user quit or process exit)
2. `debug_active` set to false
3. Compositor restores normal layout
4. Breakpoint indicators remain in gutter (grayed out / hollow)

**Leaving debug layout without stopping:**
- User can press `Ctrl-d h` to **hide** the debug layout and return to normal editing
- The debug session continues running in the background
- Status bar still shows debug status
- `Ctrl-d h` again (or `:debug` when session is active) restores the debug layout
- Breakpoint hits while layout is hidden will restore the debug layout automatically

### 4.2 Screen Layout

```
┌──────────────────────────────────────────────────────────────┐
│ [Debug: Running ▶]  src/main.rs  [ln 15, col 4]   [thread 1]│
├──────────────────────────────┬───────────────────────────────┤
│                              │ [Variables] [CallStack] [Thr] │
│    Code Editor Area          │                               │
│    (Normal mode navigation)  │  ▼ Locals                     │
│                              │    x: i32 = 42                │
│    13 │ fn main() {          │    name: String = "hello"     │
│    14 │     let x = 42;      │    ▶ vec: Vec<i32> [3 items]  │
│  ●>15 │     let y = x + 1;   │  ▼ Arguments                  │
│    16 │     foo(x, y);       │    argc: i32 = 1              │
│    17 │ }                    │                               │
│                              │  ── Call Stack ──             │
│                              │  → main()     main.rs:15      │
│                              │    run()      lib.rs:42       │
│                              │    start()    rt.rs:100       │
│                              │                               │
│                              │  ── Threads ──                │
│                              │  ● Thread 1 (stopped)         │
│                              │  ○ Thread 2 (running)         │
├──────────────────────────────┴───────────────────────────────┤
│ [Console] [Output]                                           │
│ > _                                                          │
│ [INFO] Process started with PID 12345                        │
│ [OUT] Hello, world!                                          │
│ [ERR] Warning: unused variable                               │
└──────────────────────────────────────────────────────────────┘
```

**Layout proportions:**
- Sidebar: ~30% width (right side)
- Bottom panel: ~25% height
- Code area: remaining space

**Sidebar sections** are rendered in a single scrollable panel with collapsible sections (Variables, Call Stack, Threads). They are always visible simultaneously, not tab-switched.

**Bottom panel** has two tabs:
- **Console**: Interactive expression evaluation + debug adapter messages
- **Output**: Program stdout/stderr output

### 4.3 Debug Mode Keymap

#### Debug-wide keys (work in ALL focus states):

| Key | Action | Description |
|---|---|---|
| `F5` | Continue/Pause | Resume if paused, pause if running |
| `F10` | Step Over | Step to next line |
| `F11` | Step In | Step into function call |
| `Shift-F11` | Step Out | Step out of current function |
| `Ctrl-d c` | Continue | Resume execution (prefix avoids conflicts) |
| `Ctrl-d n` | Next | Step over |
| `Ctrl-d s` | Step In | Step into |
| `Ctrl-d o` | Step Out | Step out |
| `Ctrl-d p` | Pause | Pause running program |
| `Ctrl-d b` | Toggle Breakpoint | Toggle breakpoint at cursor line |
| `Ctrl-d B` | Conditional Breakpoint | Set conditional breakpoint (opens prompt) |
| `Ctrl-d L` | Log Breakpoint | Set log point (opens prompt) |
| `Ctrl-d r` | Restart | Restart debug session |
| `Ctrl-d q` | Quit Debug | Terminate session, exit debug mode |
| `Ctrl-d h` | Hide/Show Layout | Toggle debug layout visibility |

#### Focus switching keys (debug-wide):

| Key | Action |
|---|---|
| `Ctrl-d Tab` | Cycle focus: Code → Sidebar → Console → Code |
| `Ctrl-d v` | Focus Variables section |
| `Ctrl-d k` | Focus Call Stack section |
| `Ctrl-d t` | Focus Threads section |
| `Ctrl-d d` | Focus Debug Console |
| `Esc` | Return focus to Code area (when in sidebar/console) |

#### Code area focus (Normal/Insert/Select modes work normally):
- **All** Normal mode keys work unchanged (h/j/k/l, w/b/e, gg, G, s, c, d, etc.)
- Search (/), goto (g), etc. all work
- Insert mode (i, a, o, etc.) is available for hot-patching
- Only `Ctrl-d` prefix and `F5/F10/F11` keys are intercepted for debug actions

#### Sidebar focus (single-letter shortcuts, no conflicts):

| Key | Action |
|---|---|
| `j` / `down` | Move to next item |
| `k` / `up` | Move to previous item |
| `Enter` / `l` | Expand/collapse tree node |
| `h` | Collapse current node |
| `y` | Copy value to clipboard |
| `c` | Continue |
| `n` | Step over |
| `s` | Step in |
| `o` | Step out |
| `Enter` on stack frame | Jump to that frame's source location |
| `Enter` on thread | Switch to that thread |

#### Console focus (input mode):

| Key | Action |
|---|---|
| Type | Input expression |
| `Enter` | Evaluate expression |
| `Up/Down` | Browse expression history |
| `Ctrl-c` | Cancel current input |
| `Esc` | Return focus to Code area |

### 4.4 Gutter Indicators

| Symbol | Meaning |
|---|---|
| `●` (red) | Verified breakpoint |
| `○` (red outline) | Unverified breakpoint (not yet confirmed by DAP) |
| `◆` (yellow) | Conditional breakpoint |
| `◇` (yellow outline) | Unverified conditional breakpoint |
| `◈` (blue) | Log point |
| `>` (green) | Current execution line |
| `●>` | Breakpoint + current execution line |

## 5. Variable Inspection & Watch

### 5.1 Variable Tree

The Variables section in the sidebar displays a lazy-loaded tree of variables:

```rust
pub struct VariableTree {
    pub scopes: Vec<Scope>,        // From DAP `scopes` response
    pub expanded: HashSet<u64>,    // Set of expanded variable references
    pub cache: HashMap<u64, Vec<Variable>>, // Cached children by variables_reference
}
```

**Data fetching flow:**
1. When execution stops → send `scopes` request for current frame
2. Each scope contains a `variables_reference` → send `variables` request to fetch top-level variables
3. Variables with `variables_reference > 0` are expandable (objects, arrays, etc.)
4. Expansion is lazy: `variables` request only sent when user expands a node
5. Cache is invalidated on each step/continue (new stop event)

**Display format:**
```
▼ Locals
   x: i32 = 42
   name: String = "hello world"
   ▶ config: Config { ... }       ← collapsed, expandable
   ▼ vec: Vec<i32> [3 items]      ← expanded
      [0]: 1
      [1]: 2
      [2]: 3
▼ Arguments
   argc: i32 = 1
▶ Globals                          ← collapsed scope
```

### 5.2 Watch Expressions

Users can add persistent watch expressions that are evaluated on every stop:

```rust
pub struct WatchExpression {
    pub expression: String,
    pub result: Option<String>,    // Last evaluation result
    pub error: Option<String>,     // Last evaluation error
}
```

**Commands:**
- `Ctrl-d w` → Add watch expression (opens prompt)
- In sidebar, navigate to Watch section → `x` to remove, `Enter` to edit

**Display in sidebar (below Variables, above Call Stack):**
```
▼ Watch
   count: 5
   items.len(): 3
   x + y: 43
   ✗ invalid_expr: "Could not evaluate"
```

Watch expressions are persisted in `.helix/debug.toml` under a `[watch]` section:
```toml
[watch]
expressions = ["count", "items.len()", "x + y"]
```

### 5.3 Thread & Frame Switching

When the user selects a different thread or stack frame:
1. Send `scopes` request for the selected frame
2. Update variable tree with new scope data
3. Jump editor to the source location of the selected frame
4. Update gutter to show current execution line at the new location
5. If source file is not open, open it in the code editor area

### 5.4 Hover Evaluation

When the cursor hovers over a variable name in the code editor (in Normal mode):
- After a short delay (300ms), send `evaluate` request with the word under cursor
- Display result as an inline popup near the cursor (similar to LSP hover)
- Popup dismissed on any cursor movement

### 5.5 Capability Negotiation & Graceful Degradation

During `initialize`, the client declares its capabilities. The server responds with `DebuggerCapabilities`. Feature availability depends on adapter capabilities:

| Feature | Required Capability | Fallback |
|---|---|---|
| Conditional breakpoints | `supportsConditionalBreakpoints` | Show error when user tries to set one |
| Log points | `supportsLogPoints` | Show error |
| Step back | `supportsStepBack` | Hide step-back key |
| Restart | `supportsRestartRequest` | Terminate + relaunch instead |
| Set variable | `supportsSetVariable` | Variable tree is read-only |
| Exception breakpoints | `exceptionBreakpointFilters` | Hide exception BP commands |
| Evaluate in hover | `supportsEvaluateForHovers` | Disable hover evaluation |

## 6. Configuration System

### 6.1 Language-level Defaults (languages.toml)

```toml
[[language]]
name = "rust"

[language.debugger]
name = "codelldb"
transport = "stdio"             # "stdio" | "tcp"
command = "codelldb"
args = ["--port", "{port}"]
port_arg = "--port"

[[language.debugger.templates]]
name = "binary"
request = "launch"
completion = [
  { name = "binary", completion = "filename", default = "./target/debug/{file_stem}" }
]
args = { program = "{0}", cwd = "." }

[[language.debugger.templates]]
name = "attach"
request = "attach"
completion = [
  { name = "pid", completion = "pid" }
]
args = { pid = "{0}" }
```

### 6.2 Project-level Configuration (.helix/debug.toml)

```toml
[[configurations]]
name = "Debug Main"
language = "rust"
request = "launch"
program = "./target/debug/myapp"
args = ["--verbose"]
cwd = "."
env = { RUST_LOG = "debug" }
pre_launch_task = "cargo build"    # optional: run before launching (see 6.5)

[[configurations]]
name = "Attach to Server"
language = "rust"
request = "attach"
# pid omitted = prompt at runtime

[[configurations]]
name = "Debug Tests"
language = "rust"
request = "launch"
program = "./target/debug/deps/myapp_tests"
args = ["--test-threads=1"]
env = { RUST_BACKTRACE = "1" }
```

### 6.3 Configuration Resolution Order

1. `.helix/debug.toml` configurations (project-specific, shown first in picker)
2. `languages.toml` debugger templates (language defaults, shown second)
3. Runtime parameter completion for unfilled values

### 6.4 Configuration Schema Validation

When loading `.helix/debug.toml`:
- Invalid TOML syntax → status bar error with line number, ignore entire file
- Missing required fields (`language`, `request`) → skip that configuration, warn in status bar
- Unknown keys → silently ignored (forward compatibility)
- Invalid values (e.g., `request = "invalid"`) → skip that configuration, warn in status bar

### 6.5 Pre-launch Task

When a configuration specifies `pre_launch_task`:
1. Run the command in the editor's built-in terminal (bottom panel)
2. **Block** debug launch until the task completes
3. Show task output in the bottom panel with a spinner in status bar
4. If exit code != 0 → show error, abort debug launch, keep terminal output visible
5. If exit code == 0 → proceed with debug launch
6. Timeout: 60 seconds (configurable via `pre_launch_task_timeout` in config)

This is a **Phase 4** feature. In earlier phases, `pre_launch_task` is silently ignored.

### 6.6 Configuration Picker UX

When user triggers `:debug`:

```
Debug Configuration
─────────────────────────
 Project Configurations
   Debug Main            (launch, rust)
   Attach to Server      (attach, rust)
   Debug Tests           (launch, rust)
 ─────────────────────────
 Language Defaults
   rust: binary           (launch)
   rust: attach            (attach)
```

## 7. Breakpoint Management

### 7.1 Breakpoint Types

```rust
pub struct Breakpoint {
    pub id: Option<u64>,          // DAP-assigned ID (after verification)
    pub line: usize,
    pub verified: bool,
    pub condition: Option<String>,
    pub hit_condition: Option<String>,
    pub log_message: Option<String>,
    pub message: Option<String>,   // DAP verification message
}
```

### 7.2 Breakpoint Persistence

Breakpoints are persisted to `.helix/breakpoints.toml`:

```toml
[[breakpoints]]
file = "src/main.rs"
line = 15

[[breakpoints]]
file = "src/lib.rs"
line = 42
condition = "x > 10"

[[breakpoints]]
file = "src/server.rs"
line = 88
log_message = "Request received: {request}"
```

**Path semantics:** All file paths in `breakpoints.toml` are relative to the workspace root (the directory containing `.helix/`). This ensures portability across machines and directory structures.

**Persistence rules:**
- Breakpoints are saved immediately when set/modified/removed
- On editor startup, breakpoints are loaded and shown in gutter (as unverified)
- When a debug session starts, all breakpoints are sent to DAP for verification
- Breakpoint positions are updated when DAP reports adjusted line numbers

**Real-time line tracking:**
- Breakpoints are anchored to document change tracking (same mechanism as diagnostics)
- When lines are inserted/deleted above a breakpoint, the breakpoint line number updates in real-time
- The persisted `.helix/breakpoints.toml` is updated on file save (not on every edit)
- If a breakpoint's line is deleted entirely, the breakpoint moves to the nearest valid line

### 7.3 Breakpoint Synchronization

When a debug session is active:
1. New breakpoints set by user → immediately sent to DAP via `setBreakpoints`
2. DAP responds with verification status → update gutter indicator
3. If DAP adjusts line number → update stored breakpoint position
4. On file save → re-send breakpoints for the saved file (lines may have shifted)

## 8. Typed Commands

| Command | Description |
|---|---|
| `:debug` | Open debug configuration picker, start session |
| `:debug-attach` | Open attach configuration picker |
| `:debug-eval <expr>` | Evaluate expression in current debug context |
| `:debug-restart` | Restart current debug session |
| `:debug-stop` | Stop current debug session |
| `:breakpoint [line]` | Toggle breakpoint at current/specified line |
| `:breakpoint-condition <expr>` | Set conditional breakpoint |
| `:breakpoint-log <msg>` | Set log point |
| `:breakpoint-clear` | Clear all breakpoints in current file |
| `:breakpoint-clear-all` | Clear all breakpoints in all files |

## 9. Entry Points (Outside Debug Mode)

Before entering debug mode, these keybindings are available in Normal mode under `space d`:

| Key | Action |
|---|---|
| `space d l` | Launch debug (open config picker) |
| `space d a` | Attach to process |
| `space d b` | Toggle breakpoint at cursor |
| `space d B` | Set conditional breakpoint |
| `space d c` | Clear all breakpoints |

These provide quick access to debug setup without needing to enter Debug mode first.

## 10. Status Bar Integration

### 10.1 Debug Status in Status Line

When a debug session is active, the status line shows:

```
[Debug: <state>] <file> [ln <line>, col <col>] [<thread>]
```

States:
- `Connecting...` with spinner animation
- `Running ▶` (green)
- `Paused ⏸ file.rs:15` (yellow)
- `Reconnecting (2/3)...` with spinner
- `Disconnected` (red)

### 10.2 Connection Status Notifications

Major state changes show temporary notifications in the editor:
- "Debug session started" (on successful connection)
- "Breakpoint hit at main.rs:15" (on stop)
- "Connection lost, reconnecting..." (on disconnect in attach mode)
- "Reconnection failed after 3 attempts" (on max retries)
- "Debug session ended" (on termination)

## 11. Out of Scope (v1)

The following features are explicitly deferred to future versions:
- **Multi-session debugging**: Only one active debug session at a time
- **Memory view / disassembly view**: Low-level inspection views
- **Inline values**: Showing variable values inline in the editor gutter
- **Remote debugging**: Connecting to remote DAP servers over network
- **Source mapping**: Custom source map handling (DAP adapter handles this)
- **Module / loaded sources browser**: Separate panels for loaded modules

## 12. Implementation Phases

### Phase 1: Foundation (Remove old + DAP types + Transport)
- Remove all existing debug code
- Rewrite `helix-dap-types` with complete DAP protocol coverage
- Rewrite `helix-dap` transport layer with connection state machine
- Implement heartbeat and reconnection logic

### Phase 2: Core Debug Experience
- Implement `ConnectionManager` with full state machine
- Add `Debug` mode to helix-view (mode enum, state types)
- Implement debug commands (launch, attach, step, continue, etc.)
- Add breakpoint management and persistence

### Phase 3: Debug UI
- Implement debug sidebar (variables tree, call stack, threads)
- Implement debug console (expression evaluation, output display)
- Implement debug layout manager (split screen)
- Add gutter indicators for breakpoints and execution line

### Phase 4: Configuration & Polish
- Implement `.helix/debug.toml` configuration loading
- Implement configuration picker
- Add `languages.toml` debugger template support
- Add status bar integration
- Add Normal mode entry points (`space d` prefix)

### Phase 5: Testing & Stabilization
- Integration tests with mock DAP server
- Connection stability testing (disconnect/reconnect scenarios)
- Multi-thread debugging testing
- Edge case handling (adapter crash, malformed responses)
