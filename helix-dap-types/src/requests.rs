use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::{
    DataBreakpoint, EvaluateContext, FunctionBreakpoint, InstructionBreakpoint, Source,
    SourceBreakpoint, StackFrameFormat, SteppingGranularity, ThreadId, ValueFormat,
};

/// DAP command names as string constants.
pub mod commands {
    pub const INITIALIZE: &str = "initialize";
    pub const LAUNCH: &str = "launch";
    pub const ATTACH: &str = "attach";
    pub const DISCONNECT: &str = "disconnect";
    pub const TERMINATE: &str = "terminate";
    pub const RESTART: &str = "restart";
    pub const CONFIGURATION_DONE: &str = "configurationDone";
    pub const SET_BREAKPOINTS: &str = "setBreakpoints";
    pub const SET_FUNCTION_BREAKPOINTS: &str = "setFunctionBreakpoints";
    pub const SET_EXCEPTION_BREAKPOINTS: &str = "setExceptionBreakpoints";
    pub const SET_DATA_BREAKPOINTS: &str = "setDataBreakpoints";
    pub const SET_INSTRUCTION_BREAKPOINTS: &str = "setInstructionBreakpoints";
    pub const CONTINUE: &str = "continue";
    pub const NEXT: &str = "next";
    pub const STEP_IN: &str = "stepIn";
    pub const STEP_OUT: &str = "stepOut";
    pub const STEP_BACK: &str = "stepBack";
    pub const PAUSE: &str = "pause";
    pub const RESTART_FRAME: &str = "restartFrame";
    pub const GOTO: &str = "goto";
    pub const REVERSE_CONTINUE: &str = "reverseContinue";
    pub const THREADS: &str = "threads";
    pub const STACK_TRACE: &str = "stackTrace";
    pub const SCOPES: &str = "scopes";
    pub const VARIABLES: &str = "variables";
    pub const SET_VARIABLE: &str = "setVariable";
    pub const SET_EXPRESSION: &str = "setExpression";
    pub const EVALUATE: &str = "evaluate";
    pub const COMPLETIONS: &str = "completions";
    pub const EXCEPTION_INFO: &str = "exceptionInfo";
    pub const SOURCE: &str = "source";
    pub const LOADED_SOURCES: &str = "loadedSources";
    pub const MODULES: &str = "modules";
    pub const GOTO_TARGETS: &str = "gotoTargets";
    pub const STEP_IN_TARGETS: &str = "stepInTargets";
    pub const DATA_BREAKPOINT_INFO: &str = "dataBreakpointInfo";
    pub const DISASSEMBLE: &str = "disassemble";
    pub const READ_MEMORY: &str = "readMemory";
    pub const WRITE_MEMORY: &str = "writeMemory";
    pub const CANCEL: &str = "cancel";
    pub const BREAKPOINT_LOCATIONS: &str = "breakpointLocations";
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct InitializeArguments {
    #[serde(rename = "clientID", default, skip_serializing_if = "Option::is_none")]
    pub client_id: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub client_name: Option<String>,
    #[serde(rename = "adapterID")]
    pub adapter_id: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub locale: Option<String>,
    #[serde(rename = "linesStartAt1", default, skip_serializing_if = "Option::is_none")]
    pub lines_start_at_1: Option<bool>,
    #[serde(rename = "columnsStartAt1", default, skip_serializing_if = "Option::is_none")]
    pub columns_start_at_1: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub path_format: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub supports_variable_type: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub supports_variable_paging: Option<bool>,
    #[serde(rename = "supportsRunInTerminalRequest", default, skip_serializing_if = "Option::is_none")]
    pub supports_run_in_terminal_request: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub supports_memory_references: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub supports_progress_reporting: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub supports_invalidated_event: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub supports_memory_event: Option<bool>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetBreakpointsArguments {
    pub source: Source,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub breakpoints: Option<Vec<SourceBreakpoint>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub source_modified: Option<bool>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetFunctionBreakpointsArguments {
    pub breakpoints: Vec<FunctionBreakpoint>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetExceptionBreakpointsArguments {
    pub filters: Vec<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub filter_options: Option<Vec<Value>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub exception_options: Option<Vec<Value>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetDataBreakpointsArguments {
    pub breakpoints: Vec<DataBreakpoint>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetInstructionBreakpointsArguments {
    pub breakpoints: Vec<InstructionBreakpoint>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ContinueArguments {
    pub thread_id: ThreadId,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub single_thread: Option<bool>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NextArguments {
    pub thread_id: ThreadId,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub single_thread: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub granularity: Option<SteppingGranularity>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StepInArguments {
    pub thread_id: ThreadId,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub single_thread: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub target_id: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub granularity: Option<SteppingGranularity>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StepOutArguments {
    pub thread_id: ThreadId,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub single_thread: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub granularity: Option<SteppingGranularity>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StepBackArguments {
    pub thread_id: ThreadId,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub single_thread: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub granularity: Option<SteppingGranularity>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PauseArguments {
    pub thread_id: ThreadId,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StackTraceArguments {
    pub thread_id: ThreadId,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub start_frame: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub levels: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub format: Option<StackFrameFormat>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ScopesArguments {
    pub frame_id: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VariablesArguments {
    pub variables_reference: usize,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub filter: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub start: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub count: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub format: Option<ValueFormat>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetVariableArguments {
    pub variables_reference: usize,
    pub name: String,
    pub value: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub format: Option<ValueFormat>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EvaluateArguments {
    pub expression: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub frame_id: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub context: Option<EvaluateContext>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub format: Option<ValueFormat>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompletionsArguments {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub frame_id: Option<usize>,
    pub text: String,
    pub column: usize,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub line: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExceptionInfoArguments {
    pub thread_id: ThreadId,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SourceArguments {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub source: Option<Source>,
    pub source_reference: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ModulesArguments {
    pub start_module: usize,
    pub module_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GotoTargetsArguments {
    pub source: Source,
    pub line: usize,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub column: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StepInTargetsArguments {
    pub frame_id: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GotoArguments {
    pub thread_id: ThreadId,
    pub target_id: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct RestartFrameArguments {
    pub frame_id: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DataBreakpointInfoArguments {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub variables_reference: Option<usize>,
    pub name: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub frame_id: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DisassembleArguments {
    pub memory_reference: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub offset: Option<isize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub instruction_offset: Option<isize>,
    pub instruction_count: usize,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub resolve_symbols: Option<bool>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ReadMemoryArguments {
    pub memory_reference: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub offset: Option<isize>,
    pub count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct WriteMemoryArguments {
    pub memory_reference: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub offset: Option<isize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub allow_partial: Option<bool>,
    pub data: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DisconnectArguments {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub restart: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub terminate_debuggee: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub suspend_debuggee: Option<bool>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TerminateArguments {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub restart: Option<bool>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CancelArguments {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub request_id: Option<u64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub progress_id: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BreakpointLocationsArguments {
    pub source: Source,
    pub line: usize,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub column: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub end_line: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub end_column: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetExpressionArguments {
    pub expression: String,
    pub value: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub frame_id: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub format: Option<ValueFormat>,
}

// Response body types

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetBreakpointsResponseBody {
    pub breakpoints: Vec<crate::Breakpoint>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ContinueResponseBody {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub all_threads_continued: Option<bool>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StackTraceResponseBody {
    pub stack_frames: Vec<crate::StackFrame>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub total_frames: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ThreadsResponseBody {
    pub threads: Vec<crate::Thread>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ScopesResponseBody {
    pub scopes: Vec<crate::Scope>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VariablesResponseBody {
    pub variables: Vec<crate::Variable>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EvaluateResponseBody {
    pub result: String,
    #[serde(default, rename = "type", skip_serializing_if = "Option::is_none")]
    pub ty: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub presentation_hint: Option<crate::VariablePresentationHint>,
    pub variables_reference: usize,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub named_variables: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub indexed_variables: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub memory_reference: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompletionsResponseBody {
    pub targets: Vec<crate::CompletionItem>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ExceptionInfoResponseBody {
    pub exception_id: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub break_mode: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub details: Option<crate::ExceptionDetails>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SourceResponseBody {
    pub content: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mime_type: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ModulesResponseBody {
    pub modules: Vec<crate::Module>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub total_modules: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GotoTargetsResponseBody {
    pub targets: Vec<crate::GotoTarget>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StepInTargetsResponseBody {
    pub targets: Vec<crate::StepInTarget>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SetVariableResponseBody {
    pub value: String,
    #[serde(default, rename = "type", skip_serializing_if = "Option::is_none")]
    pub ty: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub variables_reference: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub named_variables: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub indexed_variables: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LoadedSourcesResponseBody {
    pub sources: Vec<Source>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BreakpointLocationsResponseBody {
    pub breakpoints: Vec<BreakpointLocation>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BreakpointLocation {
    pub line: usize,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub column: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub end_line: Option<usize>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub end_column: Option<usize>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DataBreakpointInfoResponseBody {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub data_id: Option<String>,
    pub description: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub access_types: Option<Vec<crate::DataBreakpointAccessType>>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub can_persist: Option<bool>,
}
