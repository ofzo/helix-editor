use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::{
    BreakpointEventBody, CapabilitiesEventBody, ContinuedEventBody, ExitedEventBody,
    MemoryEventBody, ModuleEventBody, OutputEventBody, ProcessEventBody, StoppedEventBody,
    TerminatedEventBody, ThreadEventBody,
};

/// All DAP event types.
///
/// Each variant corresponds to a DAP event name and its associated body type.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "event", content = "body")]
pub enum EventBody {
    #[serde(rename = "initialized")]
    Initialized(Option<Value>),

    #[serde(rename = "stopped")]
    Stopped(StoppedEventBody),

    #[serde(rename = "continued")]
    Continued(ContinuedEventBody),

    #[serde(rename = "exited")]
    Exited(ExitedEventBody),

    #[serde(rename = "terminated")]
    Terminated(Option<TerminatedEventBody>),

    #[serde(rename = "thread")]
    Thread(ThreadEventBody),

    #[serde(rename = "output")]
    Output(OutputEventBody),

    #[serde(rename = "breakpoint")]
    Breakpoint(BreakpointEventBody),

    #[serde(rename = "module")]
    Module(ModuleEventBody),

    #[serde(rename = "process")]
    Process(ProcessEventBody),

    #[serde(rename = "capabilities")]
    Capabilities(CapabilitiesEventBody),

    #[serde(rename = "memory")]
    Memory(MemoryEventBody),

    #[serde(rename = "loadedSource")]
    LoadedSource(Value),

    #[serde(rename = "progressStart")]
    ProgressStart(Value),

    #[serde(rename = "progressUpdate")]
    ProgressUpdate(Value),

    #[serde(rename = "progressEnd")]
    ProgressEnd(Value),

    #[serde(rename = "invalidated")]
    Invalidated(Value),
}

/// DAP event names as string constants, useful for matching on raw event names.
pub mod event_names {
    pub const INITIALIZED: &str = "initialized";
    pub const STOPPED: &str = "stopped";
    pub const CONTINUED: &str = "continued";
    pub const EXITED: &str = "exited";
    pub const TERMINATED: &str = "terminated";
    pub const THREAD: &str = "thread";
    pub const OUTPUT: &str = "output";
    pub const BREAKPOINT: &str = "breakpoint";
    pub const MODULE: &str = "module";
    pub const PROCESS: &str = "process";
    pub const CAPABILITIES: &str = "capabilities";
    pub const MEMORY: &str = "memory";
    pub const LOADED_SOURCE: &str = "loadedSource";
    pub const PROGRESS_START: &str = "progressStart";
    pub const PROGRESS_UPDATE: &str = "progressUpdate";
    pub const PROGRESS_END: &str = "progressEnd";
    pub const INVALIDATED: &str = "invalidated";
}
