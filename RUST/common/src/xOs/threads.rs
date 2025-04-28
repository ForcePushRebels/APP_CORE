////////////////////////////////////////////////////////////
//  Mutex header file
//  Defines mutex types and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

use std::thread;
use std::time::Duration;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::AtomicU32;

use crate::xAssert::xAssert;
use crate::xLog::write_log;


pub enum ThreadState 
{
    Running,
    Suspended,
    Terminated,
}

pub struct ThreadConfig 
{
    name: String,
    priority: u32,
    stack_size: usize,
    entry_point: fn(),
    parameters: fn(),
}   
pub struct Thread 
{
    id: thread::ThreadId,
    config: ThreadConfig,
    state: ThreadState,
}


impl ThreadConfig
{
    pub fn new(name: String, priority: u32, stack_size: usize, entry_point: fn(), parameters: fn()) -> Self 
    {
        xAssert(name.is_empty());
        xAssert(priority > 0 && priority <= 10);
        xAssert(stack_size > 0);
        Self { name, priority, stack_size, entry_point, parameters }
    }
}

impl Thread
{
    pub fn new(config: ThreadConfig) -> Self 
    {
        Self {, config, state: ThreadState::Running }
    }
}