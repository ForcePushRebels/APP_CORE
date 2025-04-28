////////////////////////////////////////////////////////////
//  Threads header file
//  Defines threads types and functions
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
    Created,
    Running,
    Suspended,
    Terminated,
}

pub struct ThreadConfig 
{
    name: String,
    priority: u32,
    stack_size: usize,
    entry_point: Box<dyn FnOnce() + Send + 'static>,
}   
pub struct Thread 
{
    handle: Option<thread::JoinHandle<()>>,
    config: ThreadConfig,
    state: ThreadState,
}


impl ThreadConfig {
    pub fn new<F>(name: String, priority: u32, stack_size: usize, entry_point: F) -> Self
    where
        F: FnOnce() + Send + 'static,
    {
        xAssert(!name.is_empty());
        xAssert(priority > 0 && priority <= 10);
        xAssert(stack_size > 0);
        Self 
        {
            name,
            priority,
            stack_size,
            entry_point: Box::new(entry_point),
        }
    }
}

impl Thread
{
    pub fn new(config: ThreadConfig) -> Self 
    {
        xAssert(!config.name.is_empty());
        xAssert(config.priority > 0 && config.priority <= 10);
        xAssert(config.stack_size > 0);
        Self 
        {
            handle: None,
            config,
            state: ThreadState::Suspended,
        }
    }

    pub fn start(&mut self) -> Result<(), &'static str> 
    {
        let entry_point = std::mem::replace(&mut self.config.entry_point, Box::new(|| {}));
        let handle = thread::spawn(entry_point);
        self.handle = Some(handle);
        self.state = ThreadState::Running;
        Ok(())
    }

    pub fn join(self) -> Result<(), &'static str> 
    {
        if let Some(handle) = self.handle {
            handle.join().unwrap();
        }
        Ok(())
    }
  
}