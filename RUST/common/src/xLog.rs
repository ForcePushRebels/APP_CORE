////////////////////////////////////////////////////////////
//  Log header file
//  Defines log types and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

use crate::xAssert::*;
use crate::xOs::mutex::*;
use std::fs::File;
use std::io::{self, Read};


// static variables
static S_B_INITIALIZED: bool = false;
static S_B_MUTEX_INITIALIZED: bool = false;


// Macros
#[macro_export]
macro_rules! X_LOG_ASSERT 
{
    (p_sFile : String, p_ulLine : u32, $message:expr) => 
    {
        xLogWrite(p_sFile, p_ulLine, $message)
    };
}

#[macro_export]
macro_rules! X_LOG_TRACE {
    (p_sFile : String, p_ulLine : u32, $message:expr) => {
        xLogWrite(p_sFile, p_ulLine, $message)
    };
}

/// Config structure
pub struct LogConfig 
{
    pub include_timestamp: bool,
    pub console_output: bool,
    pub file_output: bool,
    pub log_path: String,
    pub file_log: Option<File>,
    pub mutex: t_MutexCtx
}

/// Initialize the logging system
pub fn xLogInit(p_tConfig: &mut LogConfig) -> u8 
{
    if S_B_INITIALIZED
    {
        return 0;
    }

    p_tConfig.mutex = t_MutexCtx::new("xLog");

    if (p_tConfig.mutex.get_state() != MUTEX_OK)
    {
        return 0;
    }

    if (p_tConfig.file_output)
    {
        p_tConfig.file_log = Some(File::create(p_tConfig.log_path.clone()).unwrap());
        
        if (p_tConfig.file_log.is_none())
        {
            return 1;
        }
    }
    
    
    
    return 1;
    
}



