////////////////////////////////////////////////////////////
//  Log header file
//  Defines log types and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

use chrono;
use once_cell::sync::Lazy;
use parking_lot::Mutex as PLMutex;
use std::fs::File;
use std::io::Write;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

// Constants
pub const XOS_LOG_OK: u8 = 0x00;
pub const XOS_LOG_ERROR: u8 = 0x01;
pub const XOS_LOG_NOT_INIT: u8 = 0x02;
pub const XOS_LOG_MUTEX_ERROR: u8 = 0x03;

// Global state and atomic bool for thread safety
static S_B_INITIALIZED: AtomicBool = AtomicBool::new(false);
static S_B_MUTEX_INITIALIZED: AtomicBool = AtomicBool::new(false);

// The unique instance with lazy initialization
static INSTANCE: Lazy<Arc<PLMutex<LogConfig>>> =
    Lazy::new(|| Arc::new(PLMutex::new(LogConfig::new("explo.log"))));

///////////////////////////////////
/// @brief LogConfig
/// @param include_timestamp
/// @param console_output
/// @param file_output
/// @param log_path
/// @param file_log
/// @param mutex_name
///////////////////////////////////
pub struct LogConfig {
    pub include_timestamp: bool,
    pub console_output: bool,
    pub file_output: bool,
    pub log_path: String,
    pub file_log: Option<File>,
    pub mutex_name: String,
}

impl LogConfig {
    pub fn new(name: &str) -> Self {
        let mut log_path = String::from("log.txt");

        if name != "" {
            log_path = String::from(name);
        }

        Self {
            include_timestamp: true,
            console_output: true,
            file_output: true,
            log_path: log_path,
            file_log: None,
            mutex_name: String::from("xLog"),
        }
    }
}

// Public functions for the xLog module
pub fn initialize(config: LogConfig) -> u8 {
    // Check if already initialized
    if S_B_INITIALIZED.load(Ordering::SeqCst) 
    {
        return XOS_LOG_OK;
    }

    // Using Lazy instead of Once for consistent initialization
    // We will replace the existing instance if necessary
    if let Ok(file) = File::create(&config.log_path) {
        // Créer une copie modifiable de la config
        let mut config_with_file = config;
        config_with_file.file_log = Some(file);

        // Replace the existing instance with the new configuration
        let _ = INSTANCE; // Force initialization 
        if let Some(mut instance) = INSTANCE.try_lock() 
        {
            *instance = config_with_file;
        } else {
            return XOS_LOG_MUTEX_ERROR;
        }

        // Update the state indicators
        S_B_INITIALIZED.store(true, Ordering::SeqCst);
        S_B_MUTEX_INITIALIZED.store(true, Ordering::SeqCst);

        return XOS_LOG_OK;
    }

    XOS_LOG_ERROR
}

pub fn is_initialized() -> bool 
{
    S_B_INITIALIZED.load(Ordering::SeqCst);
    if S_B_INITIALIZED.load(Ordering::SeqCst) == true
    {
        return true;
    }
    else
    {
        return false;
    }
}

// Modified function to access the instance directly
#[track_caller]
pub fn write_log(message: &str) 
{
    if !is_initialized() 
    {
        return;
    }

    let location = std::panic::Location::caller();
    let caller_line = location.line();
    let caller_file = location.file();

    // extract the filename from the caller_file
    let filename = caller_file.split("/").last().unwrap();

    // Direct access to the instance without using get()
    if let Some(mut log) = INSTANCE.try_lock() 
    {
        // Console output
        if log.console_output 
        {
            if log.include_timestamp 
            {
                //common/src/hardware.rs:244 | 
                println!(
                    "{} | {}:{} | {}",
                    chrono::Local::now().format("%Y-%m-%d %H:%M:%S.%f"),
                    filename,
                    caller_line,
                    message
                );
            } 
            else 
            {
                println!("{}", message);
            }
        }

        // File writing thread-safe
        if log.file_output {
            if let Some(file_log) = &mut log.file_log {
                let _ = writeln!(
                    file_log,
                    "{} | {}:{} | {}",
                    chrono::Local::now().format("%Y-%m-%d %H:%M:%S.%f"),
                    filename,
                    caller_line,
                    message
                );
                let _ = file_log.flush();
            }
        }
    }
}

pub fn close() -> u8 {
    if !S_B_INITIALIZED.load(Ordering::SeqCst) {
        return XOS_LOG_NOT_INIT;
    }

    // Direct access to the instance
    if let Some(mut log) = INSTANCE.try_lock() {
        // Flush the log file
        if log.file_output {
            if let Some(file_log) = &mut log.file_log {
                let _ = file_log.flush();
            }
        }
    } else {
        return XOS_LOG_MUTEX_ERROR;
    }

    // Mark as not initialized
    S_B_INITIALIZED.store(false, Ordering::SeqCst);
    S_B_MUTEX_INITIALIZED.store(false, Ordering::SeqCst);

    XOS_LOG_OK
}
