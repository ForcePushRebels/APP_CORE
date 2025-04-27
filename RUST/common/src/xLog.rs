////////////////////////////////////////////////////////////
//  Log header file
//  Defines log types and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////
//  Log header file
//  Defines log types and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

use std::sync::{Arc, Mutex, Once};
use std::sync::OnceLock; // Changement: OnceLock au lieu de OnceCell pour thread safety
use std::sync::atomic::{AtomicBool, Ordering};
use std::fs::File;
use std::io::Write;
use crate::xOs::mutex::{t_MutexCtx, MUTEX_OK};
// use crate::xAssert::xAssert; // Supprimé: import inutilisé

// Constants
pub const XOS_LOG_OK: u8           = 0;
pub const XOS_LOG_ERROR: u8        = 1;
pub const XOS_LOG_NOT_INIT: u8     = 2;
pub const XOS_LOG_MUTEX_ERROR: u8  = 3;

// État global - AtomicBool pour thread safety
static S_B_INITIALIZED: AtomicBool = AtomicBool::new(false);
static S_B_MUTEX_INITIALIZED: AtomicBool = AtomicBool::new(false);

// L'instance unique - OnceLock pour thread safety
static INSTANCE: OnceLock<Arc<Mutex<LogConfig>>> = OnceLock::new();
static INIT: Once = Once::new();

// Structure LogConfig - sans dériver Clone car File n'est pas clonable
pub struct LogConfig {
    pub include_timestamp: bool,
    pub console_output:   bool,
    pub file_output:      bool,
    pub log_path:         String,
    pub file_log:         Option<File>,
    pub mutex:            t_MutexCtx,
}

// Macros d'accès au logger
#[macro_export]
macro_rules! X_LOG {
    ($message:expr) => {
        if $crate::xLog::is_initialized() {
            $crate::xLog::write_log(file!(), line!(), $message);
        }
    };
}

#[macro_export]
macro_rules! X_LOG_INFO {
    ($($arg:tt)*) => {
        if $crate::xLog::is_initialized() {
            $crate::xLog::write_log(file!(), line!(), &format!("INFO: {}", format!($($arg)*)));
        }
    };
}

#[macro_export]
macro_rules! X_LOG_ERROR {
    ($($arg:tt)*) => {
        if $crate::xLog::is_initialized() {
            $crate::xLog::write_log(file!(), line!(), &format!("ERROR: {}", format!($($arg)*)));
        }
    };
}

impl LogConfig {
    pub fn new() -> Self {
        Self {
            include_timestamp: true,
            console_output: true,
            file_output: true,
            log_path: String::from("log.txt"),
            file_log: None,
            mutex: t_MutexCtx::new("xLog"),
        }
    }
}

// Fonctions publiques pour le module xLog
pub fn initialize(config: LogConfig) -> u8 {
    // Vérifier si déjà initialisé
    if S_B_INITIALIZED.load(Ordering::SeqCst) {
        return XOS_LOG_OK;
    }
    
    // Initialiser une seule fois de manière thread-safe
    let result = XOS_LOG_OK;
    INIT.call_once(|| {
        if let Ok(file) = File::create(&config.log_path) {
            // Créer une copie modifiable de la config
            let mut config_with_file = config;
            config_with_file.file_log = Some(file);
            
            // Stockage thread-safe
            let _ = INSTANCE.set(Arc::new(Mutex::new(config_with_file)));
            S_B_INITIALIZED.store(true, Ordering::SeqCst);
            S_B_MUTEX_INITIALIZED.store(true, Ordering::SeqCst);
        }
    });
    
    if INSTANCE.get().is_some() {
        result
    } else {
        XOS_LOG_ERROR
    }
}

pub fn is_initialized() -> bool {
    S_B_INITIALIZED.load(Ordering::SeqCst)
}

pub fn get_instance() -> Option<&'static Arc<Mutex<LogConfig>>> {
    INSTANCE.get()
}

pub fn write_log(file: &str, line: u32, message: &str) {
    if let Some(log_arc) = get_instance() {
        // Protection thread-safe avec Mutex
        if let Ok(mut log) = log_arc.lock() {
            // Sortie console
            if log.console_output {
                if log.include_timestamp {
                    println!("{} | {}:{} | {}", 
                        chrono::Local::now().format("%Y-%m-%d %H:%M:%S"),
                        file, line, message);
                } else {
                    println!("{}:{} | {}", file, line, message);
                }
            }
            
            // Écriture fichier thread-safe
            if log.file_output {
                if let Some(file_log) = &mut log.file_log {
                    let _ = writeln!(file_log, "{} | {}:{} | {}", 
                        chrono::Local::now().format("%Y-%m-%d %H:%M:%S"),
                        file, line, message);
                    let _ = file_log.flush();
                }
            }
        }
    }
}

pub fn close() -> u8 {
    if !S_B_INITIALIZED.load(Ordering::SeqCst) {
        return XOS_LOG_NOT_INIT;
    }
    
    if let Some(log_arc) = get_instance() {
        if let Ok(mut log) = log_arc.lock() {
            // Flush du fichier de log
            if log.file_output {
                if let Some(file_log) = &mut log.file_log {
                    let _ = file_log.flush();
                }
            }
            
            // Libération du mutex
            let _ = log.mutex.destroy();
        } else {
            return XOS_LOG_MUTEX_ERROR;
        }
    }
    
    // Marquer comme non initialisé
    S_B_INITIALIZED.store(false, Ordering::SeqCst);
    S_B_MUTEX_INITIALIZED.store(false, Ordering::SeqCst);
    
    XOS_LOG_OK
}
