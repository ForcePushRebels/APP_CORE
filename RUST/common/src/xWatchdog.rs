////////////////////////////////////////////////////////////
//  Watchdog header file
//  Defines watchdog configuration and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////


use std::sync::atomic::AtomicBool;
use crate::xOs::threads::Thread;

/// Configuration du watchdog
pub struct WatchdogConfig 
{
    pub timeout_ms: u32,
    pub enable_reset: bool,
    pub threads: Thread
}

/// Initialise le watchdog avec la configuration spécifiée
pub fn init(config: WatchdogConfig) -> Result<(), &'static str> {
    unimplemented!()
}

/// Rafraîchit le watchdog pour éviter le timeout
pub fn refresh() -> Result<(), &'static str> {
    unimplemented!()
}

/// Désactive le watchdog
pub fn disable() -> Result<(), &'static str> {
    unimplemented!()
} 