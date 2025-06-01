////////////////////////////////////////////////////////////
//  Watchdog header file
//  Defines watchdog configuration and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

use std::sync::{Arc, Mutex, OnceLock};
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};
use crate::x_log::write_log;

// Instance globale du watchdog
static WATCHDOG_INSTANCE: OnceLock<Arc<Mutex<Watchdog>>> = OnceLock::new();

/// Configuration du watchdog
pub struct Watchdog {
    pub timeout_ms: u64,
    pub enable_reset: bool,
    timer_handle: Option<JoinHandle<()>>,
    terminate: Arc<AtomicBool>,
    should_reset: Arc<AtomicBool>,
    last_ping: Arc<Mutex<Instant>>,
    expiry_handler: Option<Box<dyn Fn() + Send + Sync + 'static>>,
}

impl Watchdog {
    pub fn new(timeout_ms: u64) -> Self {
        if timeout_ms == 0 {
            panic!("Timeout must be greater than 0");
        }

        Self {
            timeout_ms,
            enable_reset: true,
            timer_handle: None,
            terminate: Arc::new(AtomicBool::new(false)),
            should_reset: Arc::new(AtomicBool::new(false)),
            last_ping: Arc::new(Mutex::new(Instant::now())),
            expiry_handler: None,
        }
    }

    pub fn set_expiry_handler<F>(&mut self, handler: F)
    where
        F: Fn() + Send + Sync + 'static,
    {
        self.expiry_handler = Some(Box::new(handler));
    }

    pub fn start(&mut self) -> Result<(), &'static str> {
        if self.timer_handle.is_some() {
            return Err("Watchdog already started");
        }

        let timeout = Duration::from_millis(self.timeout_ms);
        let terminate = self.terminate.clone();
        let should_reset = self.should_reset.clone();
        let last_ping = self.last_ping.clone();
        
        // Déplacer le handler hors de self pour éviter les problèmes de borrow
        let handler = self.expiry_handler.take();

        let timer_handle = thread::spawn(move || {
            write_log(&format!("Watchdog timer started with timeout: {}ms", timeout.as_millis()));
            
            while !terminate.load(Ordering::Relaxed) {
                thread::sleep(timeout / 5);
                
                let elapsed = {
                    let last = last_ping.lock().unwrap();
                    last.elapsed()
                };

                if elapsed > timeout {
                    write_log("Watchdog timeout detected!");
                    should_reset.store(true, Ordering::Relaxed);
                    
                    if let Some(ref h) = handler {
                        h();
                    }
                    break;
                }
            }
            write_log("Watchdog timer terminated");
        });

        self.timer_handle = Some(timer_handle);
        write_log(&format!("Watchdog initialized with timeout: {}ms", self.timeout_ms));
        Ok(())
    }

    pub fn stop(&mut self) -> Result<(), &'static str> {
        self.terminate.store(true, Ordering::Relaxed);

        if let Some(handle) = self.timer_handle.take() {
            if handle.join().is_err() {
                return Err("Failed to join timer thread");
            }
            write_log("Watchdog timer thread stopped");
        }

        Ok(())
    }

    pub fn refresh(&self) -> Result<(), &'static str> {
        if let Ok(mut last_ping) = self.last_ping.lock() {
            *last_ping = Instant::now();
            write_log("Watchdog refreshed");
            Ok(())
        } else {
            Err("Failed to acquire lock for refresh")
        }
    }

    pub fn has_expired(&self) -> bool {
        self.should_reset.load(Ordering::Relaxed)
    }
}

/// Fonctions publiques pour le watchdog global
pub fn init_watchdog(watchdog: Watchdog) -> Result<(), &'static str> {
    let mut wd = watchdog;
    wd.start()?;
    
    let watchdog_arc = Arc::new(Mutex::new(wd));
    
    WATCHDOG_INSTANCE.set(watchdog_arc)
        .map_err(|_| "Watchdog already initialized")?;
    
    Ok(())
}

pub fn refresh_watchdog() -> Result<(), &'static str> {
    let instance = WATCHDOG_INSTANCE.get()
        .ok_or("Watchdog not initialized")?;
    
    let watchdog = instance.lock()
        .map_err(|_| "Failed to acquire watchdog lock")?;
    
    watchdog.refresh()
}

pub fn stop_watchdog() -> Result<(), &'static str> {
    let instance = WATCHDOG_INSTANCE.get()
        .ok_or("Watchdog not initialized")?;
    
    let mut watchdog = instance.lock()
        .map_err(|_| "Failed to acquire watchdog lock")?;
    
    watchdog.stop()
}
