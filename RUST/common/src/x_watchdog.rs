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
    expiry_handler: Option<Arc<dyn Fn() + Send + Sync + 'static>>,
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
        self.expiry_handler = Some(Arc::new(handler));
    }

    pub fn start(&mut self) -> Result<(), &'static str> {
        if self.timer_handle.is_some() {
            return Ok(());
        }

        let timeout = Duration::from_millis(self.timeout_ms);
        let terminate = self.terminate.clone();
        let should_reset = self.should_reset.clone();
        let last_ping = self.last_ping.clone();
        let enable_reset = self.enable_reset;
        
        // Cloner le handler pour le thread
        let handler = self.expiry_handler.clone();

        let timer_handle = thread::spawn(move || {
            write_log(&format!("Watchdog timer started with timeout: {}ms", timeout.as_millis()));
            
            while !terminate.load(Ordering::Relaxed) {
                // Vérifier plus fréquemment (tous les 100ms ou timeout/10)
                let check_interval = std::cmp::min(Duration::from_millis(100), timeout / 10);
                thread::sleep(check_interval);
                
                let elapsed = {
                    let last = last_ping.lock().unwrap();
                    last.elapsed()
                };

                if elapsed > timeout {
                    write_log("Watchdog timeout detected!");
                    should_reset.store(true, Ordering::Relaxed);
                    
                    // Appeler le handler d'expiration
                    if let Some(ref h) = handler {
                        h();
                    }
                    
                    // Réarmer automatiquement le watchdog en réinitialisant le timestamp
                    {
                        let mut last = last_ping.lock().unwrap();
                        *last = std::time::Instant::now();
                    }
                    
                    // Remettre à zéro le flag d'expiration après réarmement
                    should_reset.store(false, Ordering::Relaxed);
                    
                    // Si enable_reset est false, arrêter le watchdog
                    if !enable_reset {
                        write_log("Watchdog disabled after timeout");
                        break;
                    }
                    
                    write_log("Watchdog automatically rearmed after timeout");
                    
                    // Attendre un peu avant de reprendre la surveillance pour éviter les timeouts immédiats
                    thread::sleep(Duration::from_millis(100));
                }
            }
            write_log("Watchdog timer terminated");
        });

        self.timer_handle = Some(timer_handle);
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

        // Réinitialiser les flags
        self.should_reset.store(false, Ordering::Relaxed);
        self.terminate.store(false, Ordering::Relaxed);

        Ok(())
    }

    pub fn refresh(&self) -> Result<(), &'static str> {
        if let Ok(mut last_ping) = self.last_ping.lock() {
            *last_ping = Instant::now();
            // Réinitialiser le flag d'expiration lors du refresh
            self.should_reset.store(false, Ordering::Relaxed);
            Ok(())
        } else {
            Err("Failed to acquire lock for refresh")
        }
    }

    pub fn has_expired(&self) -> bool {
        self.should_reset.load(Ordering::Relaxed)
    }

    pub fn reset_expiry_flag(&self) {
        self.should_reset.store(false, Ordering::Relaxed);
    }
}

/// Fonctions publiques pour le watchdog global
pub fn init_watchdog(mut watchdog: Watchdog) -> Result<(), &'static str> {
    watchdog.start()?;
    
    let watchdog_arc = Arc::new(Mutex::new(watchdog));
    
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

pub fn reset_watchdog_expiry() -> Result<(), &'static str> {
    let instance = WATCHDOG_INSTANCE.get()
        .ok_or("Watchdog not initialized")?;
    
    let watchdog = instance.lock()
        .map_err(|_| "Failed to acquire watchdog lock")?;
    
    watchdog.reset_expiry_flag();
    Ok(())
}

pub fn is_watchdog_expired() -> Result<bool, &'static str> {
    let instance = WATCHDOG_INSTANCE.get()
        .ok_or("Watchdog not initialized")?;
    
    let watchdog = instance.lock()
        .map_err(|_| "Failed to acquire watchdog lock")?;
    
    Ok(watchdog.has_expired())
}
