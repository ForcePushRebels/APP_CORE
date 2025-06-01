////////////////////////////////////////////////////////////
//  Watchdog header file
//  Defines watchdog configuration and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

use std::sync::{
    Arc, Mutex,
    atomic::{AtomicBool, Ordering},
};
use std::thread;
use std::time::{Duration, Instant};
use lazy_static::lazy_static;

use crate::xAssert::xAssert;
use crate::xLog::write_log;
use crate::xOs::xThreads::{Thread, ThreadConfig, ThreadState};


lazy_static! {
    pub static ref WATCHDOG_INSTANCE: Mutex<Option<Arc<Watchdog>>> = Mutex::new(None);
}

/// Configuration du watchdog
pub struct Watchdog {
    // Configuration de base
    pub timeout_ms: u64,
    pub enable_reset: bool,
    pub thread_config: ThreadConfig,
    timer_thread: Option<Thread>,
    pinger_thread: Option<Thread>,
    terminate: Arc<AtomicBool>,
    should_reset: Arc<AtomicBool>,
    last_ping: Arc<Mutex<Instant>>,
    expiry_handler: Mutex<Option<Box<dyn Fn() + Send + Sync + 'static>>>,
}

impl Watchdog {
    /// Crée une nouvelle instance de watchdog
    pub fn new(timeout_ms: u64, thread_config: ThreadConfig) -> Self {
        xAssert(timeout_ms > 0);

        Self {
            timeout_ms,
            enable_reset: true,
            thread_config,
            timer_thread: None,
            pinger_thread: None,
            terminate: Arc::new(AtomicBool::new(false)),
            should_reset: Arc::new(AtomicBool::new(false)),
            last_ping: Arc::new(Mutex::new(Instant::now())),
            expiry_handler: Arc::new(Mutex::new(None)),
        }
    }

    /// Définit une fonction à appeler quand le watchdog expire
    pub fn set_expiry_handler<F>(&mut self, handler: F)
    where
        F: Fn() + Send + Sync + 'static,
    {
        let mut h = self.expiry_handler.lock().unwrap();
        *h = Some(Box::new(handler));
    }

    /// Démarre la surveillance du watchdog
    pub fn start(&mut self) -> Result<(), &'static str> {
        if self.timer_thread.is_some() || self.pinger_thread.is_some() {
            return Err("Watchdog already started");
        }

        // Paramètres partagés
        let timeout = Duration::from_millis(self.timeout_ms);
        let terminate = self.terminate.clone();
        let should_reset = self.should_reset.clone();
        let expiry_handler = self.expiry_handler.clone();
        let last_ping = self.last_ping.clone();

        // Configuration du thread de surveillance
        let timer_config = ThreadConfig {
            name: format!("{}_timer", self.thread_config.name),
            priority: self.thread_config.priority,
            stack_size: self.thread_config.stack_size,
            entry_point: Box::new(move || {
                write_log(&format!("Timer thread started with timeout: {:?}", timeout));
                while !terminate.load(Ordering::SeqCst) {
                    thread::sleep(timeout / 5);
                    let elapsed = {
                        let last = last_ping.lock().unwrap();
                        last.elapsed()
                    };

                    if elapsed > timeout {
                        write_log("Watchdog timeout detected!");
                        should_reset.store(true, Ordering::SeqCst);
                        if let Some(handler) = &*expiry_handler.lock().unwrap() {
                            handler();
                        }
                    }
                }
                write_log("Timer thread terminated");
            }),
        };

        // Création et démarrage du thread de surveillance
        let mut timer_thread = Thread::new(timer_config);
        timer_thread.start()?;
        self.timer_thread = Some(timer_thread);

        // Création du thread de ping (uniquement pour les tests, en production il faut appeler refresh())
        if cfg!(test) {
            let terminate_ping = self.terminate.clone();
            let last_ping_ping = self.last_ping.clone();
            let timeout_ping = timeout;

            let pinger_config = ThreadConfig {
                name: format!("{}_pinger", self.thread_config.name),
                priority: self.thread_config.priority,
                stack_size: self.thread_config.stack_size,
                entry_point: Box::new(move || {
                    write_log("Pinger thread started");
                    while !terminate_ping.load(Ordering::SeqCst) {
                        {
                            let mut last = last_ping_ping.lock().unwrap();
                            *last = Instant::now();
                        }
                        thread::sleep(timeout_ping / 3);
                    }
                    write_log("Pinger thread terminated");
                }),
            };

            let mut pinger_thread = Thread::new(pinger_config);
            pinger_thread.start()?;
            self.pinger_thread = Some(pinger_thread);
        }

        write_log(&format!(
            "Watchdog initialized with timeout: {}ms",
            self.timeout_ms
        ));
        Ok(())
    }

    /// Arrête la surveillance du watchdog
    pub fn stop(&mut self) -> Result<(), &'static str> {
        self.terminate.store(true, Ordering::SeqCst);

        if let Some(timer_thread) = self.timer_thread.take() {
            // Attendre la fin du thread
            // En production, il faudrait implémenter un mécanisme de join
            write_log("Watchdog timer thread stopped");
        }

        if let Some(pinger_thread) = self.pinger_thread.take() {
            // Attendre la fin du thread
            write_log("Watchdog pinger thread stopped");
        }

        Ok(())
    }

    /// Vérifie si le watchdog a expiré
    pub fn has_expired(&self) -> bool 
    {
        self.should_reset.load(Ordering::SeqCst)
    }
}

////////////////////////////////////////////////////////////
/// @brief Init watchdog component and start automatically the thread
/// @param config: Watchdog
/// @return Result<(), &'static str>
////////////////////////////////////////////////////////////
pub fn init(mut watchdog: Watchdog) -> Result<(), &'static str>
{
    // Stockage du watchdog dans une instance globale
    unsafe {
        if WATCHDOG_INSTANCE.is_some() {
            return Err("Watchdog already initialized");
        }

        let watchdog_arc = Arc::new(Mutex::new(watchdog));

        // Démarrage du watchdog
        {
            let mut watchdog = watchdog_arc.lock().unwrap();
            watchdog.start()?;
        }

        WATCHDOG_INSTANCE = Some(watchdog_arc);
    }

    Ok(())
}

////////////////////////////////////////////////////////////
/// @brief Refresh watchdog to avoid timeout
/// @return Result<(), &'static str>
////////////////////////////////////////////////////////////
pub fn refresh() -> Result<(), &'static str> {
    unsafe {
        if let Some(watchdog_instance) = &WATCHDOG_INSTANCE {
            if let Ok(watchdog) = watchdog_instance.lock() {
                if let Ok(mut last_ping) = watchdog.last_ping.lock() {
                    *last_ping = Instant::now();
                    write_log("Watchdog refreshed");
                    return Ok(());
                }
            }
        }
    }

    Err("Watchdog not initialized or couldn't refresh")
}

////////////////////////////////////////////////////////////
/// @brief Disable watchdog
/// @return Result<(), &'static str>
////////////////////////////////////////////////////////////
pub fn disable() -> Result<(), &'static str> {
    unsafe {
        if let Some(watchdog_instance) = &WATCHDOG_INSTANCE {
            if let Ok(mut watchdog) = watchdog_instance.lock() {
                return watchdog.stop();
            }
        }
    }

    Err("Watchdog not initialized or couldn't disable")
}
