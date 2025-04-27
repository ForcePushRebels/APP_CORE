////////////////////////////////////////////////////////////
//  Mutex header file
//  Defines mutex types and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

use std::sync::{Mutex, MutexGuard};
use std::time::{Duration, Instant};
use std::thread;
use crate::xAssert::xAssert;    // import de la fonction d’assertion
use crate::X_ASSERT;            // macro qui appelle xAssert(file!(), line!(), None)

// Const Return
pub const MUTEX_OK: i32        =  0;
pub const MUTEX_ERROR: i32     = -1;
pub const MUTEX_TIMEOUT: i32   = -2;
pub const MUTEX_UNLOCKED: i32  =  0;
pub const MUTEX_LOCKED: i32    =  1;
pub const MUTEX_DEFAULT_TIMEOUT: u64 = 1_000; // en ms

pub struct t_MutexCtx {
    mutex:  Mutex<()>,
    guard:  Option<MutexGuard<'static, ()>>,
    state:  i32,
    timeout: u64,
    name:   String,
}

impl t_MutexCtx {
    /// Création du contexte (remplace mutex_create)
    pub fn new(name: &str) -> Self {
        let mut ctx = t_MutexCtx {
            mutex:   Mutex::new(()),
            guard:   None,
            state:   MUTEX_UNLOCKED,
            timeout: MUTEX_DEFAULT_TIMEOUT,
            name:    name.to_string(),
        };
        // équivalent à mutex_create()
        ctx.state   = MUTEX_UNLOCKED;
        ctx.timeout = MUTEX_DEFAULT_TIMEOUT;
        ctx
    }

    pub fn get_name(&self)  -> &str { &self.name }
    pub fn get_state(&self) -> i32  { self.state }

    pub fn set_timeout(&mut self, t: u64) -> i32 {
        self.timeout = t;
        MUTEX_OK
    }

    pub fn lock(&mut self) -> i32 {
        // on bloque et on conserve le guard dans self.guard
        match self.mutex.lock() {
            Ok(g) => {
                // on étend la durée de vie du guard en 'static via transmute
                let g_static = unsafe {
                    std::mem::transmute::<MutexGuard<'_, ()>, MutexGuard<'static, ()>>(g)
                };
                self.guard = Some(g_static);
                self.state = MUTEX_LOCKED;
                MUTEX_OK
            }
            Err(_) => MUTEX_ERROR,
        }
    }

    pub fn try_lock(&mut self) -> i32 {
        match self.mutex.try_lock() {
            Ok(g) => {
                let g_static = unsafe {
                    std::mem::transmute::<MutexGuard<'_, ()>, MutexGuard<'static, ()>>(g)
                };
                self.guard = Some(g_static);
                self.state = MUTEX_LOCKED;
                MUTEX_OK
            }
            Err(_) => MUTEX_TIMEOUT,
        }
    }

    pub fn lock_timeout(&mut self, timeout: u64) -> i32 {
        let start = Instant::now();
        let to = Duration::from_millis(timeout);
        while start.elapsed() < to {
            if let Ok(g) = self.mutex.try_lock() {
                let g_static = unsafe {
                    std::mem::transmute::<MutexGuard<'_, ()>, MutexGuard<'static, ()>>(g)
                };
                self.guard = Some(g_static);
                self.state = MUTEX_LOCKED;
                return MUTEX_OK;
            }
            thread::sleep(Duration::from_millis(1));
        }
        MUTEX_TIMEOUT
    }

    pub fn unlock(&mut self) -> i32 {
        if self.guard.is_some() {
            // en droppant le guard, on libère réellement le mutex
            self.guard.take();
            self.state = MUTEX_UNLOCKED;
            MUTEX_OK
        } else {
            MUTEX_ERROR
        }
    }

    pub fn destroy(&mut self) -> i32 {
        // on s’assure d’avoir libéré le lock
        self.guard.take();
        self.state = MUTEX_UNLOCKED;
        MUTEX_OK
    }
}

impl Drop for t_MutexCtx {
    fn drop(&mut self) {
        self.destroy();
    }
}
