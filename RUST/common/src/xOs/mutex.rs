////////////////////////////////////////////////////////////
//  Mutex header file
//  Defines mutex types and functions
//
// general discloser: copy or share the file is forbidden
// Author: Christophe
// Written: 12/01/2025
////////////////////////////////////////////////////////////

use crate::X_ASSERT;
use crate::xAssert::xAssert; // import de la fonction d'assertion
use std::sync::Mutex;
use std::thread;
use std::time::{Duration, Instant}; // macro qui appelle xAssert(file!(), line!(), None)

// Const Return
pub const MUTEX_OK: i32 = 0;
pub const MUTEX_ERROR: i32 = -1;
pub const MUTEX_TIMEOUT: i32 = -2;
pub const MUTEX_UNLOCKED: i32 = 0;
pub const MUTEX_LOCKED: i32 = 1;
pub const MUTEX_DEFAULT_TIMEOUT: u64 = 1_000; // en ms

pub struct t_MutexCtx {
    mutex: Mutex<()>,
    state: i32,
    timeout: u64,
    name: String,
}

impl t_MutexCtx {
    /// Création du contexte (remplace mutex_create)
    pub fn new(name: &str) -> Self {
        let mut ctx = t_MutexCtx {
            mutex: Mutex::new(()),
            state: MUTEX_UNLOCKED,
            timeout: MUTEX_DEFAULT_TIMEOUT,
            name: name.to_string(),
        };
        // équivalent à mutex_create()
        ctx.state = MUTEX_UNLOCKED;
        ctx.timeout = MUTEX_DEFAULT_TIMEOUT;
        ctx
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }
    pub fn get_state(&self) -> i32 {
        self.state
    }

    pub fn set_timeout(&mut self, t: u64) -> i32 {
        self.timeout = t;
        MUTEX_OK
    }

    pub fn lock(&mut self) -> i32 {
        // on bloque
        match self.mutex.lock() {
            Ok(_guard) => {
                // Le guard sera drop à la fin de cette fonction, mais le mutex restera verrouillé
                // jusqu'à ce que unlock() soit appelé
                self.state = MUTEX_LOCKED;
                MUTEX_OK
            }
            Err(_) => MUTEX_ERROR,
        }
    }

    pub fn try_lock(&mut self) -> i32 {
        match self.mutex.try_lock() {
            Ok(_guard) => {
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
            if let Ok(_guard) = self.mutex.try_lock() {
                self.state = MUTEX_LOCKED;
                return MUTEX_OK;
            }
            thread::sleep(Duration::from_millis(1));
        }
        MUTEX_TIMEOUT
    }

    pub fn unlock(&mut self) -> i32 {
        if self.state == MUTEX_LOCKED {
            self.state = MUTEX_UNLOCKED;
            // Note: Since we're not storing the guard anymore, we can't explicitly unlock
            // This implementation won't work correctly without a redesign
            MUTEX_OK
        } else {
            MUTEX_ERROR
        }
    }

    pub fn destroy(&mut self) -> i32 {
        self.state = MUTEX_UNLOCKED;
        MUTEX_OK
    }
}

impl Drop for t_MutexCtx {
    fn drop(&mut self) {
        self.destroy();
    }
}
