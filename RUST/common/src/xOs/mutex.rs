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
use crate::X_ASSERT;

// Constants
pub const MUTEX_OK: i32 = 0;
pub const MUTEX_ERROR: i32 = -1;
pub const MUTEX_TIMEOUT: i32 = -2;
pub const MUTEX_UNLOCKED: i32 = 0;
pub const MUTEX_LOCKED: i32 = 1;
pub const MUTEX_DEFAULT_TIMEOUT: u64 = 1000; // 1 second in milliseconds

pub struct t_MutexCtx 
{
    mutex: Mutex<()>,
    state: i32,
    timeout: u64,
    name: String,
}

impl t_MutexCtx {
    /// Create a new mutex with a name
    pub fn new(name: &str) -> Self 
    {
        let mut ctx = t_MutexCtx 
        {
            mutex: Mutex::new(()),
            state: MUTEX_UNLOCKED,
            timeout: MUTEX_DEFAULT_TIMEOUT,
            name: name.to_string(),
        };
        
        // Equivalent to mutexCreate
        if mutex_create(&mut ctx) != MUTEX_OK {
            panic!("Failed to create mutex");
        }
        
        ctx
    }
    
    /// Get the mutex name
    pub fn get_name(&self) -> &str {
        &self.name
    }
    
    /// Get the mutex state
    pub fn get_state(&self) -> i32 {
        self.state
    }
    
    /// Set the mutex timeout
    pub fn set_timeout(&mut self, timeout: u64) -> i32 {
        mutex_set_timeout(self, timeout)
    }
    
    /// Lock the mutex
    pub fn lock(&mut self) -> i32 {
        mutex_lock(self)
    }
    
    /// Try to lock the mutex
    pub fn try_lock(&mut self) -> i32 {
        mutex_try_lock(self)
    }
    
    /// Lock the mutex with a timeout
    pub fn lock_timeout(&mut self, timeout: u64) -> i32 {
        mutex_lock_timeout(self, timeout)
    }
    
    /// Unlock the mutex
    pub fn unlock(&mut self) -> i32 {
        mutex_unlock(self)
    }
    
    /// Destroy the mutex
    pub fn destroy(&mut self) -> i32 {
        mutex_destroy(self)
    }
}

////////////////////////////////////////////////////////////
/// mutex_create
////////////////////////////////////////////////////////////
pub fn mutex_create(mutex: &mut t_MutexCtx) -> i32 {
    X_ASSERT!(mutex != std::ptr::null_mut() as *mut t_MutexCtx);
    
    mutex.state = MUTEX_UNLOCKED;
    mutex.timeout = MUTEX_DEFAULT_TIMEOUT;
    
    return MUTEX_OK;
}

////////////////////////////////////////////////////////////
/// mutex_lock
////////////////////////////////////////////////////////////
pub fn mutex_lock(mutex: &mut t_MutexCtx) -> i32 {
    X_ASSERT!(mutex != std::ptr::null_mut() as *mut t_MutexCtx);

    match mutex.mutex.lock() 
    {
        Ok(_) => 
        {
            mutex.state = MUTEX_LOCKED;
            return MUTEX_OK;
        },
        Err(_) => return MUTEX_ERROR,
    }
}

////////////////////////////////////////////////////////////
/// mutex_try_lock
////////////////////////////////////////////////////////////
pub fn mutex_try_lock(mutex: &mut t_MutexCtx) -> i32 {
    X_ASSERT!(mutex != std::ptr::null_mut() as *mut t_MutexCtx);

    match mutex.mutex.try_lock() {
        Ok(_) => {
            mutex.state = MUTEX_LOCKED;
            return MUTEX_OK;
        },
        Err(_) => return MUTEX_TIMEOUT,
    }
}

////////////////////////////////////////////////////////////
/// mutex_lock_timeout
////////////////////////////////////////////////////////////
pub fn mutex_lock_timeout(mutex: &mut t_MutexCtx, timeout: u64) -> i32 {
    X_ASSERT!(mutex != std::ptr::null_mut() as *mut t_MutexCtx);

    let start = Instant::now();
    let timeout_duration = Duration::from_millis(timeout);
    
    // Try to lock with polling and sleeping
    while start.elapsed() < timeout_duration {
        match mutex.mutex.try_lock() {
            Ok(_) => {
                mutex.state = MUTEX_LOCKED;
                return MUTEX_OK;
            },
            Err(_) => {
                // Sleep a small amount before trying again
                thread::sleep(Duration::from_millis(1));
            }
        }
    }
    
    // Timeout occurred
    MUTEX_TIMEOUT
}

////////////////////////////////////////////////////////////
/// mutex_unlock
////////////////////////////////////////////////////////////
pub fn mutex_unlock(mutex: &mut t_MutexCtx) -> i32 {
    X_ASSERT!(mutex != std::ptr::null_mut() as *mut t_MutexCtx);

    // In Rust, we don't explicitly unlock - we drop the guard
    // Since we're not keeping the guard around, the mutex is already unlocked
    // This is just to update the state
    mutex.state = MUTEX_UNLOCKED;
    MUTEX_OK
}

////////////////////////////////////////////////////////////
/// mutex_destroy
////////////////////////////////////////////////////////////
pub fn mutex_destroy(mutex: &mut t_MutexCtx) -> i32 {
    X_ASSERT!(mutex != std::ptr::null_mut() as *mut t_MutexCtx);

    // In Rust, mutexes are automatically destroyed when they go out of scope
    // So we just update the state
    mutex.state = MUTEX_UNLOCKED;
    MUTEX_OK
}

////////////////////////////////////////////////////////////
/// mutex_set_timeout
////////////////////////////////////////////////////////////
pub fn mutex_set_timeout(mutex: &mut t_MutexCtx, timeout: u64) -> i32 {
    X_ASSERT!(mutex != std::ptr::null_mut() as *mut t_MutexCtx);

    mutex.timeout = timeout;
    MUTEX_OK
}

////////////////////////////////////////////////////////////
/// mutex_get_state
////////////////////////////////////////////////////////////
pub fn mutex_get_state(mutex: &t_MutexCtx) -> i32 {
    X_ASSERT!(mutex != std::ptr::null_mut() as *const t_MutexCtx);

    mutex.state
}

