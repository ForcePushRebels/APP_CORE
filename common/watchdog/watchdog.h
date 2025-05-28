////////////////////////////////////////////////////////////
//  Watchdog header file - NASA JPL 10 Rules Compliant LOCKLESS
//  Provides ultra-fast lockless watchdog support using POSIX timer
//
// general disclosure: copy or share the file is forbidden
// Written : 02/05/2025
// Modified: 28/05/2025 - NASA JPL compliance improvements
// Modified: XX/XX/2025 - LOCKLESS optimization for maximum performance
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////

#pragma once

#ifndef WATCHDOG_H
#define WATCHDOG_H


#include <stdbool.h>
#include <stdint.h>
#include <time.h>
#include <signal.h>
#include <sys/types.h>
#include <stdatomic.h>
#include "xTask.h"
#include "xOsMemory.h"

// Specific error codes for the watchdog
#define WATCHDOG_SUCCESS                  0x57D09A00
#define WATCHDOG_ERROR_INIT_FAILED        0x57D09A01
#define WATCHDOG_ERROR_ALREADY_INIT       0x57D09A02
#define WATCHDOG_ERROR_NOT_INIT           0x57D09A03
#define WATCHDOG_ERROR_TIMER_CREATE       0x57D09A05
#define WATCHDOG_ERROR_TIMER_SET          0x57D09A06
#define WATCHDOG_ERROR_THREAD_CREATE      0x57D09A07
#define WATCHDOG_ERROR_THREAD_FAILED      0x57D09A08
#define WATCHDOG_ERROR_INVALID_PARAM      0x57D09A09

// Fixed upper bounds for validation 
#define WATCHDOG_DEFAULT_TIMEOUT 100     // 100ms default timeout
#define WATCHDOG_MIN_TIMEOUT_MS 10       // Minimum timeout: 10ms
#define WATCHDOG_MAX_TIMEOUT_MS 3600000  // Maximum timeout: 1 hour
#define WATCHDOG_MAX_STOP_ITERATIONS 100      // Maximum iterations for stop operations
#define WATCHDOG_SLEEP_INTERVAL_MS 100         // Sleep interval between checks
#define WATCHDOG_DEVICE_NAME "watchdog"

// LOCKLESS Performance Constants
#define WATCHDOG_CACHE_REFRESH_CYCLES 20  // Refresh cached values every N cycles for optimal performance
#define WATCHDOG_FAST_CHECK_CYCLES 5      // Ultra-fast checks without memory barriers

////////////////////////////////////////////////////////////
/// @brief LOCKLESS Watchdog structure with only atomic variables for maximum performance
/// @param task_ctx : Task context for watchdog management
/// @param timer_id : POSIX timer (void* to avoid type dependencies)
/// @param a_ulTimeout : Atomic timeout in ms (thread-safe lockless)
/// @param a_bShouldReset : Atomic reset flag (thread-safe lockless)
/// @param a_iIsRunning : Atomic running state (thread-safe lockless)
/// @param a_iTerminate : Atomic termination signal (thread-safe lockless)
/// @param a_ulTimerArmed : Atomic timer armed timestamp for lockless ping
/// @param a_iExpired : Atomic expiration flag for lockless status
////////////////////////////////////////////////////////////
typedef struct watchdog
{
    xOsTaskCtx task_ctx;                    // Task context for watchdog management
    void *timer_id;                         // POSIX timer (void* to avoid type dependencies)
    atomic_ulong a_ulTimeout;               // Atomic timeout in ms (lockless)
    atomic_bool a_bShouldReset;             // Atomic reset flag (lockless)
    atomic_int a_iIsRunning;                // Atomic running state (lockless)
    atomic_int a_iTerminate;                // Atomic termination signal (lockless)
    atomic_ulong a_ulTimerArmed;            // Atomic timer armed timestamp (lockless ping tracking)
    atomic_int a_iExpired;                  // Atomic expiration flag (lockless status)
} watchdog_t;

//////////////////////////////////
/// @brief LOCKLESS Watchdog thread function - Ultra-fast Rule 4 compliance
/// @param arg : Thread argument pointer
/// @return Pointer for pthread compatibility
/// @note LOCKLESS thread monitors and pings watchdog at maximum speed
/// @pre Watchdog must be initialized before calling
//////////////////////////////////
void *watchdog_thread(void *arg);

//////////////////////////////////
/// @brief Initialize the LOCKLESS software watchdog with POSIX timer - Rule 4 compliance
/// @param timeout_ms : Expiration timeout in milliseconds (0 to use default)
/// @return WATCHDOG_SUCCESS on success, error code otherwise
/// @note Timeout is bounded by WATCHDOG_MAX_TIMEOUT_MS
/// @pre timeout_ms must be within valid bounds
//////////////////////////////////
int watchdog_init(int timeout_ms);

//////////////////////////////////
/// @brief Stop the LOCKLESS watchdog and release resources - Rule 4 compliance
/// @return none
/// @note Performs ultra-fast graceful shutdown with bounded wait loops
/// @note Cleans up all allocated resources securely
//////////////////////////////////
void watchdog_stop(void);

//////////////////////////////////
/// @brief LOCKLESS manually send a heartbeat signal to the watchdog - Rule 4 compliance
/// @details This signal resets the timer, preventing its expiration - MAXIMUM PERFORMANCE
/// @return WATCHDOG_SUCCESS on success, error code otherwise
/// @note LOCKLESS thread-safe operation using only atomic variables
//////////////////////////////////
int watchdog_ping(void);

//////////////////////////////////
/// @brief LOCKLESS check if the watchdog has expired - Rule 4 compliance
/// @return true if watchdog has expired, false otherwise
/// @note LOCKLESS thread-safe atomic read operation - ULTRA FAST
//////////////////////////////////
bool watchdog_has_expired(void);

//////////////////////////////////
/// @brief Set callback handler for watchdog expiration - Rule 4 compliance
/// @param callback : Function to call when watchdog expires (can be NULL)
/// @return none
/// @note LOCKLESS thread-safe operation using atomic compare-and-swap
//////////////////////////////////
void watchdog_set_expiry_handler(void (*callback)(void));

//////////////////////////////////
/// @brief Get a textual description of a watchdog error code - Rule 4 compliance
/// @param error_code : Watchdog error code
/// @return Pointer to a constant string describing the error
/// @note Returns const string, no memory management needed
//////////////////////////////////
const char* watchdog_get_error_string(int error_code);

#endif /* WATCHDOG_H */
