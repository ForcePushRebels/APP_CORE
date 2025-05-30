////////////////////////////////////////////////////////////
//  Watchdog header file 
//  Provides ultra-fast  watchdog support using POSIX timer
//
// general disclosure: copy or share the file is forbidden
// Written : 02/05/2025
// Modified: 28/05/2025 - Security improvements, optimization for maximum performance
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
#define WATCHDOG_DEFAULT_TIMEOUT        1000        // 1000 ms default timeout
#define WATCHDOG_MIN_TIMEOUT_MS         100         // Minimum timeout: 100ms
#define WATCHDOG_MAX_TIMEOUT_MS         3600000     // Maximum timeout: 1 hour
#define WATCHDOG_MAX_STOP_ITERATIONS    100         // Maximum iterations for stop operations
#define WATCHDOG_SLEEP_INTERVAL_MS      1000        // Sleep interval between checks
#define WATCHDOG_DEVICE_NAME            "watchdog"

//  Performance Constants
#define WATCHDOG_CACHE_REFRESH_CYCLES 20  // Refresh cached values every N cycles for optimal performance
#define WATCHDOG_FAST_CHECK_CYCLES 5      // Ultra-fast checks without memory barriers

////////////////////////////////////////////////////////////
/// @brief  Watchdog structure with only atomic variables for maximum performance
/// @param task_ctx : Task context for watchdog management
/// @param timer_id : POSIX timer (void* to avoid type dependencies)
/// @param a_ulTimeout : Atomic timeout in ms (thread-safe )
/// @param a_bShouldReset : Atomic reset flag (thread-safe )
/// @param a_iIsRunning : Atomic running state (thread-safe )
/// @param a_iTerminate : Atomic termination signal (thread-safe )
/// @param a_ulTimerArmed : Atomic timer armed timestamp for  ping
/// @param a_iExpired : Atomic expiration flag for  status
////////////////////////////////////////////////////////////
typedef struct watchdog
{
    xOsTaskCtx task_ctx;                    // Task context for watchdog management
    void *timer_id;                         // POSIX timer (void* to avoid type dependencies)
    atomic_ulong a_ulTimeout;               // Atomic timeout in ms ()
    atomic_bool a_bShouldReset;             // Atomic reset flag ()
    atomic_int a_iIsRunning;                // Atomic running state ()
    atomic_int a_iTerminate;                // Atomic termination signal ()
    atomic_ulong a_ulTimerArmed;            // Atomic timer armed timestamp ( ping tracking)
    atomic_int a_iExpired;                  // Atomic expiration flag ( status)
} watchdog_t;

//////////////////////////////////
/// @brief  Watchdog thread function - Ultra-fast Rule 4 compliance
/// @param arg : Thread argument pointer
/// @return Pointer for pthread compatibility
/// @note  thread monitors and pings watchdog at maximum speed
/// @pre Watchdog must be initialized before calling
//////////////////////////////////
void *watchdog_thread(void *arg);

//////////////////////////////////
/// @brief Initialize the  software watchdog with POSIX timer - Rule 4 compliance
/// @param timeout_ms : Expiration timeout in milliseconds (0 to use default)
/// @return WATCHDOG_SUCCESS on success, error code otherwise
/// @note Timeout is bounded by WATCHDOG_MAX_TIMEOUT_MS
/// @pre timeout_ms must be within valid bounds
//////////////////////////////////
int watchdog_init(int timeout_ms);

//////////////////////////////////
/// @brief Stop the  watchdog and release resources - Rule 4 compliance
/// @return none
/// @note Performs ultra-fast graceful shutdown with bounded wait loops
/// @note Cleans up all allocated resources securely
//////////////////////////////////
void watchdog_stop(void);

//////////////////////////////////
/// @brief  manually send a heartbeat signal to the watchdog - Rule 4 compliance
/// @details This signal resets the timer, preventing its expiration - MAXIMUM PERFORMANCE
/// @return WATCHDOG_SUCCESS on success, error code otherwise
/// @note  thread-safe operation using only atomic variables
//////////////////////////////////
int watchdog_ping(void);

//////////////////////////////////
/// @brief  check if the watchdog has expired - Rule 4 compliance
/// @return true if watchdog has expired, false otherwise
/// @note  thread-safe atomic read operation - ULTRA FAST
//////////////////////////////////
bool watchdog_has_expired(void);

//////////////////////////////////
/// @brief Set callback handler for watchdog expiration - Rule 4 compliance
/// @param callback : Function to call when watchdog expires (can be NULL)
/// @return none
/// @note  thread-safe operation using atomic compare-and-swap
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
