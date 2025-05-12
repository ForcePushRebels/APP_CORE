////////////////////////////////////////////////////////////
//  Watchdog header file
//  Provides watchdog support using POSIX timer
//
// general disclosure: copy or share the file is forbidden
// Written : 02/05/2025
////////////////////////////////////////////////////////////

#ifndef WATCHDOG_H
#define WATCHDOG_H

#include <stdbool.h>
#include <stdint.h>
#include <time.h>
#include <signal.h>
#include <sys/types.h>
#include "xTask.h"
#include "xOsMutex.h"

// Specific error codes for the watchdog (less than 0x7FFFFFFF)
#define WATCHDOG_SUCCESS                  0x57D09A00
#define WATCHDOG_ERROR_INIT_FAILED        0x57D09A01
#define WATCHDOG_ERROR_ALREADY_INIT       0x57D09A02
#define WATCHDOG_ERROR_NOT_INIT           0x57D09A03
#define WATCHDOG_ERROR_MUTEX_FAILED       0x57D09A04
#define WATCHDOG_ERROR_TIMER_CREATE       0x57D09A05
#define WATCHDOG_ERROR_TIMER_SET          0x57D09A06
#define WATCHDOG_ERROR_THREAD_CREATE      0x57D09A07
#define WATCHDOG_ERROR_THREAD_FAILED      0x57D09A08
#define WATCHDOG_ERROR_INVALID_PARAM      0x57D09A09

// Definitions for the timer
#define WATCHDOG_DEFAULT_TIMEOUT 100 // 100ms
#define WATCHDOG_DEVICE_NAME "watchdog"

////////////////////////////////////////////////////////////
/// @param watchdog_t
/// @brief Watchdog structure
////////////////////////////////////////////////////////////
typedef struct watchdog
{
    xOsTaskCtx task_ctx;              // Task context for watchdog management
    void *timer_id;                  // POSIX timer (void* to avoid type dependencies)
    uint32_t timeout;                // Expiration delay in ms
    xOsMutexCtx mutex;               // Mutex to protect concurrent access
    bool should_reset;               // Indicates if a system reset is needed
    int is_running;                  // Indicates if the watchdog thread is running
    volatile sig_atomic_t terminate; // Signal to stop the thread cleanly
} watchdog_t;

//////////////////////////////////
/// @brief Watchdog thread function
/// @param arg : Thread argument pointer
/// @return Pointer for pthread compatibility
//////////////////////////////////
void *watchdog_thread(void *arg);

//////////////////////////////////
/// @brief Initialize the software watchdog with POSIX timer
/// @param timeout_ms : Expiration timeout in milliseconds (0 to use default)
/// @return 0 on success, -1 on error
//////////////////////////////////
int watchdog_init(int timeout_ms);

//////////////////////////////////
/// @brief Stop the watchdog and release resources
/// @return none
//////////////////////////////////
void watchdog_stop(void);

//////////////////////////////////
/// @brief Manually send a heartbeat signal to the watchdog
/// @details This signal resets the timer, preventing its expiration
/// @return 0 on success, -1 on error
//////////////////////////////////
int watchdog_ping(void);

//////////////////////////////////
/// @brief Check if the watchdog has expired
/// @return true if watchdog has expired, false otherwise
//////////////////////////////////
bool watchdog_has_expired(void);

//////////////////////////////////
/// @brief Set callback handler for watchdog expiration
/// @param callback : Function to call when watchdog expires
/// @return none
//////////////////////////////////
void watchdog_set_expiry_handler(void (*callback)(void));

//////////////////////////////////
/// @brief Get a textual description of a watchdog error code
/// @param error_code : Watchdog error code
/// @return Pointer to a string describing the error
//////////////////////////////////
const char* watchdog_get_error_string(int error_code);

#endif /* WATCHDOG_H */
