////////////////////////////////////////////////////////////
//  timer header file
//  defines the timer types and functions
//
// general discloser: copy or share the file is forbidden
// Written : 12/01/2025
// Modified: 12/05/2025 - Improved thread safety and timing precision
// Modified: 12/06/2025 - Fixed code execution security issues
////////////////////////////////////////////////////////////
#pragma once

#ifndef XOS_TIMER_H_
#define XOS_TIMER_H_

#include <stdint.h>
#include <time.h>
#include "xOsMutex.h"

// Timer error codes
#define XOS_TIMER_OK 0x9A84B10
#define XOS_TIMER_ERROR 0x9A84B11
#define XOS_TIMER_INVALID 0x9A84B12
#define XOS_TIMER_TIMEOUT 0x9A84B13
#define XOS_TIMER_NOT_INIT 0x9A84B14
#define XOS_TIMER_MUTEX_ERROR 0x9A84B15

// Timer modes
#define XOS_TIMER_MODE_ONESHOT 0
#define XOS_TIMER_MODE_PERIODIC 1

// Security limits
#define XOS_TIMER_MAX_PERIOD_MS UINT32_MAX  // Maximum period in milliseconds (uint32_t max value)
#define XOS_TIMER_MIN_PERIOD_MS 1           // Minimum period in milliseconds
#define XOS_TIMER_MAX_CALLBACKS 1000        // Maximum callbacks per call to prevent DoS

/**
 * Timer context structure
 * Thread-safe implementation with access protection
 */
typedef struct xos_timer_t
{
    uint32_t t_ulPeriod;      // Timer period in milliseconds
    uint8_t t_ucMode;         // Timer mode (one-shot or periodic)
    uint8_t t_ucActive;       // Timer active flag
    struct timespec t_tStart; // Start time
    struct timespec t_tNext;  // Next trigger time
    xOsMutexCtx t_tMutex;     // Mutex for thread-safety
} xOsTimerCtx;

//////////////////////////////////
/// @brief Create and initialize a timer
/// @param p_ptTimer : timer structure pointer
/// @param p_ulPeriod : timer period in milliseconds
/// @param p_ucMode : timer mode (XOS_TIMER_MODE_ONESHOT or XOS_TIMER_MODE_PERIODIC)
/// @return success or error code
//////////////////////////////////
int xTimerCreate(xOsTimerCtx *p_ptTimer, uint32_t p_ulPeriod, uint8_t p_ucMode);

//////////////////////////////////
/// @brief Start a timer
/// @param p_ptTimer : timer structure pointer
/// @return success or error code
//////////////////////////////////
int xTimerStart(xOsTimerCtx *p_ptTimer);

//////////////////////////////////
/// @brief Stop a timer
/// @param p_ptTimer : timer structure pointer
/// @return success or error code
//////////////////////////////////
int xTimerStop(xOsTimerCtx *p_ptTimer);

//////////////////////////////////
/// @brief Check if timer has expired
/// @param p_ptTimer : timer structure pointer
/// @return XOS_TIMER_OK if expired, XOS_TIMER_TIMEOUT if not expired
//////////////////////////////////
int xTimerExpired(xOsTimerCtx *p_ptTimer);

//////////////////////////////////
/// @brief Get current time in milliseconds (monotonic clock)
/// @return current time in milliseconds as uint64_t to prevent overflow
//////////////////////////////////
uint64_t xTimerGetCurrentMs(void);

//////////////////////////////////
/// @brief High-precision delay using nanosleep
/// @param p_ulDelay : delay in milliseconds
/// @return success or error code
//////////////////////////////////
int xTimerDelay(uint32_t p_ulDelay);

//////////////////////////////////
/// @brief Check if timer has expired and execute callback for each missed period
/// @param p_ptTimer : timer structure pointer
/// @param p_pfCallback : function to call for each elapsed period
/// @param p_pvData : user data to pass to callback
/// @return Number of periods elapsed, or negative error code
//////////////////////////////////
int xTimerProcessElapsedPeriods(xOsTimerCtx *p_ptTimer, void (*p_pfCallback)(void *), void *p_pvData);

#endif // XOS_TIMER_H_
