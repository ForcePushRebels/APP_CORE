////////////////////////////////////////////////////////////
//  timer header file
//  defines high-performance timer types and functions
//  Uses kernel-based timers (timerfd) for optimal efficiency
//
// general discloser: copy or share the file is forbidden
// Written : 12/01/2025
// Modified: 12/05/2025 - Improved thread safety and timing precision
// Modified: 04/06/2025 - Fixed code execution security issues
// Modified: 20/06/2025 - Refactored for kernel-based high-performance timers
////////////////////////////////////////////////////////////
#pragma once

#ifndef XOS_TIMER_H_
#define XOS_TIMER_H_

#include "xOsMutex.h"
#include <stdint.h>
#include <time.h>
#include <sys/timerfd.h>
#include <stdatomic.h>

// Timer error codes
#define XOS_TIMER_OK 0x9A84B10
#define XOS_TIMER_ERROR 0x9A84B11
#define XOS_TIMER_INVALID 0x9A84B12
#define XOS_TIMER_TIMEOUT 0x9A84B13
#define XOS_TIMER_NOT_INIT 0x9A84B14
#define XOS_TIMER_FD_ERROR 0x9A84B15

// Timer modes
#define XOS_TIMER_MODE_ONESHOT 0
#define XOS_TIMER_MODE_PERIODIC 1

// Security limits
#define XOS_TIMER_MAX_PERIOD_MS (24 * 60 * 60 * 1000U) // 24 hours max
#define XOS_TIMER_MIN_PERIOD_MS 1                       // Minimum period in milliseconds
#define XOS_TIMER_MAX_CALLBACKS 1000                    // Maximum callbacks per call to prevent DoS

/**
 * High-performance timer context structure
 * Uses kernel-based timerfd for zero-polling efficiency
 */
typedef struct xos_timer_t
{
    int t_iTimerFd;                  // Kernel timer file descriptor (-1 if not initialized)
    uint32_t t_ulPeriod;             // Timer period in milliseconds
    uint8_t t_ucMode;                // Timer mode (one-shot or periodic)
    atomic_bool t_bActive;           // Timer active flag (atomic for thread safety)
    struct timespec t_tStart;        // Start time (for statistics and calculations)
    xOsMutexCtx t_tMutex;            // Mutex for thread-safety on control operations
    atomic_bool t_bStopRequested;    // Stop request flag for graceful shutdown
    uint64_t t_ullExpirationCount;   // Number of expirations (for statistics)
} xOsTimerCtx;

//////////////////////////////////
/// @brief Create and initialize a high-performance timer
/// @param p_ptTimer : timer structure pointer
/// @param p_ulPeriod : timer period in milliseconds
/// @param p_ucMode : timer mode (XOS_TIMER_MODE_ONESHOT or XOS_TIMER_MODE_PERIODIC)
/// @return success or error code
/// @note Uses kernel timerfd for zero-polling efficiency
//////////////////////////////////
int xTimerCreate(xOsTimerCtx *p_ptTimer, uint32_t p_ulPeriod, uint8_t p_ucMode);

//////////////////////////////////
/// @brief Start a timer (kernel-based, no polling)
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
/// @brief Destroy a timer and release kernel resources
/// @param p_ptTimer : timer structure pointer
/// @return success or error code
//////////////////////////////////
int xTimerDestroy(xOsTimerCtx *p_ptTimer);

//////////////////////////////////
/// @brief Wait for timer expiration (blocking, kernel-managed)
/// @param p_ptTimer : timer structure pointer
/// @param p_pullExpirations : pointer to store number of expirations (can be NULL)
/// @return XOS_TIMER_OK if expired, error code otherwise
/// @note This function blocks until timer expires or is stopped
//////////////////////////////////
int xTimerWaitExpiration(xOsTimerCtx *p_ptTimer, uint64_t *p_pullExpirations);

//////////////////////////////////
/// @brief Check if timer has expired (non-blocking)
/// @param p_ptTimer : timer structure pointer
/// @param p_pullExpirations : pointer to store number of expirations (can be NULL)
/// @return XOS_TIMER_OK if expired, XOS_TIMER_TIMEOUT if not expired
//////////////////////////////////
int xTimerCheckExpired(xOsTimerCtx *p_ptTimer, uint64_t *p_pullExpirations);

//////////////////////////////////
/// @brief Get current time in milliseconds (monotonic clock)
/// @return current time in milliseconds as uint64_t to prevent overflow
//////////////////////////////////
uint64_t xTimerGetCurrentMs(void);

//////////////////////////////////
/// @brief High-precision delay using clock_nanosleep (absolute time)
/// @param p_ulDelay : delay in milliseconds
/// @return success or error code
//////////////////////////////////
int xTimerDelay(uint32_t p_ulDelay);

//////////////////////////////////
/// @brief Process timer with callback (efficient blocking implementation)
/// @param p_ptTimer : timer structure pointer
/// @param p_pfCallback : function to call on each expiration
/// @param p_pvData : user data to pass to callback
/// @return Number of callbacks executed, or negative error code
/// @note This function blocks and handles expirations efficiently
//////////////////////////////////
int xTimerProcessCallback(xOsTimerCtx *p_ptTimer, void (*p_pfCallback)(void *), void *p_pvData);

//////////////////////////////////
/// @brief Get timer file descriptor for integration with epoll/select
/// @param p_ptTimer : timer structure pointer
/// @return timer file descriptor or -1 on error
/// @note Allows integration with event loops (epoll, select, poll)
//////////////////////////////////
int xTimerGetFd(xOsTimerCtx *p_ptTimer);

//////////////////////////////////
/// @brief Get timer statistics
/// @param p_ptTimer : timer structure pointer
/// @param p_pullTotalExpirations : pointer to store total expiration count
/// @param p_pullUptime : pointer to store uptime in milliseconds
/// @return success or error code
//////////////////////////////////
int xTimerGetStats(xOsTimerCtx *p_ptTimer, uint64_t *p_pullTotalExpirations, uint64_t *p_pullUptime);

#endif // XOS_TIMER_H_
