////////////////////////////////////////////////////////////
//  High-Performance Timer Implementation
//  Uses kernel-based timerfd for zero-polling efficiency
//  Follows principles: monotonic clock, kernel wakeup, grouped expirations
//
// general discloser: copy or share the file is forbidden
// Written : 12/01/2025
// Modified: 20/06/2025 - Refactored for kernel-based high-performance timers
////////////////////////////////////////////////////////////

#include "xTimer.h"
#include "xAssert.h"
#include "xLog.h"
#include "xOsMemory.h"
#include <errno.h>
#include <time.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/timerfd.h>

////////////////////////////////////////////////////////////
/// xTimerCreate
////////////////////////////////////////////////////////////
int xTimerCreate(xOsTimerCtx *p_ptTimer, uint32_t p_ulPeriod, uint8_t p_ucMode)
{
    X_ASSERT(p_ptTimer != NULL);
    X_ASSERT(p_ulPeriod >= XOS_TIMER_MIN_PERIOD_MS && p_ulPeriod <= XOS_TIMER_MAX_PERIOD_MS);
    X_ASSERT(p_ucMode == XOS_TIMER_MODE_PERIODIC || p_ucMode == XOS_TIMER_MODE_ONESHOT);

    // Clear the structure
    XOS_MEMORY_SANITIZE(p_ptTimer, sizeof(xOsTimerCtx));

    // Create kernel timer using timerfd
    p_ptTimer->t_iTimerFd = timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC);
    if (p_ptTimer->t_iTimerFd == -1)
    {
        X_LOG_TRACE("xTimerCreate: Failed to create timerfd: %d", errno);
        return XOS_TIMER_FD_ERROR;
    }

    // Initialize timer parameters
    p_ptTimer->t_ulPeriod = p_ulPeriod;
    p_ptTimer->t_ucMode = p_ucMode;
    atomic_store(&p_ptTimer->t_bActive, false);
    atomic_store(&p_ptTimer->t_bStopRequested, false);
    p_ptTimer->t_ullExpirationCount = 0;

    // Initialize mutex for control operations
    int l_iResult = mutexCreate(&p_ptTimer->t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        close(p_ptTimer->t_iTimerFd);
        p_ptTimer->t_iTimerFd = -1;
        X_LOG_TRACE("xTimerCreate: Failed to create mutex");
        return l_iResult;
    }

    X_LOG_TRACE("xTimerCreate: Timer created with period %u ms, mode %s", 
                p_ulPeriod, p_ucMode == XOS_TIMER_MODE_PERIODIC ? "periodic" : "oneshot");

    return XOS_TIMER_OK;
}

////////////////////////////////////////////////////////////
/// xTimerStart
////////////////////////////////////////////////////////////
int xTimerStart(xOsTimerCtx *p_ptTimer)
{
    X_ASSERT(p_ptTimer != NULL);
    X_ASSERT(p_ptTimer->t_iTimerFd != -1);

    int l_iResult = mutexLock(&p_ptTimer->t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        X_LOG_TRACE("xTimerStart: Failed to lock mutex");
        return l_iResult;
    }

    // Record start time for statistics
    if (clock_gettime(CLOCK_MONOTONIC, &p_ptTimer->t_tStart) != 0)
    {
        mutexUnlock(&p_ptTimer->t_tMutex);
        X_LOG_TRACE("xTimerStart: clock_gettime failed: %d", errno);
        return XOS_TIMER_ERROR;
    }

    // Configure timer specification
    struct itimerspec l_tTimerSpec;
    
    // Convert period to timespec
    l_tTimerSpec.it_value.tv_sec = p_ptTimer->t_ulPeriod / 1000;
    l_tTimerSpec.it_value.tv_nsec = (p_ptTimer->t_ulPeriod % 1000) * 1000000L;

    if (p_ptTimer->t_ucMode == XOS_TIMER_MODE_PERIODIC)
    {
        // Periodic timer: set interval same as initial value
        l_tTimerSpec.it_interval = l_tTimerSpec.it_value;
    }
    else
    {
        // One-shot timer: no interval
        l_tTimerSpec.it_interval.tv_sec = 0;
        l_tTimerSpec.it_interval.tv_nsec = 0;
    }

    // Start the kernel timer
    if (timerfd_settime(p_ptTimer->t_iTimerFd, 0, &l_tTimerSpec, NULL) == -1)
    {
        mutexUnlock(&p_ptTimer->t_tMutex);
        X_LOG_TRACE("xTimerStart: timerfd_settime failed: %d", errno);
        return XOS_TIMER_ERROR;
    }

    atomic_store(&p_ptTimer->t_bActive, true);
    atomic_store(&p_ptTimer->t_bStopRequested, false);
    p_ptTimer->t_ullExpirationCount = 0;

    mutexUnlock(&p_ptTimer->t_tMutex);

    X_LOG_TRACE("xTimerStart: Timer started successfully");
    return XOS_TIMER_OK;
}

////////////////////////////////////////////////////////////
/// xTimerStop
////////////////////////////////////////////////////////////
int xTimerStop(xOsTimerCtx *p_ptTimer)
{
    X_ASSERT(p_ptTimer != NULL);
    X_ASSERT(p_ptTimer->t_iTimerFd != -1);

    int l_iResult = mutexLock(&p_ptTimer->t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        X_LOG_TRACE("xTimerStop: Failed to lock mutex");
        return l_iResult;
    }

    // Signal stop request (for blocking operations)
    atomic_store(&p_ptTimer->t_bStopRequested, true);

    // Stop the kernel timer by setting it to zero
    struct itimerspec l_tTimerSpec = {{0, 0}, {0, 0}};
    if (timerfd_settime(p_ptTimer->t_iTimerFd, 0, &l_tTimerSpec, NULL) == -1)
    {
        mutexUnlock(&p_ptTimer->t_tMutex);
        X_LOG_TRACE("xTimerStop: timerfd_settime failed: %d", errno);
        return XOS_TIMER_ERROR;
    }

    atomic_store(&p_ptTimer->t_bActive, false);

    mutexUnlock(&p_ptTimer->t_tMutex);

    X_LOG_TRACE("xTimerStop: Timer stopped successfully");
    return XOS_TIMER_OK;
}

////////////////////////////////////////////////////////////
/// xTimerDestroy
////////////////////////////////////////////////////////////
int xTimerDestroy(xOsTimerCtx *p_ptTimer)
{
    X_ASSERT(p_ptTimer != NULL);

    if (p_ptTimer->t_iTimerFd != -1)
    {
        // Stop timer first
        xTimerStop(p_ptTimer);

        // Close the timer file descriptor
        close(p_ptTimer->t_iTimerFd);
        p_ptTimer->t_iTimerFd = -1;
    }

    // Destroy mutex
    int l_iResult = mutexDestroy(&p_ptTimer->t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        X_LOG_TRACE("xTimerDestroy: Failed to destroy mutex");
        return l_iResult;
    }

    // Clear the structure
    XOS_MEMORY_SANITIZE(p_ptTimer, sizeof(xOsTimerCtx));

    X_LOG_TRACE("xTimerDestroy: Timer destroyed successfully");
    return XOS_TIMER_OK;
}

////////////////////////////////////////////////////////////
/// xTimerWaitExpiration
////////////////////////////////////////////////////////////
int xTimerWaitExpiration(xOsTimerCtx *p_ptTimer, uint64_t *p_pullExpirations)
{
    X_ASSERT(p_ptTimer != NULL);
    X_ASSERT(p_ptTimer->t_iTimerFd != -1);

    if (!atomic_load(&p_ptTimer->t_bActive))
    {
        return XOS_TIMER_NOT_INIT;
    }

    uint64_t l_ullExpirations;
    ssize_t l_iBytes = read(p_ptTimer->t_iTimerFd, &l_ullExpirations, sizeof(l_ullExpirations));

    if (l_iBytes == -1)
    {
        if (errno == EAGAIN || errno == EWOULDBLOCK)
        {
            return XOS_TIMER_TIMEOUT;
        }
        else if (errno == EINTR)
        {
            // Interrupted by signal, check if stop was requested
            if (atomic_load(&p_ptTimer->t_bStopRequested))
            {
                return XOS_TIMER_NOT_INIT;
            }
            return XOS_TIMER_TIMEOUT;
        }
        X_LOG_TRACE("xTimerWaitExpiration: read failed: %d", errno);
        return XOS_TIMER_ERROR;
    }

    if (l_iBytes != sizeof(l_ullExpirations))
    {
        X_LOG_TRACE("xTimerWaitExpiration: unexpected read size: %zd", l_iBytes);
        return XOS_TIMER_ERROR;
    }

    // Update expiration statistics
    p_ptTimer->t_ullExpirationCount += l_ullExpirations;

    if (p_pullExpirations != NULL)
    {
        *p_pullExpirations = l_ullExpirations;
    }

    // For one-shot timers, mark as inactive after expiration
    if (p_ptTimer->t_ucMode == XOS_TIMER_MODE_ONESHOT)
    {
        atomic_store(&p_ptTimer->t_bActive, false);
    }

    return XOS_TIMER_OK;
}

////////////////////////////////////////////////////////////
/// xTimerCheckExpired
////////////////////////////////////////////////////////////
int xTimerCheckExpired(xOsTimerCtx *p_ptTimer, uint64_t *p_pullExpirations)
{
    X_ASSERT(p_ptTimer != NULL);
    X_ASSERT(p_ptTimer->t_iTimerFd != -1);

    if (!atomic_load(&p_ptTimer->t_bActive))
    {
        return XOS_TIMER_NOT_INIT;
    }

    // Make the file descriptor non-blocking for this check
    int l_iFlags = fcntl(p_ptTimer->t_iTimerFd, F_GETFL);
    if (l_iFlags == -1)
    {
        X_LOG_TRACE("xTimerCheckExpired: fcntl F_GETFL failed: %d", errno);
        return XOS_TIMER_ERROR;
    }

    if (fcntl(p_ptTimer->t_iTimerFd, F_SETFL, l_iFlags | O_NONBLOCK) == -1)
    {
        X_LOG_TRACE("xTimerCheckExpired: fcntl F_SETFL failed: %d", errno);
        return XOS_TIMER_ERROR;
    }

    uint64_t l_ullExpirations;
    ssize_t l_iBytes = read(p_ptTimer->t_iTimerFd, &l_ullExpirations, sizeof(l_ullExpirations));

    // Restore blocking mode
    fcntl(p_ptTimer->t_iTimerFd, F_SETFL, l_iFlags);

    if (l_iBytes == -1)
    {
        if (errno == EAGAIN || errno == EWOULDBLOCK)
        {
            return XOS_TIMER_TIMEOUT;
        }
        X_LOG_TRACE("xTimerCheckExpired: read failed: %d", errno);
        return XOS_TIMER_ERROR;
    }

    if (l_iBytes != sizeof(l_ullExpirations))
    {
        X_LOG_TRACE("xTimerCheckExpired: unexpected read size: %zd", l_iBytes);
        return XOS_TIMER_ERROR;
    }

    // Update expiration statistics
    p_ptTimer->t_ullExpirationCount += l_ullExpirations;

    if (p_pullExpirations != NULL)
    {
        *p_pullExpirations = l_ullExpirations;
    }

    // For one-shot timers, mark as inactive after expiration
    if (p_ptTimer->t_ucMode == XOS_TIMER_MODE_ONESHOT)
    {
        atomic_store(&p_ptTimer->t_bActive, false);
    }

    return XOS_TIMER_OK;
}

////////////////////////////////////////////////////////////
/// xTimerGetCurrentMs
////////////////////////////////////////////////////////////
inline uint64_t xTimerGetCurrentMs(void)
{
    struct timespec l_tNow;
    if (clock_gettime(CLOCK_MONOTONIC, &l_tNow) != 0)
    {
        X_LOG_TRACE("xTimerGetCurrentMs: clock_gettime failed: %d", errno);
        return 0;
    }
    return (uint64_t)((uint64_t)l_tNow.tv_sec * 1000ULL +
                     (uint64_t)l_tNow.tv_nsec / 1000000ULL);
}

////////////////////////////////////////////////////////////
/// xTimerDelay
////////////////////////////////////////////////////////////
int xTimerDelay(uint32_t p_ulDelay)
{
    if (p_ulDelay > XOS_TIMER_MAX_PERIOD_MS)
    {
        X_LOG_TRACE("xTimerDelay: Delay too large %u ms", p_ulDelay);
        return XOS_TIMER_INVALID;
    }

    if (p_ulDelay == 0)
    {
        return XOS_TIMER_OK;
    }

    // Calculate absolute wakeup time using monotonic clock
    struct timespec l_tWakeup;
    if (clock_gettime(CLOCK_MONOTONIC, &l_tWakeup) != 0)
    {
        X_LOG_TRACE("xTimerDelay: clock_gettime failed: %d", errno);
        return XOS_TIMER_ERROR;
    }

    // Add delay to current time
    uint64_t l_ullDelayNs = (uint64_t)p_ulDelay * 1000000ULL;
    uint64_t l_ullTotalNs = (uint64_t)l_tWakeup.tv_nsec + l_ullDelayNs;

    l_tWakeup.tv_sec += l_ullTotalNs / 1000000000ULL;
    l_tWakeup.tv_nsec = l_ullTotalNs % 1000000000ULL;

    // Use absolute time sleep - let kernel handle the precise timing
    int l_iResult = clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &l_tWakeup, NULL);
    
    if (l_iResult != 0 && l_iResult != EINTR)
    {
        X_LOG_TRACE("xTimerDelay: clock_nanosleep failed with error %d", l_iResult);
        return XOS_TIMER_ERROR;
    }

    return XOS_TIMER_OK;
}

////////////////////////////////////////////////////////////
/// xTimerProcessCallback
////////////////////////////////////////////////////////////
int xTimerProcessCallback(xOsTimerCtx *p_ptTimer, void (*p_pfCallback)(void *), void *p_pvData)
{
    X_ASSERT(p_ptTimer != NULL);
    X_ASSERT(p_pfCallback != NULL);

    if (!atomic_load(&p_ptTimer->t_bActive))
    {
        return XOS_TIMER_NOT_INIT;
    }

    int l_iTotalCallbacks = 0;

    while (atomic_load(&p_ptTimer->t_bActive) && !atomic_load(&p_ptTimer->t_bStopRequested))
    {
        uint64_t l_ullExpirations;
        int l_iResult = xTimerWaitExpiration(p_ptTimer, &l_ullExpirations);

        if (l_iResult == XOS_TIMER_OK)
        {
            // Limit callbacks to prevent DoS
            uint64_t l_ullCallbackCount = l_ullExpirations;
            if (l_ullCallbackCount > XOS_TIMER_MAX_CALLBACKS)
            {
                X_LOG_TRACE("xTimerProcessCallback: Limiting callbacks from %llu to %d",
                            l_ullCallbackCount, XOS_TIMER_MAX_CALLBACKS);
                l_ullCallbackCount = XOS_TIMER_MAX_CALLBACKS;
            }

            // Execute callbacks for each expiration
            for (uint64_t i = 0; i < l_ullCallbackCount; i++)
            {
                p_pfCallback(p_pvData);
                l_iTotalCallbacks++;

                // Check if stop was requested during callback
                if (atomic_load(&p_ptTimer->t_bStopRequested))
                {
                    break;
                }
            }

            // For one-shot timers, exit after first expiration
            if (p_ptTimer->t_ucMode == XOS_TIMER_MODE_ONESHOT)
            {
                break;
            }
        }
        else if (l_iResult == XOS_TIMER_NOT_INIT || l_iResult == XOS_TIMER_TIMEOUT)
        {
            // Timer stopped or interrupted
            break;
        }
        else
        {
            // Error occurred
            return l_iResult;
        }
    }

    return l_iTotalCallbacks;
}

////////////////////////////////////////////////////////////
/// xTimerGetFd
////////////////////////////////////////////////////////////
int xTimerGetFd(xOsTimerCtx *p_ptTimer)
{
    X_ASSERT(p_ptTimer != NULL);
    return p_ptTimer->t_iTimerFd;
}

////////////////////////////////////////////////////////////
/// xTimerGetStats
////////////////////////////////////////////////////////////
int xTimerGetStats(xOsTimerCtx *p_ptTimer, uint64_t *p_pullTotalExpirations, uint64_t *p_pullUptime)
{
    X_ASSERT(p_ptTimer != NULL);

    int l_iResult = mutexLock(&p_ptTimer->t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        return l_iResult;
    }

    if (p_pullTotalExpirations != NULL)
    {
        *p_pullTotalExpirations = p_ptTimer->t_ullExpirationCount;
    }

    if (p_pullUptime != NULL)
    {
        if (atomic_load(&p_ptTimer->t_bActive))
        {
            struct timespec l_tNow;
            if (clock_gettime(CLOCK_MONOTONIC, &l_tNow) == 0)
            {
                uint64_t l_ullNowMs = (uint64_t)l_tNow.tv_sec * 1000ULL + (uint64_t)l_tNow.tv_nsec / 1000000ULL;
                uint64_t l_ullStartMs = (uint64_t)p_ptTimer->t_tStart.tv_sec * 1000ULL + (uint64_t)p_ptTimer->t_tStart.tv_nsec / 1000000ULL;
                *p_pullUptime = l_ullNowMs - l_ullStartMs;
            }
            else
            {
                *p_pullUptime = 0;
            }
        }
        else
        {
            *p_pullUptime = 0;
        }
    }

    mutexUnlock(&p_ptTimer->t_tMutex);
    return XOS_TIMER_OK;
}
