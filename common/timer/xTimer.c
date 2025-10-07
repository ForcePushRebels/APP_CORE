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
#include <fcntl.h>
#include <poll.h>
#include <sys/timerfd.h>
#include <time.h>
#include <unistd.h>

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
    p_ptTimer->t_ulExpirationCount = 0;

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
                p_ulPeriod,
                p_ucMode == XOS_TIMER_MODE_PERIODIC ? "periodic" : "oneshot");

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

    // Convert period to timespec - safe conversion since timer periods are positive
    {
        time_t l_tSeconds = (time_t)(p_ptTimer->t_ulPeriod / 1000U);
        long l_lNanoseconds = (long)((p_ptTimer->t_ulPeriod % 1000U) * 1000000U);
        l_tTimerSpec.it_value.tv_sec = l_tSeconds;
        l_tTimerSpec.it_value.tv_nsec = l_lNanoseconds;
    }

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
    p_ptTimer->t_ulExpirationCount = 0;

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
int xTimerWaitExpiration(xOsTimerCtx *p_ptTimer, uint32_t *p_pulExpirations)
{
    X_ASSERT(p_ptTimer != NULL);
    X_ASSERT(p_ptTimer->t_iTimerFd != -1);

    if (!atomic_load(&p_ptTimer->t_bActive))
    {
        return XOS_TIMER_NOT_INIT;
    }

    uint64_t l_ulExpirations;
    ssize_t l_iBytes = read(p_ptTimer->t_iTimerFd, &l_ulExpirations, sizeof(l_ulExpirations));

    if (l_iBytes == -1)
    {
        // Check for non-blocking I/O completion
#if defined(__ARM_ARCH_6__)
        if (errno == EAGAIN)
#else
        if (errno == EAGAIN || errno == EWOULDBLOCK)
#endif
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

    if (l_iBytes != sizeof(l_ulExpirations))
    {
        X_LOG_TRACE("xTimerWaitExpiration: unexpected read size: %zd", l_iBytes);
        return XOS_TIMER_ERROR;
    }

    // Vérification de dépassement pour l_ulExpirations seul
    if (l_ulExpirations > UINT32_MAX)
    {
        X_LOG_TRACE("xTimer: expiration count exceeds UINT32_MAX");
        p_ptTimer->t_ulExpirationCount = UINT32_MAX;
        if (p_pulExpirations != NULL)
        {
            *p_pulExpirations = UINT32_MAX;
        }
    }
    else
    {
        uint32_t l_ulExpirations32 = (uint32_t)l_ulExpirations;

        // Vérification de dépassement pour l'addition
        if (p_ptTimer->t_ulExpirationCount > UINT32_MAX - l_ulExpirations32)
        {
            X_LOG_TRACE("xTimer: expiration count would overflow, resetting to max");
            p_ptTimer->t_ulExpirationCount = UINT32_MAX;
        }
        else
        {
            p_ptTimer->t_ulExpirationCount += l_ulExpirations32;
        }

        if (p_pulExpirations != NULL)
        {
            *p_pulExpirations = l_ulExpirations32;
        }
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
int xTimerCheckExpired(xOsTimerCtx *p_ptTimer, uint32_t *p_pulExpirations)
{
    X_ASSERT(p_ptTimer != NULL);
    X_ASSERT(p_ptTimer->t_iTimerFd != -1);

    if (!atomic_load(&p_ptTimer->t_bActive))
    {
        return XOS_TIMER_NOT_INIT;
    }

    struct pollfd l_tPfd = {.fd = p_ptTimer->t_iTimerFd, .events = POLLIN, .revents = 0};
    int l_iPoll = poll(&l_tPfd, 1, 0);

    if (l_iPoll == 0)
    {
        return XOS_TIMER_TIMEOUT;
    }
    else if (l_iPoll < 0)
    {
        X_LOG_TRACE("xTimerCheckExpired: poll failed: %d", errno);
        return XOS_TIMER_ERROR;
    }

    uint64_t l_ulExpirations;
    ssize_t l_iBytes = read(p_ptTimer->t_iTimerFd, &l_ulExpirations, sizeof(l_ulExpirations));

    if (l_iBytes == -1)
    {
#if defined(__ARM_ARCH_6__)
        if (errno == EAGAIN)
#else
        if (errno == EAGAIN || errno == EWOULDBLOCK)
#endif
        {
            return XOS_TIMER_TIMEOUT;
        }
        X_LOG_TRACE("xTimerCheckExpired: read failed: %d", errno);
        return XOS_TIMER_ERROR;
    }

    if (l_iBytes != sizeof(l_ulExpirations))
    {
        X_LOG_TRACE("xTimerCheckExpired: unexpected read size: %zd", l_iBytes);
        return XOS_TIMER_ERROR;
    }

    // Convert to uint32_t with overflow protection
    uint32_t l_ulExpirations32;
    if (l_ulExpirations > UINT32_MAX)
    {
        X_LOG_TRACE("xTimer: expiration count exceeds UINT32_MAX, capping at max value");
        l_ulExpirations32 = UINT32_MAX;
        p_ptTimer->t_ulExpirationCount = UINT32_MAX;
    }
    else
    {
        l_ulExpirations32 = (uint32_t)l_ulExpirations;

        // Check for addition overflow
        if (p_ptTimer->t_ulExpirationCount > UINT32_MAX - l_ulExpirations32)
        {
            X_LOG_TRACE("xTimer: expiration count would overflow, resetting to max");
            p_ptTimer->t_ulExpirationCount = UINT32_MAX;
        }
        else
        {
            p_ptTimer->t_ulExpirationCount += l_ulExpirations32;
        }
    }

    if (p_pulExpirations != NULL)
    {
        *p_pulExpirations = l_ulExpirations32;
    }

    if (p_ptTimer->t_ucMode == XOS_TIMER_MODE_ONESHOT)
    {
        atomic_store(&p_ptTimer->t_bActive, false);
    }

    return XOS_TIMER_OK;
}

////////////////////////////////////////////////////////////
/// xTimerGetCurrentMs
////////////////////////////////////////////////////////////
inline uint32_t xTimerGetCurrentMs(void)
{
    struct timespec l_tNow;
    if (clock_gettime(CLOCK_MONOTONIC, &l_tNow) != 0)
    {
        X_LOG_TRACE("xTimerGetCurrentMs: clock_gettime failed: %d", errno);
        return 0;
    }

    // Strict overflow check: tv_sec must not exceed max safe value
    if ((uint64_t)l_tNow.tv_sec > XOS_TIMER_MAX_TIMESTAMP_SEC)
    {
        X_LOG_TRACE("xTimerGetCurrentMs: timestamp overflow detected");
        return UINT32_MAX;
    }

    // Safe conversion: calculate milliseconds with overflow protection
    uint64_t l_ullMs = (uint64_t)l_tNow.tv_sec * 1000ULL + (uint64_t)l_tNow.tv_nsec / 1000000ULL;

    if (l_ullMs > UINT32_MAX)
    {
        X_LOG_TRACE("xTimerGetCurrentMs: calculation overflow detected");
        return UINT32_MAX;
    }

    return (uint32_t)l_ullMs;
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

    // Check for potential overflow in nanosecond calculation
    const uint32_t l_ulMaxSafeDelayMs = XOS_TIMER_MAX_DELAY_MS;
    if (p_ulDelay > l_ulMaxSafeDelayMs)
    {
        X_LOG_TRACE("xTimerDelay: Delay %u ms would cause nanosecond overflow", p_ulDelay);
        return XOS_TIMER_INVALID;
    }

    // Calculate absolute wakeup time using monotonic clock
    struct timespec l_tWakeup;
    if (clock_gettime(CLOCK_MONOTONIC, &l_tWakeup) != 0)
    {
        X_LOG_TRACE("xTimerDelay: clock_gettime failed: %d", errno);
        return XOS_TIMER_ERROR;
    }

    // Add delay to current time with overflow protection
    uint64_t l_ullDelayNs = (uint64_t)p_ulDelay * 1000000ULL;
    uint64_t l_ullTotalNs = (uint64_t)l_tWakeup.tv_nsec + l_ullDelayNs;

    // Check for seconds overflow
    uint64_t l_ullAdditionalSec = l_ullTotalNs / 1000000000ULL;
    if (l_ullAdditionalSec > UINT32_MAX - (uint64_t)l_tWakeup.tv_sec)
    {
        X_LOG_TRACE("xTimerDelay: Total seconds would overflow");
        return XOS_TIMER_INVALID;
    }

    // Safe assignment to timespec fields
    {
        time_t l_tAdditionalSec = (time_t)l_ullAdditionalSec;
        long l_lRemainderNs = (long)(l_ullTotalNs % 1000000000ULL);
        l_tWakeup.tv_sec += l_tAdditionalSec;
        l_tWakeup.tv_nsec = l_lRemainderNs;
    }

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
        uint32_t l_ulExpirations;
        int l_iResult = xTimerWaitExpiration(p_ptTimer, &l_ulExpirations);

        if (l_iResult == XOS_TIMER_OK)
        {
            // Limit callbacks to prevent DoS
            uint32_t l_ulCallbackCount = l_ulExpirations;
            if (l_ulCallbackCount > XOS_TIMER_MAX_CALLBACKS)
            {
                #ifdef DEBUG
                X_LOG_TRACE("xTimerProcessCallback: Limiting callbacks from %lu to %d for function %s",
                            l_ulCallbackCount,
                            XOS_TIMER_MAX_CALLBACKS,
                            p_ptTimer->t_acTimerName);
                #else
                X_LOG_TRACE("xTimerProcessCallback: Limiting callbacks from %lu to %d",
                            l_ulCallbackCount,
                            XOS_TIMER_MAX_CALLBACKS);
                #endif
                l_ulCallbackCount = XOS_TIMER_MAX_CALLBACKS;
            }

            // Execute callbacks for each expiration
            for (uint32_t i = 0; i < l_ulCallbackCount; i++)
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
