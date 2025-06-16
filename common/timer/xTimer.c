////////////////////////////////////////////////////////////
//  timer source file
//  implements the timer functions
//
// general discloser: copy or share the file is forbidden
// Written : 12/01/2025
// Modified: 12/05/2025 - Improved thread safety and timing precision
// Modified: 30/05/2025 - Fixed code execution security issues
////////////////////////////////////////////////////////////

#include "xTimer.h"
#include "xAssert.h"
#include "xLog.h"
#include "xOsMemory.h"
#include <errno.h>
#include <time.h>

////////////////////////////////////////////////////////////
/// xTimerCreate
////////////////////////////////////////////////////////////
int xTimerCreate(xOsTimerCtx *p_ptTimer, uint32_t p_ulPeriod, uint8_t p_ucMode)
{
    X_ASSERT(p_ptTimer != NULL);
    X_ASSERT(p_ulPeriod > XOS_TIMER_MIN_PERIOD_MS && p_ulPeriod < XOS_TIMER_MAX_PERIOD_MS);
    X_ASSERT(p_ucMode == XOS_TIMER_MODE_PERIODIC || p_ucMode == XOS_TIMER_MODE_ONESHOT);

    // Clear the structure
    XOS_MEMORY_SANITIZE(p_ptTimer, sizeof(xOsTimerCtx));

    // Initialize timer parameters
    p_ptTimer->t_ulPeriod = p_ulPeriod;
    p_ptTimer->t_ucMode = p_ucMode;
    p_ptTimer->t_ucActive = 0;

    // Initialize mutex for thread safety
    int l_iResult = mutexCreate(&p_ptTimer->t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        X_LOG_TRACE("xTimerCreate: Failed to create mutex");
        return l_iResult;
    }

    return XOS_TIMER_OK;
}

////////////////////////////////////////////////////////////
/// xTimerStart
////////////////////////////////////////////////////////////
int xTimerStart(xOsTimerCtx *p_ptTimer)
{
    X_ASSERT(p_ptTimer != NULL);

    int l_iResult;

    // Lock mutex for thread safety
    l_iResult = mutexLock(&p_ptTimer->t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        X_LOG_TRACE("xTimerStart: Failed to lock mutex");
        return l_iResult;
    }

    // Validate timer period before starting
    if (p_ptTimer->t_ulPeriod < XOS_TIMER_MIN_PERIOD_MS || 
        p_ptTimer->t_ulPeriod > XOS_TIMER_MAX_PERIOD_MS)
    {
        mutexUnlock(&p_ptTimer->t_tMutex);
        X_LOG_TRACE("xTimerStart: Invalid timer period %u", p_ptTimer->t_ulPeriod);
        return XOS_TIMER_INVALID;
    }

    // Get current time with error checking
    if (clock_gettime(CLOCK_MONOTONIC, &p_ptTimer->t_tStart) != 0)
    {
        mutexUnlock(&p_ptTimer->t_tMutex);
        X_LOG_TRACE("xTimerStart: clock_gettime failed with errno %d", errno);
        return XOS_TIMER_ERROR;
    }

    // Convert period from milliseconds to nanoseconds with overflow protection
    uint64_t l_ulPeriodNs;
    l_ulPeriodNs = (uint64_t)p_ptTimer->t_ulPeriod * 1000000ULL;

    // Calculate next trigger time with overflow protection
    p_ptTimer->t_tNext = p_ptTimer->t_tStart;
    uint64_t l_ulCurrentNs = (uint64_t)p_ptTimer->t_tNext.tv_nsec;
    
    if (l_ulCurrentNs > (UINT64_MAX - l_ulPeriodNs))
    {
        mutexUnlock(&p_ptTimer->t_tMutex);
        X_LOG_TRACE("xTimerStart: Time calculation overflow");
        return XOS_TIMER_ERROR;
    }
    
    uint64_t l_ulNextNs = l_ulCurrentNs + l_ulPeriodNs;

    p_ptTimer->t_tNext.tv_sec += l_ulNextNs / 1000000000ULL;
    p_ptTimer->t_tNext.tv_nsec = l_ulNextNs % 1000000000ULL;

    p_ptTimer->t_ucActive = 1;

    // Unlock mutex
    l_iResult = mutexUnlock(&p_ptTimer->t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        X_LOG_TRACE("xTimerStart: Failed to unlock mutex");
        return l_iResult;
    }

    if (p_ptTimer->t_ucMode == XOS_TIMER_MODE_PERIODIC)
    {
        atomic_store(&p_ptTimer->t_bPeriodicLockFlag, true);
    }

    return XOS_TIMER_OK;
}

////////////////////////////////////////////////////////////
/// xTimerStop
////////////////////////////////////////////////////////////
int xTimerStop(xOsTimerCtx *p_ptTimer)
{
    X_ASSERT(p_ptTimer != NULL);

    int l_iResult;

    if (p_ptTimer->t_ucMode == XOS_TIMER_MODE_PERIODIC)
    {
        atomic_store(&p_ptTimer->t_bPeriodicLockFlag, false);
    }

    // Lock mutex for thread safety
    l_iResult = mutexLock(&p_ptTimer->t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        X_LOG_TRACE("xTimerStop: Failed to lock mutex");
        return l_iResult;
    }

    p_ptTimer->t_ucActive = 0;

    // Unlock mutex
    l_iResult = mutexUnlock(&p_ptTimer->t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        X_LOG_TRACE("xTimerStop: Failed to unlock mutex");
        return l_iResult;
    }

    return XOS_TIMER_OK;
}

////////////////////////////////////////////////////////////
/// xTimerDestroy
////////////////////////////////////////////////////////////
int xTimerDestroy(xOsTimerCtx *p_ptTimer)
{
    X_ASSERT(p_ptTimer != NULL);

    int l_iResult = mutexDestroy(&p_ptTimer->t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        X_LOG_TRACE("xTimerDestroy: Failed to destroy mutex");
        return l_iResult;
    }

    atomic_store(&p_ptTimer->t_bPeriodicLockFlag, false);

    XOS_MEMORY_SANITIZE(p_ptTimer, sizeof(xOsTimerCtx));

    return XOS_TIMER_OK;
}

////////////////////////////////////////////////////////////
/// xTimerExpired
////////////////////////////////////////////////////////////
int xTimerExpired(xOsTimerCtx *p_ptTimer)
{
    X_ASSERT(p_ptTimer != NULL);

    int l_iResult;
    int l_iReturn;

    // Lock mutex for thread safety
    l_iResult = mutexLock(&p_ptTimer->t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        X_LOG_TRACE("xTimerExpired: Failed to lock mutex");
        return l_iResult;
    }

    if (!p_ptTimer->t_ucActive)
    {
        // Unlock mutex before return
        mutexUnlock(&p_ptTimer->t_tMutex);
        return XOS_TIMER_NOT_INIT;
    }

    struct timespec l_tNow;
    if (clock_gettime(CLOCK_MONOTONIC, &l_tNow) != 0)
    {
        mutexUnlock(&p_ptTimer->t_tMutex);
        X_LOG_TRACE("xTimerExpired: clock_gettime failed with errno %d", errno);
        return XOS_TIMER_ERROR;
    }

    // Fast comparison first - avoid expensive calculations if not expired
    if (l_tNow.tv_sec < p_ptTimer->t_tNext.tv_sec || 
        (l_tNow.tv_sec == p_ptTimer->t_tNext.tv_sec && l_tNow.tv_nsec < p_ptTimer->t_tNext.tv_nsec))
    {
        // Timer not expired, early exit
        l_iReturn = XOS_TIMER_TIMEOUT;
    }
    else
    {
        // Timer expired - handle periodic or one-shot mode
        if (p_ptTimer->t_ucMode == XOS_TIMER_MODE_PERIODIC)
        {
            // Convert times to nanoseconds for calculation with overflow protection
            uint64_t l_ulNowNs = (uint64_t)l_tNow.tv_sec * 1000000000ULL + (uint64_t)l_tNow.tv_nsec;
            uint64_t l_ulStartNs = (uint64_t)p_ptTimer->t_tStart.tv_sec * 1000000000ULL + (uint64_t)p_ptTimer->t_tStart.tv_nsec;
            uint64_t l_ulElapsedNs = l_ulNowNs - l_ulStartNs;

            // Check for period overflow and division by zero
            if (p_ptTimer->t_ulPeriod == 0 || p_ptTimer->t_ulPeriod > XOS_TIMER_MAX_PERIOD_MS)
            {
                mutexUnlock(&p_ptTimer->t_tMutex);
                X_LOG_TRACE("xTimerExpired: Invalid period for calculation");
                return XOS_TIMER_ERROR;
            }

            uint64_t l_ulPeriodNs = (uint64_t)p_ptTimer->t_ulPeriod * 1000000ULL;
            if (l_ulPeriodNs == 0)
            {
                mutexUnlock(&p_ptTimer->t_tMutex);
                X_LOG_TRACE("xTimerExpired: Zero period in nanoseconds");
                return XOS_TIMER_ERROR;
            }

            uint64_t l_ulPeriods = (l_ulElapsedNs / l_ulPeriodNs) + 1;

            // Calculate next trigger time with overflow protection
            if (l_ulPeriods > (UINT64_MAX / l_ulPeriodNs))
            {
                mutexUnlock(&p_ptTimer->t_tMutex);
                X_LOG_TRACE("xTimerExpired: Time calculation overflow");
                return XOS_TIMER_ERROR;
            }

            uint64_t l_ulNextTime = l_ulStartNs + (l_ulPeriods * l_ulPeriodNs);
            p_ptTimer->t_tNext.tv_sec = (time_t)(l_ulNextTime / 1000000000ULL);
            p_ptTimer->t_tNext.tv_nsec = (long)(l_ulNextTime % 1000000000ULL);
        }
        else
        {
            p_ptTimer->t_ucActive = 0;
        }
        l_iReturn = XOS_TIMER_OK;
    }

    // Unlock mutex
    l_iResult = mutexUnlock(&p_ptTimer->t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        X_LOG_TRACE("xTimerExpired: Failed to unlock mutex");
        return l_iResult;
    }

    return l_iReturn;
}

////////////////////////////////////////////////////////////
/// xTimerGetCurrentMs
////////////////////////////////////////////////////////////
inline uint64_t xTimerGetCurrentMs(void)
{
    struct timespec l_tNow;
    if (clock_gettime(CLOCK_MONOTONIC, &l_tNow) != 0)
    {
        X_LOG_TRACE("xTimerGetCurrentMs: clock_gettime failed with errno %d", errno);
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
    // Input validation
    if (p_ulDelay > XOS_TIMER_MAX_PERIOD_MS)
    {
        X_LOG_TRACE("xTimerDelay: Delay too large %u ms", p_ulDelay);
        return XOS_TIMER_INVALID;
    }

    struct timespec l_tSleep, l_tRemain;

    // Convert milliseconds to timespec structure
    l_tSleep.tv_sec = p_ulDelay / 1000;
    l_tSleep.tv_nsec = (p_ulDelay % 1000) * 1000000L;

    // Use nanosleep for higher precision
    while (nanosleep(&l_tSleep, &l_tRemain) == -1)
    {
        if (errno != EINTR)
        {
            X_LOG_TRACE("xTimerDelay: nanosleep failed with errno %d", errno);
            return XOS_TIMER_ERROR;
        }

        // If interrupted, continue sleeping with remaining time
        l_tSleep = l_tRemain;
    }

    return XOS_TIMER_OK;
}

////////////////////////////////////////////////////////////
/// xTimerProcessElapsedPeriods
////////////////////////////////////////////////////////////
int xTimerProcessElapsedPeriods(xOsTimerCtx *p_ptTimer, void (*p_pfCallback)(void *), void *p_pvData)
{
    X_ASSERT(p_ptTimer != NULL);
    X_ASSERT(p_pfCallback != NULL);

    int l_iResult;
    int l_iPeriodCount = 0;

    // Lock mutex for thread safety
    l_iResult = mutexLock(&p_ptTimer->t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        X_LOG_TRACE("xTimerProcessElapsedPeriods: Failed to lock mutex");
        return l_iResult;
    }

    if (!p_ptTimer->t_ucActive)
    {
        // Unlock mutex before return
        mutexUnlock(&p_ptTimer->t_tMutex);
        return XOS_TIMER_NOT_INIT;
    }

    struct timespec l_tNow;
    if (clock_gettime(CLOCK_MONOTONIC, &l_tNow) != 0)
    {
        mutexUnlock(&p_ptTimer->t_tMutex);
        X_LOG_TRACE("xTimerProcessElapsedPeriods: clock_gettime failed with errno %d", errno);
        return XOS_TIMER_ERROR;
    }

    // Convert times to nanoseconds for calculation with overflow protection
    uint64_t l_ulNowNs = (uint64_t)l_tNow.tv_sec * 1000000000ULL + (uint64_t)l_tNow.tv_nsec;
    uint64_t l_ulStartNs = (uint64_t)p_ptTimer->t_tStart.tv_sec * 1000000000ULL + (uint64_t)p_ptTimer->t_tStart.tv_nsec;
    uint64_t l_ulNextNs = (uint64_t)p_ptTimer->t_tNext.tv_sec * 1000000000ULL + (uint64_t)p_ptTimer->t_tNext.tv_nsec;

    // Validate period for division
    if (p_ptTimer->t_ulPeriod == 0 || p_ptTimer->t_ulPeriod > XOS_TIMER_MAX_PERIOD_MS)
    {
        mutexUnlock(&p_ptTimer->t_tMutex);
        X_LOG_TRACE("xTimerProcessElapsedPeriods: Invalid period %u", p_ptTimer->t_ulPeriod);
        return XOS_TIMER_ERROR;
    }

    uint64_t l_ulPeriodNs = (uint64_t)p_ptTimer->t_ulPeriod * 1000000ULL;
    if (l_ulPeriodNs == 0)
    {
        mutexUnlock(&p_ptTimer->t_tMutex);
        X_LOG_TRACE("xTimerProcessElapsedPeriods: Zero period in nanoseconds");
        return XOS_TIMER_ERROR;
    }

    if (l_ulNowNs >= l_ulNextNs)
    {
        // Calculate number of elapsed periods
        uint64_t l_ulElapsedNs = l_ulNowNs - l_ulStartNs;
        uint64_t l_ulLastPeriodNs = l_ulNextNs - l_ulStartNs;

        // Calculate complete periods that have passed since last check
        uint64_t l_ulPeriodsSinceLastCheck = (l_ulElapsedNs - l_ulLastPeriodNs) / l_ulPeriodNs + 1;

        // Limit the number of callbacks to prevent DoS attacks
        if (l_ulPeriodsSinceLastCheck > XOS_TIMER_MAX_CALLBACKS)
        {
            X_LOG_TRACE("xTimerProcessElapsedPeriods: Too many periods elapsed (%llu), limiting to %d",
                        l_ulPeriodsSinceLastCheck,
                        XOS_TIMER_MAX_CALLBACKS);
            l_ulPeriodsSinceLastCheck = XOS_TIMER_MAX_CALLBACKS;
        }

        l_iPeriodCount = (int)l_ulPeriodsSinceLastCheck;

        // Callback for each elapsed period with security measures
        for (int i = 0; i < l_iPeriodCount; i++)
        {
            // Temporarily unlock mutex during callback to avoid deadlocks
            l_iResult = mutexUnlock(&p_ptTimer->t_tMutex);
            if (l_iResult != MUTEX_OK)
            {
                X_LOG_TRACE("xTimerProcessElapsedPeriods: Failed to unlock mutex before callback");
                return l_iResult;
            }

            // Execute callback with error handling
            // Note: The callback should be designed to be safe and not cause infinite loops
            p_pfCallback(p_pvData);

            // Re-lock mutex
            l_iResult = mutexLock(&p_ptTimer->t_tMutex);
            if (l_iResult != MUTEX_OK)
            {
                X_LOG_TRACE("xTimerProcessElapsedPeriods: Failed to re-lock mutex after callback");
                return l_iResult;
            }

            // Check if timer was stopped during callback
            if (!p_ptTimer->t_ucActive)
            {
                l_iResult = mutexUnlock(&p_ptTimer->t_tMutex);
                return i + 1; // Return number of callbacks executed
            }
        }

        // Update next trigger time
        if (p_ptTimer->t_ucMode == XOS_TIMER_MODE_PERIODIC)
        {
            uint64_t l_ulPeriodsTotal = l_ulElapsedNs / l_ulPeriodNs + 1;

            // Check for overflow in multiplication
            if (l_ulPeriodsTotal > (UINT64_MAX / l_ulPeriodNs))
            {
                mutexUnlock(&p_ptTimer->t_tMutex);
                X_LOG_TRACE("xTimerProcessElapsedPeriods: Time calculation overflow");
                return XOS_TIMER_ERROR;
            }

            uint64_t l_ulNextTime = l_ulStartNs + (l_ulPeriodsTotal * l_ulPeriodNs);
            p_ptTimer->t_tNext.tv_sec = (time_t)(l_ulNextTime / 1000000000ULL);
            p_ptTimer->t_tNext.tv_nsec = (long)(l_ulNextTime % 1000000000ULL);
        }
        else
        {
            p_ptTimer->t_ucActive = 0;
        }
    }

    // Unlock mutex
    l_iResult = mutexUnlock(&p_ptTimer->t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        X_LOG_TRACE("xTimerProcessElapsedPeriods: Failed to unlock mutex");
        return l_iResult;
    }

    return l_iPeriodCount;
}

////////////////////////////////////////////////////////////
/// xTimerProcessPeriodicCallback
////////////////////////////////////////////////////////////
int xTimerProcessPeriodicCallback(xOsTimerCtx *p_ptTimer, void (*p_pfCallback)(void *), void *p_pvData)
{
    X_ASSERT(p_ptTimer != NULL);
    X_ASSERT(p_pfCallback != NULL);

    int l_iResult;
    int l_iTotalCallbacks = 0;
    int l_iCallbacksThisRound = 0;

    // Check if it's a periodic timer
    l_iResult = mutexLock(&p_ptTimer->t_tMutex);
    if (l_iResult != MUTEX_OK)
    {
        return l_iResult;
    }

    if (p_ptTimer->t_ucMode != XOS_TIMER_MODE_PERIODIC)
    {
        mutexUnlock(&p_ptTimer->t_tMutex);
        return XOS_TIMER_INVALID;
    }

    mutexUnlock(&p_ptTimer->t_tMutex);

    // Optimized loop with blocking absolute wait
    while (atomic_load(&p_ptTimer->t_bPeriodicLockFlag))
    {
        struct timespec l_tNext;
        
        // Get the absolute time of the next trigger
        l_iResult = mutexLock(&p_ptTimer->t_tMutex);
        if (l_iResult != MUTEX_OK)
        {
            return l_iResult;
        }

        if (!p_ptTimer->t_ucActive)
        {
            mutexUnlock(&p_ptTimer->t_tMutex);
            break;
        }

        l_tNext = p_ptTimer->t_tNext;
        mutexUnlock(&p_ptTimer->t_tMutex);

        int l_iSleepResult = clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &l_tNext, NULL);
        
        if (l_iSleepResult != 0 && l_iSleepResult != EINTR)
        {
            X_LOG_TRACE("xTimerProcessPeriodicCallback: clock_nanosleep failed with errno %d", l_iSleepResult);
            return XOS_TIMER_ERROR;
        }

        // Check if the timer is still active
        if (!atomic_load(&p_ptTimer->t_bPeriodicLockFlag))
        {
            break;
        }

        // Process the elapsed periods
        l_iCallbacksThisRound = xTimerProcessElapsedPeriods(p_ptTimer, p_pfCallback, p_pvData);

        if (l_iCallbacksThisRound < 0)
        {
            // Error in the treatment
            return l_iCallbacksThisRound;
        }

        l_iTotalCallbacks += l_iCallbacksThisRound;
    }

    return l_iTotalCallbacks;
}
