////////////////////////////////////////////////////////////
//  timer source file
//  implements the timer functions
//
// general discloser: copy or share the file is forbidden
// Written : 12/01/2025
// Modified: 12/05/2025 - Improved thread safety and timing precision
////////////////////////////////////////////////////////////

#include "xTimer.h"
#include "xAssert.h"
#include "xLog.h"
#include <string.h>
#include <errno.h>

////////////////////////////////////////////////////////////
/// xTimerCreate
////////////////////////////////////////////////////////////
int xTimerCreate(xOsTimerCtx *p_ptTimer, uint32_t p_ulPeriod, uint8_t p_ucMode)
{
    X_ASSERT(p_ptTimer != NULL);
    X_ASSERT(p_ulPeriod > 0);
    X_ASSERT(p_ucMode <= XOS_TIMER_MODE_PERIODIC);

    // Clear the structure
    memset(p_ptTimer, 0, sizeof(xOsTimerCtx));

    // Initialize timer parameters
    p_ptTimer->t_ulPeriod = p_ulPeriod;
    p_ptTimer->t_ucMode = p_ucMode;
    p_ptTimer->t_ucActive = 0;

    // Initialize mutex for thread safety
    unsigned long l_ulResult = mutexCreate(&p_ptTimer->t_tMutex);
    if (l_ulResult != MUTEX_OK)
    {
        return XOS_TIMER_MUTEX_ERROR;
    }

    return XOS_TIMER_OK;
}

////////////////////////////////////////////////////////////
/// xTimerStart
////////////////////////////////////////////////////////////
int xTimerStart(xOsTimerCtx *p_ptTimer)
{
    X_ASSERT(p_ptTimer != NULL);

    unsigned long l_ulResult;

    // Lock mutex for thread safety
    l_ulResult = mutexLock(&p_ptTimer->t_tMutex);
    if (l_ulResult != MUTEX_OK)
    {
        return XOS_TIMER_MUTEX_ERROR;
    }

    // Get current time
    clock_gettime(CLOCK_MONOTONIC, &p_ptTimer->t_tStart);

    // Convert period from milliseconds to nanoseconds
    uint64_t l_ulPeriodNs = (uint64_t)p_ptTimer->t_ulPeriod * 1000000ULL;

    // Calculate next trigger time
    p_ptTimer->t_tNext = p_ptTimer->t_tStart;
    uint64_t l_ulNextNs = (uint64_t)p_ptTimer->t_tNext.tv_nsec + l_ulPeriodNs;

    p_ptTimer->t_tNext.tv_sec += l_ulNextNs / 1000000000ULL;
    p_ptTimer->t_tNext.tv_nsec = l_ulNextNs % 1000000000ULL;

    p_ptTimer->t_ucActive = 1;

    // Unlock mutex
    l_ulResult = mutexUnlock(&p_ptTimer->t_tMutex);
    if (l_ulResult != MUTEX_OK)
    {
        return XOS_TIMER_MUTEX_ERROR;
    }

    return XOS_TIMER_OK;
}

////////////////////////////////////////////////////////////
/// xTimerStop
////////////////////////////////////////////////////////////
int xTimerStop(xOsTimerCtx *p_ptTimer)
{
    X_ASSERT(p_ptTimer != NULL);

    unsigned long l_ulResult;

    // Lock mutex for thread safety
    l_ulResult = mutexLock(&p_ptTimer->t_tMutex);
    if (l_ulResult != MUTEX_OK)
    {
        return XOS_TIMER_MUTEX_ERROR;
    }

    p_ptTimer->t_ucActive = 0;

    // Unlock mutex
    l_ulResult = mutexUnlock(&p_ptTimer->t_tMutex);
    if (l_ulResult != MUTEX_OK)
    {
        return XOS_TIMER_MUTEX_ERROR;
    }

    return XOS_TIMER_OK;
}

////////////////////////////////////////////////////////////
/// xTimerExpired
////////////////////////////////////////////////////////////
int xTimerExpired(xOsTimerCtx *p_ptTimer)
{
    X_ASSERT(p_ptTimer != NULL);

    unsigned long l_ulResult;
    unsigned long l_ulReturn;

    // Lock mutex for thread safety
    l_ulResult = mutexLock(&p_ptTimer->t_tMutex);
    if (l_ulResult != MUTEX_OK)
    {
        return XOS_TIMER_MUTEX_ERROR;
    }

    if (!p_ptTimer->t_ucActive)
    {
        // Unlock mutex before return
        mutexUnlock(&p_ptTimer->t_tMutex);
        return XOS_TIMER_NOT_INIT;
    }

    struct timespec l_tNow;
    clock_gettime(CLOCK_MONOTONIC, &l_tNow);

    // Convert times to nanoseconds for comparison
    uint64_t l_ulNowNs = (uint64_t)l_tNow.tv_sec * 1000000000ULL + l_tNow.tv_nsec;
    uint64_t l_ulNextNs = (uint64_t)p_ptTimer->t_tNext.tv_sec * 1000000000ULL + p_ptTimer->t_tNext.tv_nsec;

    if (l_ulNowNs >= l_ulNextNs)
    {
        if (p_ptTimer->t_ucMode == XOS_TIMER_MODE_PERIODIC)
        {
            // Calculate number of elapsed periods
            uint64_t l_ulStartNs = (uint64_t)p_ptTimer->t_tStart.tv_sec * 1000000000ULL + p_ptTimer->t_tStart.tv_nsec;
            uint64_t l_ulElapsedNs = l_ulNowNs - l_ulStartNs;
            uint64_t l_ulPeriodNs = (uint64_t)p_ptTimer->t_ulPeriod * 1000000ULL;
            uint64_t l_ulPeriods = (l_ulElapsedNs / l_ulPeriodNs) + 1;

            // Calculate next trigger time
            uint64_t l_ulNextTime = l_ulStartNs + (l_ulPeriods * l_ulPeriodNs);
            p_ptTimer->t_tNext.tv_sec = l_ulNextTime / 1000000000ULL;
            p_ptTimer->t_tNext.tv_nsec = l_ulNextTime % 1000000000ULL;
        }
        else
        {
            p_ptTimer->t_ucActive = 0;
        }
        l_ulReturn = XOS_TIMER_OK;
    }
    else
    {
        l_ulReturn = XOS_TIMER_TIMEOUT;
    }

    // Unlock mutex
    l_ulResult = mutexUnlock(&p_ptTimer->t_tMutex);
    if (l_ulResult != MUTEX_OK)
    {
        return XOS_TIMER_MUTEX_ERROR;
    }

    return l_ulReturn;
}

////////////////////////////////////////////////////////////
/// xTimerGetCurrentMs
////////////////////////////////////////////////////////////
uint64_t xTimerGetCurrentMs(void)
{
    struct timespec l_tNow;
    clock_gettime(CLOCK_MONOTONIC, &l_tNow);
    return (uint64_t)((l_tNow.tv_sec * 1000ULL) + (l_tNow.tv_nsec / 1000000ULL));
}

////////////////////////////////////////////////////////////
/// xTimerDelay
////////////////////////////////////////////////////////////
int xTimerDelay(uint32_t p_ulDelay)
{
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

    unsigned long l_ulResult;
    int l_iPeriodCount = 0;

    // Lock mutex for thread safety
    l_ulResult = mutexLock(&p_ptTimer->t_tMutex);
    if (l_ulResult != MUTEX_OK)
    {
        return XOS_TIMER_MUTEX_ERROR;
    }

    if (!p_ptTimer->t_ucActive)
    {
        // Unlock mutex before return
        mutexUnlock(&p_ptTimer->t_tMutex);
        return XOS_TIMER_NOT_INIT;
    }

    struct timespec l_tNow;
    clock_gettime(CLOCK_MONOTONIC, &l_tNow);

    // Convert times to nanoseconds for calculation
    uint64_t l_ulNowNs = (uint64_t)l_tNow.tv_sec * 1000000000ULL + l_tNow.tv_nsec;
    uint64_t l_ulStartNs = (uint64_t)p_ptTimer->t_tStart.tv_sec * 1000000000ULL + p_ptTimer->t_tStart.tv_nsec;
    uint64_t l_ulNextNs = (uint64_t)p_ptTimer->t_tNext.tv_sec * 1000000000ULL + p_ptTimer->t_tNext.tv_nsec;
    uint64_t l_ulPeriodNs = (uint64_t)p_ptTimer->t_ulPeriod * 1000000ULL;

    if (l_ulNowNs >= l_ulNextNs)
    {
        // Calculate number of elapsed periods
        uint64_t l_ulElapsedNs = l_ulNowNs - l_ulStartNs;
        uint64_t l_ulLastPeriodNs = ((uint64_t)p_ptTimer->t_tNext.tv_sec * 1000000000ULL +
                                     p_ptTimer->t_tNext.tv_nsec) -
                                    l_ulStartNs;

        // Calculate complete periods that have passed since last check
        l_iPeriodCount = (l_ulElapsedNs - l_ulLastPeriodNs) / l_ulPeriodNs + 1;

        // Callback for each elapsed period
        for (int i = 0; i < l_iPeriodCount; i++)
        {
            // Temporarily unlock mutex during callback to avoid deadlocks
            l_ulResult = mutexUnlock(&p_ptTimer->t_tMutex);
            if (l_ulResult != MUTEX_OK)
            {
                return XOS_TIMER_MUTEX_ERROR;
            }

            // Execute callback
            p_pfCallback(p_pvData);

            // Re-lock mutex
            l_ulResult = mutexLock(&p_ptTimer->t_tMutex);
            if (l_ulResult != MUTEX_OK)
            {
                return XOS_TIMER_MUTEX_ERROR;
            }

            // Check if timer was stopped during callback
            if (!p_ptTimer->t_ucActive)
            {
                l_ulResult = mutexUnlock(&p_ptTimer->t_tMutex);
                return l_iPeriodCount;
            }
        }

        // Update next trigger time
        if (p_ptTimer->t_ucMode == XOS_TIMER_MODE_PERIODIC)
        {
            uint64_t l_ulNextTime = l_ulStartNs + ((l_ulElapsedNs / l_ulPeriodNs) + 1) * l_ulPeriodNs;
            p_ptTimer->t_tNext.tv_sec = l_ulNextTime / 1000000000ULL;
            p_ptTimer->t_tNext.tv_nsec = l_ulNextTime % 1000000000ULL;
        }
        else
        {
            p_ptTimer->t_ucActive = 0;
        }
    }

    // Unlock mutex
    l_ulResult = mutexUnlock(&p_ptTimer->t_tMutex);
    if (l_ulResult != MUTEX_OK)
    {
        return XOS_TIMER_MUTEX_ERROR;
    }

    return l_iPeriodCount;
}
