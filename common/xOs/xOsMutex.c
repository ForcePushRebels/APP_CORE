////////////////////////////////////////////////////////////
//  mutex source file 
//  implements mutex functions
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
// Modified: 28/05/2025 - Security improvements
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////

#include "xOsMutex.h"
#include "xAssert.h"
#include <string.h>
#include <errno.h>
#include <time.h>

// Forward declarations for helper functions
static int validateTimeout(unsigned long p_ulTimeout);
static int validateMutexState(const xOsMutexCtx *p_ptMutex);

////////////////////////////////////////////////////////////
/// mutexCreate
////////////////////////////////////////////////////////////
int mutexCreate(xOsMutexCtx *p_ptMutex)
{
    X_ASSERT(p_ptMutex != NULL);

    // Initialize structure with secure defaults
    XOS_MEMORY_SANITIZE(p_ptMutex, sizeof(xOsMutexCtx));

    pthread_mutexattr_t l_tAttr;
    if (pthread_mutexattr_init(&l_tAttr) != 0)
    {
        return MUTEX_ERROR;
    }
    
    int l_iResult = pthread_mutexattr_settype(&l_tAttr, PTHREAD_MUTEX_RECURSIVE);
    if (l_iResult != 0)
    {
        pthread_mutexattr_destroy(&l_tAttr);
        return MUTEX_ERROR;
    }

    if (pthread_mutex_init(&p_ptMutex->t_mutex, &l_tAttr) != 0)
    {
        pthread_mutexattr_destroy(&l_tAttr);
        return MUTEX_ERROR;
    }

    // Clean up attributes immediately after use
    pthread_mutexattr_destroy(&l_tAttr);

    // Initialize atomic variables with secure defaults (relaxed for initialization)
    atomic_init(&p_ptMutex->a_iState, MUTEX_UNLOCKED);
    atomic_init(&p_ptMutex->a_ulTimeout, MUTEX_DEFAULT_TIMEOUT);

    return MUTEX_OK;
}

////////////////////////////////////////////////////////////
/// mutexLock
////////////////////////////////////////////////////////////
int mutexLock(xOsMutexCtx *p_ptMutex)
{
    X_ASSERT(p_ptMutex != NULL);

    // Validate mutex state
    int l_iValidation = validateMutexState(p_ptMutex);
    if (l_iValidation != MUTEX_OK)
    {
        return l_iValidation;
    }

    if (pthread_mutex_lock(&p_ptMutex->t_mutex) != 0)
    {
        return MUTEX_ERROR;
    }

    // Atomically update state with release ordering for synchronization
    atomic_store_explicit(&p_ptMutex->a_iState, MUTEX_LOCKED, memory_order_release);
    return MUTEX_OK;
}

////////////////////////////////////////////////////////////
/// mutexTryLock
////////////////////////////////////////////////////////////
int mutexTryLock(xOsMutexCtx *p_ptMutex)
{
    X_ASSERT(p_ptMutex != NULL);

    // Validate mutex state
    int l_iValidation = validateMutexState(p_ptMutex);
    if (l_iValidation != MUTEX_OK)
    {
        return l_iValidation;
    }

    int l_ulReturn = pthread_mutex_trylock(&p_ptMutex->t_mutex);
    if (l_ulReturn == EBUSY)
    {
        return MUTEX_TIMEOUT;
    }
    if (l_ulReturn != 0)
    {
        return MUTEX_ERROR;
    }

    // Atomically update state with release ordering for synchronization
    atomic_store_explicit(&p_ptMutex->a_iState, MUTEX_LOCKED, memory_order_release);
    return MUTEX_OK;
}

////////////////////////////////////////////////////////////
/// mutexLockTimeout
////////////////////////////////////////////////////////////
int mutexLockTimeout(xOsMutexCtx *p_ptMutex, unsigned long p_ulTimeout)
{
    X_ASSERT(p_ptMutex != NULL);

    // Validate mutex state
    int l_iValidation = validateMutexState(p_ptMutex);
    if (l_iValidation != MUTEX_OK)
    {
        return l_iValidation;
    }

    // Validate timeout with fixed bounds
    int l_iTimeoutValidation = validateTimeout(p_ulTimeout);
    if (l_iTimeoutValidation != MUTEX_OK)
    {
        return l_iTimeoutValidation;
    }

    struct timespec l_tTimeout;
    if (clock_gettime(CLOCK_MONOTONIC, &l_tTimeout) != 0)
    {
        return MUTEX_ERROR;
    }
    
    // Single dereference - calculate timeout once
    unsigned long l_ulTimeoutMs = p_ulTimeout;
    l_tTimeout.tv_sec += l_ulTimeoutMs / 1000;
    l_tTimeout.tv_nsec += (l_ulTimeoutMs % 1000) * 1000000;
    
    // Normalize nanoseconds if they exceed 1 second
    if (l_tTimeout.tv_nsec >= 1000000000L)
    {
        l_tTimeout.tv_sec += 1;
        l_tTimeout.tv_nsec -= 1000000000L;
    }

    int l_ulReturn = pthread_mutex_timedlock(&p_ptMutex->t_mutex, &l_tTimeout);
    if (l_ulReturn == ETIMEDOUT)
    {
        return MUTEX_TIMEOUT;
    }
    if (l_ulReturn != 0)
    {
        return MUTEX_ERROR;
    }

    // Atomically update state with release ordering for synchronization
    atomic_store_explicit(&p_ptMutex->a_iState, MUTEX_LOCKED, memory_order_release);
    return MUTEX_OK;
}

////////////////////////////////////////////////////////////
/// mutexUnlock
////////////////////////////////////////////////////////////
int mutexUnlock(xOsMutexCtx *p_ptMutex)
{
    X_ASSERT(p_ptMutex != NULL);

    // Validate mutex state
    int l_iValidation = validateMutexState(p_ptMutex);
    if (l_iValidation != MUTEX_OK)
    {
        return l_iValidation;
    }

    if (pthread_mutex_unlock(&p_ptMutex->t_mutex) != 0)
    {
        return MUTEX_ERROR;
    }

    // Atomically update state with release ordering for synchronization
    atomic_store_explicit(&p_ptMutex->a_iState, MUTEX_UNLOCKED, memory_order_release);
    return MUTEX_OK;
}

////////////////////////////////////////////////////////////
/// mutexDestroy
////////////////////////////////////////////////////////////
int mutexDestroy(xOsMutexCtx *p_ptMutex)
{
    X_ASSERT(p_ptMutex != NULL);

    // Validate mutex state
    int l_iValidation = validateMutexState(p_ptMutex);
    if (l_iValidation != MUTEX_OK)
    {
        return l_iValidation;
    }

    // Atomically load state with acquire ordering for proper synchronization
    int l_iCurrentState = atomic_load_explicit(&p_ptMutex->a_iState, memory_order_acquire);
    if (l_iCurrentState == MUTEX_LOCKED)
    {
        return MUTEX_ERROR; // Cannot destroy locked mutex
    }

    if (pthread_mutex_destroy(&p_ptMutex->t_mutex) != 0)
    {
        return MUTEX_ERROR;
    }

    // Securely clear the structure
    XOS_MEMORY_SANITIZE(p_ptMutex, sizeof(xOsMutexCtx));
    
    // Re-initialize atomic variables after clearing (relaxed for cleanup)
    atomic_init(&p_ptMutex->a_iState, MUTEX_UNLOCKED);
    atomic_init(&p_ptMutex->a_ulTimeout, 0);
    
    return MUTEX_OK;
}

////////////////////////////////////////////////////////////
/// mutexSetTimeout
////////////////////////////////////////////////////////////
int mutexSetTimeout(xOsMutexCtx *p_ptMutex, unsigned long p_ulTimeout)
{
    X_ASSERT(p_ptMutex != NULL);

    // Validate mutex state
    int l_iValidation = validateMutexState(p_ptMutex);
    if (l_iValidation != MUTEX_OK)
    {
        return l_iValidation;
    }

    // Validate timeout with fixed bounds
    int l_iTimeoutValidation = validateTimeout(p_ulTimeout);
    if (l_iTimeoutValidation != MUTEX_OK)
    {
        return l_iTimeoutValidation;
    }

    // Atomically store timeout with relaxed ordering (not critical for synchronization)
    atomic_store_explicit(&p_ptMutex->a_ulTimeout, p_ulTimeout, memory_order_relaxed);
    return MUTEX_OK;
}

////////////////////////////////////////////////////////////
/// mutexGetState
////////////////////////////////////////////////////////////
int mutexGetState(xOsMutexCtx *p_ptMutex)
{
    X_ASSERT(p_ptMutex != NULL);

    // Validate mutex state
    int l_iValidation = validateMutexState(p_ptMutex);
    if (l_iValidation != MUTEX_OK)
    {
        return l_iValidation;
    }

    // Atomically load state with acquire ordering for proper synchronization
    return atomic_load_explicit(&p_ptMutex->a_iState, memory_order_acquire);
}

////////////////////////////////////////////////////////////
/// mutexGetTimeout
////////////////////////////////////////////////////////////
unsigned long mutexGetTimeout(xOsMutexCtx *p_ptMutex)
{
    X_ASSERT(p_ptMutex != NULL);

    // Validate mutex state
    int l_iValidation = validateMutexState(p_ptMutex);
    if (l_iValidation != MUTEX_OK)
    {
        return 0; // Return 0 on error (safe default)
    }

    // Atomically load timeout with relaxed ordering (not critical for synchronization)
    return atomic_load_explicit(&p_ptMutex->a_ulTimeout, memory_order_relaxed);
}

////////////////////////////////////////////////////////////
/// validateTimeout
////////////////////////////////////////////////////////////
static int validateTimeout(unsigned long p_ulTimeout)
{
    // Fixed upper bounds validation
    if (p_ulTimeout > MUTEX_MAX_TIMEOUT_MS)
    {
        return MUTEX_INVALID;
    }
    
    return MUTEX_OK;
}

////////////////////////////////////////////////////////////
/// validateMutexState
////////////////////////////////////////////////////////////
static int validateMutexState(const xOsMutexCtx *p_ptMutex)
{
    X_ASSERT(p_ptMutex != NULL);
    
    if (p_ptMutex == NULL)
    {
        return MUTEX_INVALID;
    }
    
    return MUTEX_OK;
}
