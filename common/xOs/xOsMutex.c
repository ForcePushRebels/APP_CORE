////////////////////////////////////////////////////////////
//  mutex source file
//  implements mutex functions
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
////////////////////////////////////////////////////////////

#include "xOsMutex.h"
#include "xAssert.h"
#include <string.h>

////////////////////////////////////////////////////////////
/// mutexCreate
////////////////////////////////////////////////////////////
int mutexCreate(t_MutexCtx *p_ptMutex)
{
    X_ASSERT(p_ptMutex != NULL);

    pthread_mutexattr_t l_tAttr;
    pthread_mutexattr_init(&l_tAttr);
    int result = pthread_mutexattr_settype(&l_tAttr, PTHREAD_MUTEX_RECURSIVE);

    if (pthread_mutex_init(&p_ptMutex->t_mutex, &l_tAttr) != 0)
    {
        pthread_mutexattr_destroy(&l_tAttr);
        return MUTEX_ERROR;
    }

    if (result != 0)
    {
        pthread_mutexattr_destroy(&l_tAttr);
        return MUTEX_ERROR;
    }

    p_ptMutex->t_iState = MUTEX_UNLOCKED;
    p_ptMutex->t_ulTimeout = MUTEX_DEFAULT_TIMEOUT;

    return MUTEX_OK;
}

////////////////////////////////////////////////////////////
/// mutexLock
////////////////////////////////////////////////////////////
int mutexLock(t_MutexCtx *p_ptMutex)
{
    X_ASSERT(p_ptMutex != NULL);

    if (pthread_mutex_lock(&p_ptMutex->t_mutex) != 0)
    {
        return MUTEX_ERROR;
    }

    p_ptMutex->t_iState = MUTEX_LOCKED;
    return MUTEX_OK;
}

////////////////////////////////////////////////////////////
/// mutexTryLock
////////////////////////////////////////////////////////////
int mutexTryLock(t_MutexCtx *p_ptMutex)
{
    X_ASSERT(p_ptMutex != NULL);

    int l_iResult = pthread_mutex_trylock(&p_ptMutex->t_mutex);
    if (l_iResult == EBUSY)
    {
        return MUTEX_TIMEOUT;
    }
    if (l_iResult != 0)
    {
        return MUTEX_ERROR;
    }

    p_ptMutex->t_iState = MUTEX_LOCKED;
    return MUTEX_OK;
}

////////////////////////////////////////////////////////////
/// mutexLockTimeout
////////////////////////////////////////////////////////////
int mutexLockTimeout(t_MutexCtx *p_ptMutex, unsigned long p_ulTimeout)
{
    X_ASSERT(p_ptMutex != NULL);

    struct timespec l_tTimeout;
    clock_gettime(CLOCK_REALTIME, &l_tTimeout);
    l_tTimeout.tv_sec += p_ulTimeout / 1000;
    l_tTimeout.tv_nsec += (p_ulTimeout % 1000) * 1000000;

    int l_iResult = pthread_mutex_timedlock(&p_ptMutex->t_mutex, &l_tTimeout);
    if (l_iResult == ETIMEDOUT)
    {
        return MUTEX_TIMEOUT;
    }
    if (l_iResult != 0)
    {
        return MUTEX_ERROR;
    }

    p_ptMutex->t_iState = MUTEX_LOCKED;
    return MUTEX_OK;
}

////////////////////////////////////////////////////////////
/// mutexUnlock
////////////////////////////////////////////////////////////
int mutexUnlock(t_MutexCtx *p_ptMutex)
{
    X_ASSERT(p_ptMutex != NULL);

    if (pthread_mutex_unlock(&p_ptMutex->t_mutex) != 0)
    {
        return MUTEX_ERROR;
    }

    p_ptMutex->t_iState = MUTEX_UNLOCKED;
    return MUTEX_OK;
}

////////////////////////////////////////////////////////////
/// mutexDestroy
////////////////////////////////////////////////////////////
int mutexDestroy(t_MutexCtx *p_ptMutex)
{
    X_ASSERT(p_ptMutex != NULL);

    if (pthread_mutex_destroy(&p_ptMutex->t_mutex) != 0)
    {
        return MUTEX_ERROR;
    }

    p_ptMutex->t_iState = MUTEX_UNLOCKED;
    return MUTEX_OK;
}

////////////////////////////////////////////////////////////
/// mutexSetTimeout
////////////////////////////////////////////////////////////
int mutexSetTimeout(t_MutexCtx *p_ptMutex, unsigned long p_ulTimeout)
{
    X_ASSERT(p_ptMutex != NULL);

    p_ptMutex->t_ulTimeout = p_ulTimeout;
    return MUTEX_OK;
}

////////////////////////////////////////////////////////////
/// mutexGetState
////////////////////////////////////////////////////////////
int mutexGetState(t_MutexCtx *p_ptMutex)
{
    X_ASSERT(p_ptMutex != NULL);

    return p_ptMutex->t_iState;
}
