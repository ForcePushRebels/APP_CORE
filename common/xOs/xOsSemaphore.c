////////////////////////////////////////////////////////////
// os Semaphore src file
// defines the os function for semaphore manipulation
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////



#include "xOsSemaphore.h"
#include <time.h>  // For CLOCK_REALTIME

////////////////////////////////////////////////////////////
/// osSemInit
////////////////////////////////////////////////////////////
int osSemInit(t_OSSemCtx* p_pttOSSem, int p_iInitValue, const char* p_pcName)
{
    if (p_pttOSSem == NULL || p_iInitValue < 0) {
        return OS_SEM_ERROR;
    }

    // Reset the structure
    memset(p_pttOSSem, 0, sizeof(t_OSSemCtx));
    
    p_pttOSSem->value = p_iInitValue;
    p_pttOSSem->name = p_pcName;
    
    // Create the semaphore
    if (p_pcName != NULL) {
        // Named semaphore
        sem_t* namedSem = sem_open(p_pcName, O_CREAT, 0666, p_iInitValue);
        if (namedSem == SEM_FAILED) {
            return OS_SEM_ERROR;
        }
        // For named semaphores, store the pointer returned by sem_open
        p_pttOSSem->sem_handle = namedSem;
        p_pttOSSem->is_named = 1;
    } else {
        // Anonymous semaphore
        if (sem_init(&p_pttOSSem->sem, 0, p_iInitValue) != 0) {
            return OS_SEM_ERROR;
        }
        p_pttOSSem->is_named = 0;
    }
    
    p_pttOSSem->initialized = 1;
    return OS_SEM_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osSemDestroy
////////////////////////////////////////////////////////////
int osSemDestroy(t_OSSemCtx* p_pttOSSem)
{
    if (p_pttOSSem == NULL || !p_pttOSSem->initialized) {
        return OS_SEM_ERROR;
    }
    
    int result;
    if (p_pttOSSem->is_named) {
        // Named semaphore
        result = sem_close(p_pttOSSem->sem_handle);
        if (result == 0 && p_pttOSSem->name != NULL) {
            // Remove the named semaphore from the system if we are the last user
            sem_unlink(p_pttOSSem->name);
        }
    } else {
        // Anonymous semaphore
        result = sem_destroy(&p_pttOSSem->sem);
    }
    
    if (result != 0) {
        return OS_SEM_ERROR;
    }
    
    p_pttOSSem->initialized = 0;
    return OS_SEM_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osSemWait
////////////////////////////////////////////////////////////
int osSemWait(t_OSSemCtx* p_pttOSSem)
{
    if (p_pttOSSem == NULL || !p_pttOSSem->initialized) {
        return OS_SEM_ERROR;
    }
    
    int result;
    if (p_pttOSSem->is_named) {
        result = sem_wait(p_pttOSSem->sem_handle);
    } else {
        result = sem_wait(&p_pttOSSem->sem);
    }
    
    if (result != 0) {
        return OS_SEM_ERROR;
    }
    
    // Update the value (approximate, as there may be concurrent accesses)
    osSemGetValue(p_pttOSSem, &p_pttOSSem->value);
    
    return OS_SEM_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osSemWaitTimeout
////////////////////////////////////////////////////////////
int osSemWaitTimeout(t_OSSemCtx* p_pttOSSem, unsigned long p_ulTimeoutMs)
{
    if (p_pttOSSem == NULL || !p_pttOSSem->initialized) {
        return OS_SEM_ERROR;
    }
    
    struct timespec ts;
    if (clock_gettime(CLOCK_REALTIME, &ts) == -1) {
        return OS_SEM_ERROR;
    }
    
    // Convert milliseconds to seconds and nanoseconds
    ts.tv_sec += p_ulTimeoutMs / 1000;
    ts.tv_nsec += (p_ulTimeoutMs % 1000) * 1000000;
    
    // Normalize nanoseconds (ensure they remain < 1 second)
    if (ts.tv_nsec >= 1000000000) {
        ts.tv_sec += 1;
        ts.tv_nsec -= 1000000000;
    }
    
    int ret;
    if (p_pttOSSem->is_named) {
        ret = sem_timedwait(p_pttOSSem->sem_handle, &ts);
    } else {
        ret = sem_timedwait(&p_pttOSSem->sem, &ts);
    }
    
    if (ret != 0) {
        if (errno == ETIMEDOUT) {
            return OS_SEM_TIMEOUT;
        }
        return OS_SEM_ERROR;
    }
    
    // Update the value (approximate)
    osSemGetValue(p_pttOSSem, &p_pttOSSem->value);
    
    return OS_SEM_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osSemTryWait
////////////////////////////////////////////////////////////
int osSemTryWait(t_OSSemCtx* p_pttOSSem)
{
    if (p_pttOSSem == NULL || !p_pttOSSem->initialized) {
        return OS_SEM_ERROR;
    }
    
    int ret;
    if (p_pttOSSem->is_named) {
        ret = sem_trywait(p_pttOSSem->sem_handle);
    } else {
        ret = sem_trywait(&p_pttOSSem->sem);
    }
    
    if (ret != 0) {
        if (errno == EAGAIN) {
            return OS_SEM_NOT_AVAILABLE;
        }
        return OS_SEM_ERROR;
    }
    
    // Update the value (approximate)
    osSemGetValue(p_pttOSSem, &p_pttOSSem->value);
    
    return OS_SEM_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osSemPost
////////////////////////////////////////////////////////////
int osSemPost(t_OSSemCtx* p_pttOSSem)
{
    if (p_pttOSSem == NULL || !p_pttOSSem->initialized) {
        return OS_SEM_ERROR;
    }
    
    int result;
    if (p_pttOSSem->is_named) {
        result = sem_post(p_pttOSSem->sem_handle);
    } else {
        result = sem_post(&p_pttOSSem->sem);
    }
    
    if (result != 0) {
        return OS_SEM_ERROR;
    }
    
    // Update the value (approximate)
    osSemGetValue(p_pttOSSem, &p_pttOSSem->value);
    
    return OS_SEM_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osSemGetValue
////////////////////////////////////////////////////////////
int osSemGetValue(t_OSSemCtx* p_pttOSSem, int* p_piValue)
{
    if (p_pttOSSem == NULL || !p_pttOSSem->initialized || p_piValue == NULL) {
        return OS_SEM_ERROR;
    }
    
    int result;
    if (p_pttOSSem->is_named) {
        result = sem_getvalue(p_pttOSSem->sem_handle, p_piValue);
    } else {
        result = sem_getvalue(&p_pttOSSem->sem, p_piValue);
    }
    
    if (result != 0) {
        return OS_SEM_ERROR;
    }
    
    return OS_SEM_SUCCESS;
}
