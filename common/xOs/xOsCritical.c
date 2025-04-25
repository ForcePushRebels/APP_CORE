////////////////////////////////////////////////////////////
//  os critical src file
//  defines the os function for critical section manipulation
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
////////////////////////////////////////////////////////////

#include "xOsCritical.h"
#include <errno.h>
#include <time.h>


////////////////////////////////////////////////////////////
/// osCriticalCreate
////////////////////////////////////////////////////////////
int osCriticalCreate(t_osCriticalCtx* p_pttOSCritical)
{
    X_ASSERT(p_pttOSCritical != NULL);

    pthread_mutexattr_t l_tAttr;
    pthread_mutexattr_init(&l_tAttr);
    pthread_mutexattr_settype(&l_tAttr, PTHREAD_MUTEX_RECURSIVE);
        

    if (pthread_mutex_init(&p_pttOSCritical->critical, &l_tAttr) != 0) {
        pthread_mutexattr_destroy(&l_tAttr);
        return OS_CRITICAL_ERROR;
    }

    pthread_mutexattr_destroy(&l_tAttr);
	atomic_store(&p_pttOSCritical->a_usLockCounter, 0);
	atomic_store(&p_pttOSCritical->a_bLock, false);

    return OS_CRITICAL_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osCriticalLock
////////////////////////////////////////////////////////////
int osCriticalLock(t_osCriticalCtx* p_pttOSCritical) 
{
    X_ASSERT(p_pttOSCritical != NULL);

    if (pthread_mutex_lock(&p_pttOSCritical->critical) != 0) {
        return OS_CRITICAL_ERROR;
    }

	//increase the counter and update the critical section status 
    atomic_fetch_add(&p_pttOSCritical->a_usLockCounter, 1);
	atomic_store(&p_pttOSCritical->a_bLock, true);
    return OS_CRITICAL_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osCriticalUnlock
////////////////////////////////////////////////////////////
int osCriticalUnlock(t_osCriticalCtx* p_pttOSCritical)
{
    X_ASSERT(p_pttOSCritical != NULL);

	if (atomic_load(&p_pttOSCritical->a_usLockCounter) == 0)
    {
		return OS_CRITICAL_ERROR;
	}

    if (pthread_mutex_unlock(&p_pttOSCritical->critical) != 0) {
        return OS_CRITICAL_ERROR;
    }

	// Decrement the lock counter
    atomic_fetch_sub(&p_pttOSCritical->a_usLockCounter, 1);

	// If the lock counter is 0, set the lock status to false
	if (atomic_load(&p_pttOSCritical->a_usLockCounter) == 0)
	{
		atomic_store(&p_pttOSCritical->a_bLock, false);
	}

    return OS_CRITICAL_SUCCESS;
}

////////////////////////////////////////////////////////////
/// osCriticalDestroy
////////////////////////////////////////////////////////////
int osCriticalDestroy(t_osCriticalCtx* p_pttOSCritical)
{
    X_ASSERT(p_pttOSCritical != NULL);

    // Vérifier que le compteur de verrou est à zéro
    if (atomic_load(&p_pttOSCritical->a_usLockCounter) != 0)
    {
        // Un ou plusieurs verrous sont toujours en place
        return OS_CRITICAL_ERROR;
    }

    // Vérifier que le flag du verrou indique bien que le mutex n'est pas verrouillé
    if (atomic_load(&p_pttOSCritical->a_bLock))
    {
        return OS_CRITICAL_ERROR;
    }

    // À ce stade, on considère que le mutex n'est plus en cours d'utilisation
    if (pthread_mutex_destroy(&p_pttOSCritical->critical) != 0)
    {
        return OS_CRITICAL_ERROR;
    }

    return OS_CRITICAL_SUCCESS;
}
