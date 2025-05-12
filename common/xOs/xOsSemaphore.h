////////////////////////////////////////////////////////////
// os Semaphore header file
// defines the os function for semaphore manipulation
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////

#pragma once
#ifndef OS_SEMAPHORE_H_
#define OS_SEMAPHORE_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <semaphore.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <time.h>
#include <errno.h>
#include "xAssert.h"

// Codes de retour
#define OS_SEM_SUCCESS       0xA7D64C80
#define OS_SEM_ERROR         0xA7D64C81
#define OS_SEM_TIMEOUT       0xA7D64C82
#define OS_SEM_NOT_AVAILABLE 0xA7D64C83

// États du sémaphore
#define OS_SEM_UNLOCKED      0
#define OS_SEM_LOCKED        1

//////////////////////////////////
// Structure de gestion du sémaphore
//////////////////////////////////
typedef struct {
    sem_t       sem;        // Sémaphore POSIX (pour sémaphores anonymes)
    sem_t*      sem_handle; // Pointeur vers le sémaphore (pour sémaphores nommés)
    const char* name;       // Nom du sémaphore (NULL pour les sémaphores anonymes)
    int         value;      // Valeur actuelle du sémaphore
    int         initialized; // Flag indiquant si le sémaphore est initialisé
    int         is_named;   // Flag indiquant s'il s'agit d'un sémaphore nommé
} t_OSSemCtx;

//////////////////////////////////
/// @brief Initialize a semaphore with default values
/// @param p_pttOSSem : pointer to the semaphore structure context
/// @param p_iInitValue : initial value of the semaphore (>=0)
/// @param p_pcName : name of the semaphore (NULL for anonymous semaphore)
/// @return OS_SEM_SUCCESS if success, OS_SEM_ERROR otherwise
//////////////////////////////////
int osSemInit(t_OSSemCtx* p_pttOSSem, int p_iInitValue, const char* p_pcName);

//////////////////////////////////
/// @brief Destroy a semaphore
/// @param p_pttOSSem : pointer to the semaphore structure context
/// @return OS_SEM_SUCCESS if success, OS_SEM_ERROR otherwise
//////////////////////////////////
int osSemDestroy(t_OSSemCtx* p_pttOSSem);

//////////////////////////////////
/// @brief Wait for a semaphore (P operation / decrement)
/// @param p_pttOSSem : pointer to the semaphore structure context
/// @return OS_SEM_SUCCESS if success, OS_SEM_ERROR otherwise
/// @note This function blocks until the semaphore is available
//////////////////////////////////
int osSemWait(t_OSSemCtx* p_pttOSSem);

//////////////////////////////////
/// @brief Wait for a semaphore with timeout (P operation / decrement)
/// @param p_pttOSSem : pointer to the semaphore structure context
/// @param p_ulTimeoutMs : timeout in milliseconds
/// @return OS_SEM_SUCCESS if success, OS_SEM_TIMEOUT if timeout, OS_SEM_ERROR otherwise
//////////////////////////////////
int osSemWaitTimeout(t_OSSemCtx* p_pttOSSem, unsigned long p_ulTimeoutMs);

//////////////////////////////////
/// @brief Try to wait for a semaphore (P operation / decrement)
/// @param p_pttOSSem : pointer to the semaphore structure context
/// @return OS_SEM_SUCCESS if success, OS_SEM_NOT_AVAILABLE if semaphore not available, OS_SEM_ERROR otherwise
/// @note This function does not block if the semaphore is not available
//////////////////////////////////
int osSemTryWait(t_OSSemCtx* p_pttOSSem);

//////////////////////////////////
/// @brief Post a semaphore (V operation / increment)
/// @param p_pttOSSem : pointer to the semaphore structure context
/// @return OS_SEM_SUCCESS if success, OS_SEM_ERROR otherwise
//////////////////////////////////
int osSemPost(t_OSSemCtx* p_pttOSSem);

//////////////////////////////////
/// @brief Get the value of a semaphore
/// @param p_pttOSSem : pointer to the semaphore structure context
/// @param p_piValue : pointer to store the value of the semaphore
/// @return OS_SEM_SUCCESS if success, OS_SEM_ERROR otherwise
//////////////////////////////////
int osSemGetValue(t_OSSemCtx* p_pttOSSem, int* p_piValue);

#endif // OS_SEMAPHORE_H_
