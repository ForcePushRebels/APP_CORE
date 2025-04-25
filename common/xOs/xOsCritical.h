////////////////////////////////////////////////////////////
//  os critical header file
//  defines the os function for critical section manipulation
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
////////////////////////////////////////////////////////////
#pragma once

#ifndef OS_CRITICAL_H_
#define OS_CRITICAL_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include "xAssert.h"
#include <pthread.h>
#include <sys/time.h>
#define _POSIX_C_SOURCE 200809L

#ifdef __cplusplus
#include <atomic>   //only for test lib and C++ 20
#else
#include <stdatomic.h>
#endif



#define OS_CRITICAL_SUCCESS 0
#define OS_CRITICAL_ERROR -1

#define OS_CRITICAL_LOCKED 0
#define OS_CRITICAL_UNLOCKED 1


//////////////////////////////////
/// @brief critical struct creation
/// @param atomic lock critical lock status
/// @param critical critical section
/// @param atomic lock counter critical lock counter
/// @note the structure is used for all the management of the critical section
/// @note the critical section is used to protect the shared resources
/// 
/// @pre none
//////////////////////////////////
typedef struct {
    pthread_mutex_t critical; // section critique
#ifdef __cplusplus
    std::atomic<unsigned short> a_usLockCounter; // compteur de verrous
    std::atomic<bool> a_bLock;                  // état du verrou
#else
    _Atomic unsigned short a_usLockCounter;     // compteur de verrous
    _Atomic bool a_bLock;                         // état du verrou
#endif
} t_osCriticalCtx;


//////////////////////////////////
/// @brief osCriticalCreate
/// @param p_pttOSCritical pointer to the critical section
/// @return OS_CRITICAL_SUCCESS if success, OS_CRITICAL_ERROR otherwise
/// @note create the critical section
/// @note timeout is set to 0
//////////////////////////////////
int osCriticalCreate(t_osCriticalCtx* p_pttOSCritical);


//////////////////////////////////
/// @brief osCriticalLock
/// @param p_pttOSCritical pointer to the critical section
/// @return OS_CRITICAL_SUCCESS if success, OS_CRITICAL_ERROR otherwise
/// @note lock the critical section
/////////////////////////////////
int osCriticalLock(t_osCriticalCtx* p_pttOSCritical);

//////////////////////////////////
/// @brief osCriticalUnlock
/// @param p_pttOSCritical pointer to the critical section
/// @return OS_CRITICAL_SUCCESS if success, OS_CRITICAL_ERROR otherwise
/// @note unlock the critical section
//////////////////////////////////
int osCriticalUnlock(t_osCriticalCtx* p_pttOSCritical);

//////////////////////////////////
/// @brief osCriticalDestroy
/// @param p_pttOSCritical pointer to the critical section
/// @return OS_CRITICAL_SUCCESS if success, OS_CRITICAL_ERROR otherwise
/// @note destroy the critical section
//////////////////////////////////
int osCriticalDestroy(t_osCriticalCtx* p_pttOSCritical);


#endif // OS_CRITICAL_H_