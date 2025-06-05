////////////////////////////////////////////////////////////
//  mutex header file 
//  defines the mutex structure and related functions
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
// Modified: 28/05/2025 - Security improvements
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////

#pragma once

#ifndef XOS_MUTEX_H_
#define XOS_MUTEX_H_

#include <pthread.h>
#include <errno.h>
#include <time.h>
#include <stdatomic.h>
#include "xOsMemory.h"
#include "xError.h"

// Mutex states - atomic safe values
#define MUTEX_UNLOCKED  0
#define MUTEX_LOCKED    1

// Default timeout value (ms)
#define MUTEX_DEFAULT_TIMEOUT 1000

// Fixed upper bounds for validation
#define MUTEX_MAX_TIMEOUT_MS 3600000  // Maximum timeout: 1 hour in milliseconds
#define MUTEX_MIN_TIMEOUT_MS 0        // Minimum timeout: no wait
#define MUTEX_MAX_LOCK_ITERATIONS 3600000  // Maximum loop iterations for lock with timeout
#define MUTEX_SLEEP_INTERVAL_NS 1000000    // Sleep interval between checks in nanoseconds (1ms)

//////////////////////////////////
/// @brief mutex structure with atomic state
/// @param t_mutex : mutex handle
/// @param a_iState : atomic mutex state (thread-safe)
/// @param a_ulTimeout : atomic timeout value in milliseconds (thread-safe)
//////////////////////////////////
typedef struct xos_mutex_ctx_t
{
    pthread_mutex_t t_mutex;
    atomic_int a_iState;           // Atomic state to prevent race conditions
    atomic_ulong a_ulTimeout;      // Atomic timeout to prevent race conditions
} xOsMutexCtx;

//////////////////////////////////
/// @brief create mutex
/// @param p_ptMutex : mutex structure pointer
/// @return : success or error code
/// @note Initializes mutex with recursive attributes
/// @pre p_ptMutex must be valid pointer
//////////////////////////////////
int mutexCreate(xOsMutexCtx *p_ptMutex);

//////////////////////////////////
/// @brief lock mutex
/// @param p_ptMutex : mutex structure pointer
/// @return : success or error code
/// @note Blocks until mutex is available
//////////////////////////////////
int mutexLock(xOsMutexCtx *p_ptMutex);

//////////////////////////////////
/// @brief try to lock mutex
/// @param p_ptMutex : mutex structure pointer
/// @return : success or error code
/// @note Non-blocking lock attempt
//////////////////////////////////
int mutexTryLock(xOsMutexCtx *p_ptMutex);

//////////////////////////////////
/// @brief lock mutex with timeout
/// @param p_ptMutex : mutex structure pointer
/// @param p_ulTimeout : timeout value in milliseconds
/// @return : success or error code
/// @note Timeout value is bounded by MUTEX_MAX_TIMEOUT_MS
//////////////////////////////////
int mutexLockTimeout(xOsMutexCtx *p_ptMutex, unsigned long p_ulTimeout);

//////////////////////////////////
/// @brief unlock mutex
/// @param p_ptMutex : mutex structure pointer
/// @return : success or error code
/// @note Must be called by the locking thread
//////////////////////////////////
int mutexUnlock(xOsMutexCtx *p_ptMutex);

//////////////////////////////////
/// @brief destroy mutex
/// @param p_ptMutex : mutex structure pointer
/// @return : success or error code
/// @note Mutex must be unlocked before destruction
//////////////////////////////////
int mutexDestroy(xOsMutexCtx *p_ptMutex);

//////////////////////////////////
/// @brief set mutex timeout
/// @param p_ptMutex : mutex structure pointer
/// @param p_ulTimeout : timeout value in milliseconds
/// @return : success or error code
/// @note Timeout value is validated against bounds
//////////////////////////////////
int mutexSetTimeout(xOsMutexCtx *p_ptMutex, unsigned long p_ulTimeout);

//////////////////////////////////
/// @brief get mutex state
/// @param p_ptMutex : mutex structure pointer
/// @return : mutex state or error code
/// @note Returns current lock state (thread-safe atomic read)
//////////////////////////////////
int mutexGetState(xOsMutexCtx *p_ptMutex);

//////////////////////////////////
/// @brief get mutex timeout
/// @param p_ptMutex : mutex structure pointer
/// @return : current timeout value in milliseconds
/// @note Returns current timeout (thread-safe atomic read)
//////////////////////////////////
unsigned long mutexGetTimeout(xOsMutexCtx *p_ptMutex);

#endif // XOS_MUTEX_H_
