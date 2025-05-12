////////////////////////////////////////////////////////////
//  mutex header file
//  defines the mutex structure and related functions
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////
#pragma once

#ifndef XOS_MUTEX_H_
#define XOS_MUTEX_H_

#include <pthread.h>
#include <errno.h>
#include <time.h>

// Mutex error codes
#define MUTEX_OK 0x3B59E20
#define MUTEX_ERROR 0x3B59E21
#define MUTEX_TIMEOUT 0x3B59E22
#define MUTEX_INVALID 0x3B59E23
#define MUTEX_ALREADY_INIT 0x3B59E24
#define MUTEX_NOT_INIT 0x3B59E25

// Mutex states
#define MUTEX_UNLOCKED 0
#define MUTEX_LOCKED 1

// Default timeout value (ms)
#define MUTEX_DEFAULT_TIMEOUT 1000

//////////////////////////////////
/// @brief mutex structure
/// @param t_mutex : mutex handle
/// @param t_iState : mutex state
/// @param t_ulTimeout : timeout value in milliseconds
//////////////////////////////////
typedef struct xos_mutex_ctx_t
{
    pthread_mutex_t t_mutex;
    int t_iState;
    unsigned long t_ulTimeout;
} xOsMutexCtx;

//////////////////////////////////
/// @brief create mutex
/// @param p_ptMutex : mutex structure pointer
/// @return : success or error code
//////////////////////////////////
int mutexCreate(xOsMutexCtx *p_ptMutex);

//////////////////////////////////
/// @brief lock mutex
/// @param p_ptMutex : mutex structure pointer
/// @return : success or error code
//////////////////////////////////
int mutexLock(xOsMutexCtx *p_ptMutex);

//////////////////////////////////
/// @brief try to lock mutex
/// @param p_ptMutex : mutex structure pointer
/// @return : success or error code
//////////////////////////////////
int mutexTryLock(xOsMutexCtx *p_ptMutex);

//////////////////////////////////
/// @brief lock mutex with timeout
/// @param p_ptMutex : mutex structure pointer
/// @param p_ulTimeout : timeout value in milliseconds
/// @return : success or error code
//////////////////////////////////
int mutexLockTimeout(xOsMutexCtx *p_ptMutex, unsigned long p_ulTimeout);

//////////////////////////////////
/// @brief unlock mutex
/// @param p_ptMutex : mutex structure pointer
/// @return : success or error code
//////////////////////////////////
int mutexUnlock(xOsMutexCtx *p_ptMutex);

//////////////////////////////////
/// @brief destroy mutex
/// @param p_ptMutex : mutex structure pointer
/// @return : success or error code
//////////////////////////////////
int mutexDestroy(xOsMutexCtx *p_ptMutex);

//////////////////////////////////
/// @brief set mutex timeout
/// @param p_ptMutex : mutex structure pointer
/// @param p_ulTimeout : timeout value in milliseconds
/// @return : success or error code
//////////////////////////////////
int mutexSetTimeout(xOsMutexCtx *p_ptMutex, unsigned long p_ulTimeout);

//////////////////////////////////
/// @brief get mutex state
/// @param p_ptMutex : mutex structure pointer
/// @return : mutex state or error code
//////////////////////////////////
int mutexGetState(xOsMutexCtx *p_ptMutex);

#endif // XOS_MUTEX_H_
