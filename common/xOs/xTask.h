////////////////////////////////////////////////////////////
// os Task header file 
// defines the os function for task manipulation
//
// general discloser: copy or share the file is forbidden
// Written : 14/11/2024
// Modified: 28/05/2025 - Security improvements
// Intellectual property of Christophe Benedetti
////////////////////////////////////////////////////////////

#pragma once

#ifndef OS_TASK_H_
#define OS_TASK_H_

// Enable secure functions (C11 Annex K)
#define __STDC_WANT_LIB_EXT1__ 1

#include <pthread.h>
#include <sched.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdatomic.h>
#include <ulimit.h>
#include "xAssert.h"
#include "xOsMemory.h"

// Return codes 
#define OS_TASK_SUCCESS 0x2F41A50

#define OS_TASK_ERROR_INVALID_PARAM     0x2F41A53
#define OS_TASK_ERROR_INIT_FAILED       0x2F41A54
#define OS_TASK_ERROR_CREATE_FAILED     0x2F41A55
#define OS_TASK_ERROR_ALREADY_RUNNING   0x2F41A56
#define OS_TASK_ERROR_NOT_RUNNING       0x2F41A57
#define OS_TASK_ERROR_TERMINATE_FAILED  0x2F41A58
#define OS_TASK_ERROR_JOIN_FAILED       0x2F41A59
#define OS_TASK_ERROR_TIMEOUT           0x2F41A5A
#define OS_TASK_ERROR_PRIORITY          0x2F41A5B
#define OS_TASK_ERROR_STACK_SIZE        0x2F41A5C
#define OS_TASK_ERROR_POLICY            0x2F41A5D

// Task states 
#define OS_TASK_STATUS_READY 0UL
#define OS_TASK_STATUS_RUNNING 1UL
#define OS_TASK_STATUS_BLOCKED 2UL
#define OS_TASK_STATUS_SUSPENDED 3UL
#define OS_TASK_STATUS_TERMINATED 4UL

// Task exit codes 
#define OS_TASK_EXIT_SUCCESS 0x2F41A60
#define OS_TASK_EXIT_FAILURE 0x2F41A61

// Task stop codes 
#define OS_TASK_STOP_REQUEST 0x2F41A00 // Stop request
#define OS_TASK_SECURE_FLAG 0x2F41A01  // Security flag for stopping
#define OS_TASK_STOP_TIMEOUT 5UL       // Timeout in seconds for graceful shutdown

// Secure memory clearing macro 
#define OS_TASK_SANITIZE_MEMORY(ptr, size) do { \
    volatile unsigned char *volatile_ptr = (volatile unsigned char*)(ptr); \
    for (size_t i = 0; i < (size); i++) { \
        volatile_ptr[i] = 0; \
    } \
} while(0)

// Fixed upper bounds for validation
#define OS_TASK_MAX_TIMEOUT_SEC 3600  // Maximum timeout: 1 hour
#define OS_TASK_MIN_STACK_SIZE PTHREAD_STACK_MIN
#define OS_TASK_MAX_STACK_SIZE (64 * 1024 * 1024)  // 64MB max
#define OS_TASK_MAX_STOP_ITERATIONS 60  // Maximum loop iterations for stop (1 minute)
#define OS_TASK_SLEEP_INTERVAL_MS 100  // Sleep interval between checks in milliseconds

typedef enum
{
    OS_SCHED_NORMAL = 0, // SCHED_OTHER - Standard scheduling
    OS_SCHED_FIFO,       // SCHED_FIFO - Real-time FIFO
    OS_SCHED_RR,         // SCHED_RR - Real-time Round Robin
    OS_SCHED_BATCH,      // SCHED_BATCH - Batch processing
    OS_SCHED_IDLE        // SCHED_IDLE - Very low priority
} t_SchedPolicy;

#ifdef OS_USE_RT_SCHEDULING
#define OS_DEFAULT_SCHED_POLICY OS_SCHED_FIFO
// For Linux with real-time policy
#define OS_TASK_LOWEST_PRIORITY 1   // Minimum for SCHED_FIFO/RR
#define OS_TASK_HIGHEST_PRIORITY 99 // Maximum for SCHED_FIFO/RR
#define OS_TASK_DEFAULT_PRIORITY 50 // Median value
#else
#define OS_DEFAULT_SCHED_POLICY OS_SCHED_NORMAL
// For normal scheduling (nice values)
#define OS_TASK_LOWEST_PRIORITY 19   // Lowest priority (nice +19)
#define OS_TASK_HIGHEST_PRIORITY -20 // Highest priority (nice -20)
#define OS_TASK_DEFAULT_PRIORITY 0   // Normal priority (nice 0)
#endif

// Default stack size set as minimum stack size of the system
#define OS_TASK_DEFAULT_STACK_SIZE PTHREAD_STACK_MIN

//////////////////////////////////
// Task management structure 
//////////////////////////////////
typedef struct xos_task_ctx_t
{
    void *(*t_ptTask)(void *); // Pointer to the task function
    void *t_ptTaskArg;         // Task arguments
    int t_iPriority;           // Task priority (for RT, defined via SCHED_RR; for normal, this value is fixed)
    size_t t_ulStackSize;      // Stack size
    int t_iId;                 // Task ID (internal attribute, do not modify)
    int t_iState;              // Task status
    int t_iExitCode;           // Task exit code
    pthread_t t_tHandle;       // pthread thread handle
    atomic_int a_iStopFlag;    // Stop request flag for graceful shutdown
#ifdef OS_USE_RT_SCHEDULING
    struct sched_param t_sched_param; // Scheduling parameters
    t_SchedPolicy t_policy;           // Scheduling policy
#endif
} xOsTaskCtx;

//////////////////////////////////
/// @brief Initialise a task context with default values 
/// @param p_pttOSTask : pointer to the task structure context
/// @return OS_TASK_SUCCESS if success, error code otherwise
/// @note Initializes structure with secure defaults
/// @pre p_pttOSTask must be valid pointer
//////////////////////////////////
int osTaskInit(xOsTaskCtx *p_pttOSTask);

//////////////////////////////////
/// @brief Create a task 
/// @param p_pttOSTask : pointer to the task structure context
/// @return OS_TASK_SUCCESS if success, error code otherwise
/// @note create a task with the given parameters
/// @note the task is created with the default stack size
/// @pre configuration of the context structure
/// @pre osTaskInit must have been called
//////////////////////////////////
int osTaskCreate(xOsTaskCtx *p_pttOSTask);

//////////////////////////////////
/// @brief End a task (unsafe method) 
/// @param p_pttOSTask : pointer to the task structure context
/// @return OS_TASK_SUCCESS if success, error code otherwise
/// @deprecated Use osTaskStop() instead
/// @note This function performs forced termination
//////////////////////////////////
int osTaskEnd(xOsTaskCtx *p_pttOSTask);

//////////////////////////////////
/// @brief Safely stop a task 
/// @param p_pttOSTask : pointer to the task structure context
/// @param p_iTimeout : timeout in seconds to wait for graceful termination (0 = no timeout)
/// @return OS_TASK_SUCCESS if success, error code otherwise
/// @note The task should periodically check the a_iStopFlag in its execution loop
/// @note Timeout value is bounded by OS_TASK_MAX_TIMEOUT_SEC
//////////////////////////////////
int osTaskStop(xOsTaskCtx *p_pttOSTask, int p_iTimeout);

//////////////////////////////////
/// @brief Get the task state 
/// @param p_pttOSTask : pointer to the task structure context
/// @return Task state or error code
/// @note Thread-safe status check
//////////////////////////////////
int osTaskGetStatus(xOsTaskCtx *p_pttOSTask);

//////////////////////////////////
/// @brief Wait for a task to complete 
/// @param p_pttOSTask : pointer to the task structure context
/// @param p_pvExitValue : pointer to store the exit value (can be NULL)
/// @return OS_TASK_SUCCESS if success, error code otherwise
/// @note Blocks until task completion
//////////////////////////////////
int osTaskWait(xOsTaskCtx *p_pttOSTask, void **p_pvExitValue);

//////////////////////////////////
/// @brief Get task exit code 
/// @param p_pttOSTask : pointer to the task structure context
/// @return OS_TASK_SUCCESS if success, error code otherwise
/// @deprecated This function will be deleted, don't use it
//////////////////////////////////
int osTaskGetExitCode(xOsTaskCtx *p_pttOSTask);

//////////////////////////////////
/// @brief Convert error code to text message 
/// @param p_iErrorCode : error code to convert
/// @return Character string describing the error
/// @note Returns const string, no memory management needed
//////////////////////////////////
const char *osTaskGetErrorString(int p_iErrorCode);

#endif // OS_TASK_H_
